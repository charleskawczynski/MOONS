       module edge_mod
       ! Edges are organized as follows
       !        minmin(i) {(y,z),(x,z),(x,y)}
       !        minmax(i) {(y,z),(x,z),(x,y)}
       !        maxmin(i) {(y,z),(x,z),(x,y)}
       !        maxmax(i) {(y,z),(x,z),(x,y)}
       ! for direction i, covering all 12 edge.
       use IO_tools_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: edge
       public :: init,delete
       public :: print,export

       type edge
         integer :: bctype
         real(cp),dimension(:),allocatable :: vals
         real(cp) :: val
         integer :: s
         logical,dimension(2) :: def = .false. ! true if (bctype,vals) are defined
         logical :: defined = .false. ! = all(defined)
       end type

       interface init;       module procedure init_type;             end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_edge;       end interface
       interface print;      module procedure print_edge;        end interface
       interface export;     module procedure export_edge;       end interface
       interface export;     module procedure export_edge_unit;  end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_type(e,bctype)
         implicit none
         type(edge),intent(inout) :: e
         integer,intent(in) :: bctype
         e%bctype = bctype
         e%def(1) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_vals_RF(e,vals,s)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),dimension(:),intent(in) :: vals
         integer,intent(in) :: s
         e%s = s
         if (allocated(e%vals)) deallocate(e%vals)
         ! e%s = shape(vals) ! Is this necessary/good?
         ! Make sure that e%s has been defined here in debug mode
         allocate(e%vals(e%s))
         e%val = vals(1)
         e%vals = vals
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_val(e,val)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),intent(in) :: val
         if (allocated(e%vals)) deallocate(e%vals)
         allocate(e%vals(e%s))
         e%vals = val
         e%val = val
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_copy(e_out,e_in)
         implicit none
         type(edge),intent(inout) :: e_out
         type(edge),intent(in) :: e_in
         if (.not.e_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         if (allocated(e_in%vals)) then
           call init(e_out,e_in%vals,e_in%s)
         else; stop 'Error: trying to copy BC that has not been allocated vals'
         endif
         e_out%bctype = e_in%bctype
         e_out%val = e_in%val
         e_out%def = e_out%def
         e_out%defined = e_out%defined
         e_out%s = e_in%s
       end subroutine

       subroutine delete_edge(e)
         implicit none
         type(edge),intent(inout) :: e
         if (allocated(e%vals)) deallocate(e%vals)
         e%s = 0
         e%def = .false.
         e%defined = all(e%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_edge(e,name)
         implicit none
         type(edge), intent(in) :: e
         character(len=*),intent(in) :: name
         call exp_edge(e,name,6)
       end subroutine

       subroutine export_edge(e,dir,name)
         implicit none
         type(edge), intent(in) :: e
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_edge')
         call exp_edge(e,name,newU)
         call closeAndMessage(newU,name//'_edgeConditions',dir)
       end subroutine

       subroutine export_edge_unit(e,newU,name)
         implicit none
         type(edge), intent(in) :: e
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         call exp_edge(e,name,newU)
       end subroutine

       subroutine exp_edge(e,name,newU)
         implicit none
         type(edge), intent(in) :: e
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         if (.not.e%defined) stop 'Error: edge not defined in writeedge in edge.f90'

         write(newU,*) 'edge conditions for ' // trim(adjustl(name))
         call writeedge(e%bctype,newU)
       end subroutine

       subroutine writeEdge(bctype,NewU)
         implicit none
         integer,intent(in) :: NewU,bctype
         if (bctype.eq.1) then; write(newU,*) 'Dirichlet - direct - wall coincident'; endif
         if (bctype.eq.2) then; write(newU,*) 'Dirichlet - interpolated - wall incoincident'; endif
         if (bctype.eq.3) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh^2)'; endif
         if (bctype.eq.4) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.5) then; write(newU,*) 'Neumann - interpolated - wall incoincident O(dh)'; endif
         if (bctype.eq.6) then; write(newU,*) 'Periodic - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.7) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh)'; endif
         if (bctype.eq.8) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh^2)'; endif
       end subroutine

       end module