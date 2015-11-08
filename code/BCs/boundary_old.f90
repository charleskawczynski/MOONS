       module boundary_mod
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
       public :: boundary
       public :: init,delete
       public :: print,export

       type boundary
         integer :: bctype
         real(cp),dimension(:,:),allocatable :: vals
         real(cp) :: val
         integer,dimension(2) :: s
         logical,dimension(2) :: def = .false. ! true if (bctype,vals) are defined
         logical :: defined = .false. ! = all(defined)
       end type

       interface init;       module procedure init_type;             end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_shape;            end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_boundary;       end interface
       interface print;      module procedure print_boundary;        end interface
       interface export;     module procedure export_boundary;       end interface
       interface export;     module procedure export_boundary_unit;  end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_type(b,bctype)
         implicit none
         type(boundary),intent(inout) :: b
         integer,intent(in) :: bctype
         b%bctype = bctype
         b%def(1) = .true.
         b%defined = all(b%def)
       end subroutine

       subroutine init_vals_RF(b,vals)
         implicit none
         type(boundary),intent(inout) :: b
         real(cp),dimension(:,:),intent(in) :: vals
         if (allocated(b%vals)) deallocate(b%vals)
         ! b%s = shape(vals) ! Is this necessary/good?
         ! Make sure that b%s has been defined here in debug mode
         allocate(b%vals(b%s(1),b%s(2)))
         b%val = vals(1,1)
         b%vals = vals
         b%def(2) = .true.
         b%defined = all(b%def)
       end subroutine

       subroutine init_shape(b,s)
         implicit none
         type(boundary),intent(inout) :: b
         integer,dimension(2),intent(in) :: s
         b%s = s
       end subroutine

       subroutine init_val(b,val)
         implicit none
         type(boundary),intent(inout) :: b
         real(cp),intent(in) :: val
         if (allocated(b%vals)) deallocate(b%vals)
         allocate(b%vals(b%s(1),b%s(2)))
         b%vals = val
         b%val = val
         b%def(2) = .true.
         b%defined = all(b%def)
       end subroutine

       subroutine init_copy(b_out,b_in)
         implicit none
         type(boundary),intent(inout) :: b_out
         type(boundary),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         if (allocated(b_in%vals)) then
           call init(b_out,b_in%vals)
         else; stop 'Error: trying to copy BC that has not been allocated vals'
         endif
         b_out%bctype = b_in%bctype
         b_out%val = b_in%val
         b_out%def = b_in%def
         b_out%defined = b_in%defined
         b_out%s = b_in%s
       end subroutine

       subroutine delete_boundary(b)
         implicit none
         type(boundary),intent(inout) :: b
         if (allocated(b%vals)) deallocate(b%vals)
         b%s = 0
         b%def = .false.
         b%defined = all(b%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_boundary(b,name)
         implicit none
         type(boundary), intent(in) :: b
         character(len=*),intent(in) :: name
         call exp_boundary(b,name,6)
         write(*,*) 'val = ',b%val
       end subroutine

       subroutine export_boundary(b,dir,name)
         implicit none
         type(boundary), intent(in) :: b
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_boundary')
         call exp_boundary(b,name,newU)
         call closeAndMessage(newU,name//'_BoundaryConditions',dir)
       end subroutine

       subroutine export_boundary_unit(b,newU,name)
         implicit none
         type(boundary), intent(in) :: b
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         call exp_boundary(b,name,newU)
       end subroutine

       subroutine exp_boundary(b,name,newU)
         implicit none
         type(boundary), intent(in) :: b
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         if (.not.b%defined) stop 'Error: boundary not defined in writeBoundary in boundary.f90'

         write(newU,*) 'Boundary conditions for ' // trim(adjustl(name))
         call writeBoundary(b%bctype,newU)
       end subroutine

       subroutine writeBoundary(bctype,NewU)
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