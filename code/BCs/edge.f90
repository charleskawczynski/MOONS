       module edge_mod
       ! Edges are organized as follows
       !        minmin(i) {(y,z),(x,z),(x,y)}
       !        minmax(i) {(y,z),(x,z),(x,y)}
       !        maxmin(i) {(y,z),(x,z),(x,y)}
       !        maxmax(i) {(y,z),(x,z),(x,y)}
       ! for direction i, covering all 12 edge.
       use IO_tools_mod
       use bctype_mod
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
         type(bctype) :: b
         real(cp),dimension(:),allocatable :: vals
         integer :: s
         logical,dimension(2) :: def = .false. ! (size,vals)
         logical :: defined = .false. ! = all(e%def)
       end type

       interface init;       module procedure init_size;             end interface
       interface init;       module procedure init_vals_RF;          end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_edge;       end interface
       interface print;      module procedure print_edge;        end interface
       interface export;     module procedure export_edge;       end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_size(e,s)
         implicit none
         type(edge),intent(inout) :: e
         integer,intent(in) :: s
         if (s.lt.1) stop 'Error: edge size input less than 1 in edge.f90'
         e%s = s
         e%def(1) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_vals_RF(e,vals)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),dimension(:),intent(in) :: vals
         integer :: s
         s = size(vals)
         if (s.lt.1) stop 'Error: edge size input less than 1 in edge.f90'
         if (s.ne.e%s) stop 'Error: shape mis-match in init_vals_RF in edge.f90'
         call init(e%b,vals)
         e%vals = vals
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_val(e,val)
         implicit none
         type(edge),intent(inout) :: e
         real(cp),intent(in) :: val
         integer :: s
         s = size(e%vals)
         if (s.lt.1) stop 'Error: edge size less than 1 in edge.f90'
         call init(e%b,val)
         e%vals = val
         e%def(2) = .true.
         e%defined = all(e%def)
       end subroutine

       subroutine init_copy(e_out,e_in)
         implicit none
         type(edge),intent(inout) :: e_out
         type(edge),intent(in) :: e_in
         if (.not.e_in%defined) stop 'Error: trying to copy undefined edge in edge.f90'
         call init(e_out%b,e_in%b)
         e_out%vals = e_out%vals
         e_out%def = e_out%def
         e_out%defined = e_out%defined
         e_out%s = e_in%s
       end subroutine

       subroutine delete_edge(e)
         implicit none
         type(edge),intent(inout) :: e
         if (allocated(e%vals)) deallocate(e%vals)
         call delete(e%b)
         e%s = 0
         e%def = .false.
         e%defined = .false.
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_edge(e)
         implicit none
         type(edge), intent(in) :: e
         call export(e,6)
       end subroutine

       subroutine export_edge(e,newU)
         implicit none
         type(edge), intent(in) :: e
         integer,intent(in) :: NewU
         if (.not.e%defined) stop 'Error: edge not defined in export_edge in edge.f90'
         call export(e%b,newU)
       end subroutine

       end module