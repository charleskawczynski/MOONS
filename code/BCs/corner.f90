       module corner_mod
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
       public :: corner
       public :: init,delete
       public :: print,export

       type corner
         type(bctype) :: b
         real(cp) :: val
         logical :: defined = .false.
       end type

       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_corner;       end interface
       interface print;      module procedure print_corner;        end interface
       interface export;     module procedure export_corner;       end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_val(c,val)
         implicit none
         type(corner),intent(inout) :: c
         real(cp),intent(in) :: val
         call init(c%b,val)
         c%val = val
         c%defined = .true.
       end subroutine

       subroutine init_copy(c_out,c_in)
         implicit none
         type(corner),intent(inout) :: c_out
         type(corner),intent(in) :: c_in
         if (.not.c_in%defined) stop 'Error: trying to copy undefined corner in corner.f90'
         call init(c_out%b,c_in%b)
         c_out%val = c_in%val
         c_out%defined = c_out%defined
       end subroutine

       subroutine delete_corner(c)
         implicit none
         type(corner),intent(inout) :: c
         call delete(c%b)
         c%val = 0.0_cp
         c%defined = .false.
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_corner(c)
         implicit none
         type(corner), intent(in) :: c
         call export(c,6)
       end subroutine

       subroutine export_corner(c,newU)
         implicit none
         type(corner), intent(in) :: c
         integer,intent(in) :: NewU
         if (.not.c%defined) stop 'Error: corner not defined in export_corner in corner.f90'
         call export(c%b,newU)
       end subroutine

       end module