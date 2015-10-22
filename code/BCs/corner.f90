       module corner_mod
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
       public :: corner
       public :: init,delete
       public :: print,export

       type corner
         integer :: bctype
         real(cp) :: val
         logical,dimension(2) :: def = .false. ! true if (bctype,vals) are defined
         logical :: defined = .false. ! = all(defined)
       end type

       interface init;       module procedure init_type;             end interface
       interface init;       module procedure init_val;              end interface
       interface init;       module procedure init_copy;             end interface

       interface delete;     module procedure delete_corner;       end interface
       interface print;      module procedure print_corner;        end interface
       interface export;     module procedure export_corner;       end interface
       interface export;     module procedure export_corner_unit;  end interface

       contains

       ! *******************************************************************************
       ! ********************************* INIT/DELETE *********************************
       ! *******************************************************************************

       subroutine init_type(c,bctype)
         implicit none
         type(corner),intent(inout) :: c
         integer,intent(in) :: bctype
         c%bctype = bctype
         c%def(1) = .true.
         c%defined = all(c%def)
       end subroutine

       subroutine init_val(c,val)
         implicit none
         type(corner),intent(inout) :: c
         real(cp),intent(in) :: val
         c%val = val
         c%def(2) = .true.
         c%defined = all(c%def)
       end subroutine

       subroutine init_copy(c_out,c_in)
         implicit none
         type(corner),intent(inout) :: c_out
         type(corner),intent(in) :: c_in
         if (.not.c_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         c_out%bctype = c_in%bctype
         c_out%val = c_in%val
         c_out%def = c_out%def
         c_out%defined = c_out%defined
       end subroutine

       subroutine delete_corner(c)
         implicit none
         type(corner),intent(inout) :: c
         c%val = 0.0_cp
         c%def = .false.
         c%defined = all(c%def)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_corner(c,name)
         implicit none
         type(corner), intent(in) :: c
         character(len=*),intent(in) :: name
         call exp_corner(c,name,6)
       end subroutine

       subroutine export_corner(c,dir,name)
         implicit none
         type(corner), intent(in) :: c
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_corner')
         call exp_corner(c,name,newU)
         call closeAndMessage(newU,name//'_cornerConditions',dir)
       end subroutine

       subroutine export_corner_unit(c,newU,name)
         implicit none
         type(corner), intent(in) :: c
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         call exp_corner(c,name,newU)
       end subroutine

       subroutine exp_corner(c,name,newU)
         implicit none
         type(corner), intent(in) :: c
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         if (.not.c%defined) stop 'Error: corner not defined in writecorner in corner.f90'

         write(newU,*) 'corner conditions for ' // trim(adjustl(name))
         call writecorner(c%bctype,newU)
       end subroutine

       subroutine writecorner(bctype,NewU)
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