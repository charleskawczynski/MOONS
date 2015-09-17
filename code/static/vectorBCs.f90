       module vectorBCs_mod
       use BCs_mod
       use grid_mod
       use IO_tools_mod
       implicit none

       private


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: vectorBCs
       public :: init,delete
       public :: print,export

       type vectorBCs
         type(BCs) :: x,y,z
       end type

       interface init;      module procedure init_VBCsCopy;       end interface
       interface init;      module procedure init_VBCsGrid;       end interface
       interface delete;    module procedure delete_VBCs;         end interface
       interface print;     module procedure print_VBCs;          end interface
       interface export;    module procedure export_VBCs;         end interface

       contains

       subroutine init_VBCsCopy(VBC,bx,by,bz)
         implicit none
         type(vectorBCs),intent(inout) :: VBC
         type(BCs),intent(inout) :: bx,by,bz
         call init(VBC%x,bx)
         call init(VBC%y,by)
         call init(VBC%z,bz)
       end subroutine

       subroutine delete_VBCs(VBC)
         implicit none
         type(vectorBCs),intent(inout) :: VBC
         call delete(VBC%x)
         call delete(VBC%y)
         call delete(VBC%z)
       end subroutine

       subroutine init_VBCsGrid(VBC,g,s1,s2,s3)
         implicit none
         type(vectorBCs),intent(inout) :: VBC
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: s1,s2,s3
         call init(VBC%x,g,s1)
         call init(VBC%y,g,s2)
         call init(VBC%z,g,s3)
       end subroutine

       subroutine print_VBCs(VBC,name)
         implicit none
         type(vectorBCs),intent(in) :: VBC
         character(len=*),intent(in) :: name
         call print(VBC%x,name//'x')
         call print(VBC%y,name//'y')
         call print(VBC%z,name//'z')
       end subroutine

       subroutine export_VBCs(VBC,dir,name)
         implicit none
         type(vectorBCs),intent(in) :: VBC
         character(len=*),intent(in) :: dir,name
         call export(VBC%x,dir,name//'x')
         call export(VBC%y,dir,name//'y')
         call export(VBC%z,dir,name//'z')
       end subroutine

       end module