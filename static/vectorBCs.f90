       module vectorBCs_mod

       use BCs_mod
       use grid_mod
       use IO_tools_mod
       implicit none

       private

       public :: BCs
       public :: init,delete,setGrid

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
       public :: printVectorBCs
       public :: writeVectorBCs

       type vectorBCs
         type(BCs) :: x,y,z
       end type

       interface init;       module procedure initVectorBCsCopy;       end interface
       interface delete;     module procedure deleteVectorBCs;         end interface

       contains

       subroutine initVectorBCsCopy(this,bx,by,bz)
         implicit none
         type(vectorBCs),intent(inout) :: this
         type(BCs),intent(inout) :: bx,by,bz
         call init(this%x,bx)
         call init(this%y,by)
         call init(this%z,bz)
       end subroutine

       subroutine deleteVectorBCs(this)
         implicit none
         type(vectorBCs),intent(inout) :: this
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
       end subroutine

       subroutine printVectorBCs(this,namex,namey,namez)
         implicit none
         type(vectorBCs),intent(in) :: this
         character(len=*),intent(in) :: namex,namey,namez
         call printAllBoundaries(this%x,namex)
         call printAllBoundaries(this%y,namey)
         call printAllBoundaries(this%z,namez)
       end subroutine

       subroutine writeVectorBCs(this,dir,namex,namey,namez)
         implicit none
         type(vectorBCs),intent(in) :: this
         character(len=*),intent(in) :: dir,namex,namey,namez
         call writeAllBoundaries(this%x,dir,namex)
         call writeAllBoundaries(this%y,dir,namey)
         call writeAllBoundaries(this%z,dir,namez)
       end subroutine

       end module