       module coordinates_mod
       use constants_mod
       use myExceptions_mod
       implicit none
       ! Implementation:
       ! type(coordinates3D) :: xyz
       ! real(dpn),dimension(Nx) :: x
       ! call nullifyCoordinates(xyz)
       ! call setX(v3,x)
       ! 
       ! Improvements / fixes:
       ! 
       ! - After that, make sure that deleteCoordinates is NEVER called 
       !   unless the Coordinates is allocated (and therefore associated).
       !   Also, make sure that, e.g. when Del is called, the other 
       !   components can be tested / handled correctly.

       private

       public :: deleteCoordinates

       public :: coordinates1D,coordinates2D,coordinates3D
       public :: setX,setY,setZ
       public :: getX,getY,getZ

       type coordinates1D
         private
         integer :: sx
         real(dpn),dimension(:),allocatable :: x
       end type

       type coordinates2D
         private
         integer :: sx,sy
         real(dpn),dimension(:),allocatable :: x
         real(dpn),dimension(:),allocatable :: y
       end type

       type coordinates3D
         private
         integer :: sx,sy,sz
         real(dpn),dimension(:),allocatable :: x
         real(dpn),dimension(:),allocatable :: y
         real(dpn),dimension(:),allocatable :: z
       end type

       interface setX
         module procedure setCoordinatesX1D
         module procedure setCoordinatesX2D
         module procedure setCoordinatesX3D
       end interface

       interface setY
         module procedure setCoordinatesY2D
         module procedure setCoordinatesY3D
       end interface

       interface setZ
         module procedure setCoordinatesZ3D
       end interface

       interface getX
         module procedure getCoordinatesX1D
         module procedure getCoordinatesX2D
         module procedure getCoordinatesX3D
       end interface

       interface getY
         module procedure getCoordinatesY2D
         module procedure getCoordinatesY3D
       end interface

       interface getZ
         module procedure getCoordinatesZ3D
       end interface

       interface deleteCoordinates
         module procedure deleteCoordinates1D
         module procedure deleteCoordinates2D
         module procedure deleteCoordinates3D
       end interface

       contains

! ----------------------------------------------------------------- nullify/delete

       subroutine deleteCoordinates1D(this)
         implicit none
         type(coordinates1D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
       end subroutine

       subroutine deleteCoordinates2D(this)
         implicit none
         type(coordinates2D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
         if (allocated(this%y)) deallocate(this%y)
       end subroutine

       subroutine deleteCoordinates3D(this)
         implicit none
         type(coordinates3D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
         if (allocated(this%y)) deallocate(this%y)
         if (allocated(this%z)) deallocate(this%z)
       end subroutine
! ----------------------------------------------------------------- delete/set

       subroutine setCoordinatesX1D(this,x)
         implicit none
         type(coordinates1D),intent(inout) :: this
         real(dpn),dimension(:),intent(in) :: x
         allocate(this%x(size(x))); this%x = x
       end subroutine

       subroutine setCoordinatesX2D(this,x)
         implicit none
         type(coordinates2D),intent(inout) :: this
         real(dpn),dimension(:),intent(in),target :: x
         allocate(this%x(size(x))); this%x = x
       end subroutine

       subroutine setCoordinatesY2D(this,y)
         implicit none
         type(coordinates2D),intent(inout) :: this
         real(dpn),dimension(:),intent(in),target :: y
         allocate(this%y(size(y))); this%y = y
       end subroutine

       subroutine setCoordinatesX3D(this,x)
         implicit none
         type(coordinates3D),intent(inout) :: this
         real(dpn),dimension(:),intent(in),target :: x
         allocate(this%x(size(x))); this%x = x
       end subroutine

       subroutine setCoordinatesY3D(this,y)
         implicit none
         type(coordinates3D),intent(inout) :: this
         real(dpn),dimension(:),intent(in),target :: y
         allocate(this%y(size(y))); this%y = y
       end subroutine

       subroutine setCoordinatesZ3D(this,z)
         implicit none
         type(coordinates3D),intent(inout) :: this
         real(dpn),dimension(:),intent(in),target :: z
         allocate(this%z(size(z))); this%z = z
       end subroutine

! ****************************************************************************************SET/GET

       subroutine getCoordinatesX1D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates1D),intent(in) :: this
         if (allocated(this%x)) then
           res = this%x
         else; call propertyNotAssigned('x','getCoordinatesX2D')
         endif
       end subroutine

       subroutine getCoordinatesX2D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates2D),intent(in) :: this
         if (allocated(this%x)) then
           res = this%x
         else; call propertyNotAssigned('x','getCoordinatesX2D')
         endif
       end subroutine 

       subroutine getCoordinatesY2D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates2D),intent(in) :: this
         if (allocated(this%y)) then
           res = this%y
         else; call propertyNotAssigned('y','getCoordinatesY2D')
         endif
       end subroutine 

       subroutine getCoordinatesX3D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates3D),intent(in) :: this
         if (allocated(this%x)) then
           res = this%x
         else; call propertyNotAssigned('x','getCoordinatesX3D')
         endif
       end subroutine 

       subroutine getCoordinatesY3D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates3D),intent(in) :: this
         if (allocated(this%y)) then
           res = this%y
         else; call propertyNotAssigned('y','getCoordinatesY3D')
         endif
       end subroutine 

       subroutine getCoordinatesZ3D(this,res)
         implicit none
         real(dpn),dimension(:),intent(out) :: res
         type(coordinates3D),intent(in) :: this
         if (allocated(this%z)) then
           res = this%z
         else; call propertyNotAssigned('z','getCoordinatesZ3D')
         endif
       end subroutine 

       end module