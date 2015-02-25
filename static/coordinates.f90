       module coordinatesOld_mod
       use constants_mod
       use myExceptions_mod
       implicit none

       private

       public :: allocateCoordinates
       public :: delete

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

       interface delete
         module procedure deallocateCoordinates1D
         module procedure deallocateCoordinates2D
         module procedure deallocateCoordinates3D
       end interface

       interface allocateCoordinates
         module procedure allocateCoordinates1D
         module procedure allocateCoordinates2D
         module procedure allocateCoordinates3D
       end interface

       contains

! ----------------------------------------------------------------- allocate/deallocate

       subroutine deallocateCoordinates1D(this)
         implicit none
         type(coordinates1D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
       end subroutine

       subroutine deallocateCoordinates2D(this)
         implicit none
         type(coordinates2D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
         if (allocated(this%y)) deallocate(this%y)
       end subroutine

       subroutine deallocateCoordinates3D(this)
         implicit none
         type(coordinates3D),intent(inout) :: this
         if (allocated(this%x)) deallocate(this%x)
         if (allocated(this%y)) deallocate(this%y)
         if (allocated(this%z)) deallocate(this%z)
       end subroutine

       subroutine allocateCoordinates1D(this,Nx)
         implicit none
         type(coordinates1D),intent(inout) :: this
         integer,intent(in) :: Nx
         allocate(this%x(Nx))
       end subroutine

       subroutine allocateCoordinates2D(this,Nx,Ny)
         implicit none
         type(coordinates2D),intent(inout) :: this
         integer,intent(in) :: Nx,Ny
         allocate(this%x(Nx))
         allocate(this%y(Ny))
       end subroutine

       subroutine allocateCoordinates3D(this,Nx,Ny,Nz)
         implicit none
         type(coordinates3D),intent(inout) :: this
         integer,intent(in) :: Nx,Ny,Nz
         allocate(this%x(Nx))
         allocate(this%y(Ny))
         allocate(this%z(Nz))
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