       module initializeBfield_mod
       use constants_mod
       use myIO_mod
       use griddata_mod
       use myAllocate_mod
       use simParams_mod
       use applyBCs_mod
       implicit none

       private
       public :: initializeBfield

       contains

       subroutine initializeBfield(Bx,By,Bz,Bx0,By0,Bz0,gd,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         if (restartB) then
           call initializeRestartBfield(Bx,By,Bz,Bx0,By0,Bz0,gd,dir)
         elseif (preDefinedB_ICs.ne.0) then
           call initializePreDefinedBfield(Bx,By,Bz,Bx0,By0,Bz0,gd)
         else
           call initializeUserBfield(Bx,By,Bz,Bx0,By0,Bz0,gd)
         endif
       end subroutine

       subroutine initializeRestartBfield(Bx,By,Bz,Bx0,By0,Bz0,gd,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt

         call myAllocate(Nx,Ny,Nz,gd,totalCC)
         allocate(xct(Nx),yct(Ny),zct(Nz))
         call myAllocate(Nx,Ny,Nz,gd,totalN)
         allocate(xnt(Nx),ynt(Ny),znt(Nz))

         call getXYZcc(gd,xct,yct,zct)
         call getXYZn(gd,xnt,ynt,znt)
         ! Need to write B0 to file then change this to a read statement!
         call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,applied_B_dir)

         select case (BLoc)
         case (dom_cc_tot)
           call readFromFile(xct,yct,zct,Bx,By,Bz,dir//'Bfield/','Bxct','Byct','Bzct')
         case (dom_n_tot)
           call readFromFile(xnt,ynt,znt,Bx,By,Bz,dir//'Bfield/','Bxnt','Bynt','Bznt')
         end select
         ! Compute the induced magnetic field (B_total is exprted)
         Bx = Bx - Bx0; By = By - By0; Bz = Bz - Bz0

         deallocate(xnt,ynt,znt)
         deallocate(xct,yct,zct)
       end subroutine

       subroutine uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,dir)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         integer,intent(in) :: dir
         Bx = zero; By = zero; Bz = zero
         select case (dir)
         case (0); Bx0 = zero; By0 = zero; Bz0 = zero
         case (1); Bx0 = one; By0 = zero; Bz0 = zero
         case (2); Bx0 = zero; By0 = one; Bz0 = zero
         case (3); Bx0 = zero; By0 = zero; Bz0 = one
         end select
       end subroutine

       subroutine initializePreDefinedBfield(Bx,By,Bz,Bx0,By0,Bz0,gd)
         implicit none
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0

         select case (preDefinedB_ICs)
         case (1); call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,applied_B_dir)
         case (2); call initializeFringingField(Bx,By,Bz,Bx0,By0,Bz0,gd)
         case (3); call initializeFringingField(Bx,By,Bz,Bx0,By0,Bz0,gd)
         case default
           write(*,*) 'Incorrect preDefinedB_ICs case in initializeBfield.'; stop
         end select

       end subroutine

       subroutine initializeFringingField(Bx,By,Bz,Bx0,By0,Bz0,gd)
         implicit none
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt
         integer,dimension(3) :: s
         integer :: i2,i
         real(dpn) :: Bstretch,Bshift

         call myAllocate(Nx,Ny,Nz,gd,totalCC)
         allocate(xct(Nx),yct(Ny),zct(Nz))
         call myAllocate(Nx,Ny,Nz,gd,totalN)
         allocate(xnt(Nx),ynt(Ny),znt(Nz))

         call getXYZcc(gd,xct,yct,zct)
         call getXYZn(gd,xnt,ynt,znt)

         s = shape(Bz0)
         Bx = zero; By = zero; Bz = zero
         Bx0 = zero
         By0 = zero
         
         ! Sergey's fringe:
         Bstretch = 1.d0   ! stretching parameter
         Bshift = 10.d0    ! shift parameter

         do i=1,s(1)
           Bz0(i,:,:) = (one+Dtanh((xct(i)-Bshift)/Bstretch))/2.d0
         enddo
         i2 = 0
         do i=1+(s(1)-1)/2,s(1)
           Bz0(i,:,:) = Bz0(1+(s(1)-1)/2-i2,:,:)
           i2 = i2+1
         enddo

         deallocate(xnt,ynt,znt)
         deallocate(xct,yct,zct)
       end subroutine

       subroutine initializeUserBfield(Bx,By,Bz,Bx0,By0,Bz0,gd)
         implicit none
         type(griddata),intent(in) :: gd
         real(dpn),dimension(:,:,:),intent(inout) :: Bx,By,Bz,Bx0,By0,Bz0
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt

         call myAllocate(Nx,Ny,Nz,gd,totalCC)
         allocate(xct(Nx),yct(Ny),zct(Nz))
         call myAllocate(Nx,Ny,Nz,gd,totalN)
         allocate(xnt(Nx),ynt(Ny),znt(Nz))

         call getXYZcc(gd,xct,yct,zct)
         call getXYZn(gd,xnt,ynt,znt)

         call uniformBfield(Bx,By,Bz,Bx0,By0,Bz0,3)

         deallocate(xnt,ynt,znt)
         deallocate(xct,yct,zct)
       end subroutine

       end module
