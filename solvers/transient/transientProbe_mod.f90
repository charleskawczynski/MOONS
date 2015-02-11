       module transientProbe_mod
       use simParams_mod
       use constants_mod
       use myIO_mod
       use myDebug_mod
       use scalarField_mod
       use vectorField_mod

       use griddata_mod
       use myError_mod

       implicit none

       private
       public :: initialize,delete,solve
       public :: export
       public :: printProbe


       type probe
         character(len=9) :: name = 'probe'
         real(dpn),dimension(:),alloatable :: u ! data
         type(myError) :: e
         real(dpn),dimension(3) :: h_desired    ! desired x,y,z
         real(dpn),dimension(3) :: h            ! actual  x,y,z
         integer,dimension(3) :: i              ! index for x,y,z
         integer,dimension(3) :: s              ! size of data
         integer :: Nsteps                      ! number of steps
         integer :: component                   ! component of field (for staggered)
         logical :: errorProbe                  ! T/F
       end type

       interface initialize
         module procedure initializeProbe
       end interface

       interface check
         module procedure checkProbe
       end interface

       interface delete
         module procedure deleteProbe
       end interface

       interface apply
         module procedure applyTransientProbe
         module procedure applyTransientError
       end interface

       interface export
         module procedure exportProbe
       end interface

       contains

        subroutine initializeProbe(p,U,x,y,z,N,component)
          implicit none
          type(probe),intent(inout) :: p
          real(dpn),dimension(:,:,:),intent(in) :: U
          real(dpn),intent(in) :: x,y,z
          integer,intent(in) :: N
          p%h_desired(1) = x; p%h_desired(2) = y; p%h_desired(3) = z
          p%s = shape(U)
          p%Nsteps = N
          p%component = component
          p%j = 1
          p%errorProbe = .false.
        end subroutine

        subroutine initializeErrorProbe(p,U,N)
          implicit none
          type(probe),intent(inout) :: p
          real(dpn),dimension(:,:,:),intent(in) :: U
          integer,intent(in) :: N
          p%s = shape(U)
          p%Nsteps = N
          p%j = 1
          p%errorProbe = .true.
        end subroutine

        subroutine checkProbe(p,gd)
          implicit none
          type(probe),intent(in) :: p
          type(griddata),intent(in) :: gd
          real(dpn),dimension(3) :: hmin,hmax
          hmin = getHMin(gd); hmax = getHMax(gd)
          if (p%x.lt.hmin(1)) call boundsError('xmin')
          if (p%x.gt.hmax(1)) call boundsError('xmax')
          if (p%y.lt.hmin(2)) call boundsError('ymin')
          if (p%y.gt.hmax(2)) call boundsError('ymax')
          if (p%z.lt.hmin(3)) call boundsError('zmin')
          if (p%z.gt.hmax(3)) call boundsError('zmax')
        end subroutine

        subroutine boundsError(name)
          implicit none
          character(len=*),intent(in) :: name
          write(*,*) 'Error: probe bounds are not correct in ',name; stop
        end subroutine

        subroutine fixLocationDirectionless(p,gd,dir)
          implicit none
          type(probe),intent(in) :: p
          type(griddata),intent(in) :: gd
          integer,intent(in) :: dir
          real(dpn),dimension(:),allocatable :: hn,dist
          integer :: Ndir,i
          integer,dimension(3) :: N
          integer,dimension(1) :: ML
          call getN(gd,N)
          Ndir = N(dir)
          allocate(hn(Ndir+1))
          select case (dir)
          case (1); call getXn(gd,hn)
          case (2); call getYn(gd,hn)
          case (3); call getZn(gd,hn)
          end select
          tol = 
          do i=1,Ndir+1
            dist(i) = hn(i) - h_desired(dir)
          enddo
          ML = minloc(dist)
          p%h(dir) = hn(ML(1))
          p%i(dir) = ML(1)
          deallocate(hn,dist)
        end subroutine

        subroutine fixLocation(p,gd)
          implicit none
          type(probe),intent(in) :: p
          type(griddata),intent(in) :: gd
          call fixLocationDirectionless(p,gd,1)
          call fixLocationDirectionless(p,gd,2)
          call fixLocationDirectionless(p,gd,3)
        end subroutine


        subroutine applyTransientProbe(p,gd,u)
          implicit none
          type(probe),intent(in) :: p
          type(griddata),intent(in) :: gd
          real(dpn),dimension(:,:,:),intent(in) :: u
          s = shape(u)
          k = mod(Ni,2)

          p%u(j) = (u(p%i(1),p%i(2),p%i(3))+dble(k(p%component)) * &
                u(p%i(1)+x,p%i(2)+y,p%i(3)+z))/(dble(1.0)+dble(k(p%component)))
          p%j = p%j+1
        end subroutine

        subroutine applyTransientError(p,u,u_exact)
          implicit none
          type(probe),intent(in) :: p
          real(dpn),dimension(:,:,:),intent(in) :: u,u_exact
          call computeError(p%e,u,u_exact)
          p%u(j) = getL2(p%e)
          p%j = p%j+1
        end subroutine

       subroutine printProbe(p)
         implicit none
         type(probe), intent(in) :: p
         call writeProbeToFileOrScreen(p,6)
       end subroutine

       subroutine writeProbeToFileOrScreen(p,u)
         implicit none
         type(probea), intent(in) :: p
         integer,intent(in) :: u

         write(u,*) '----------------PROBE--------------'
         write(u,'(A9,'//xyzifmt//')') ' data shape = ',p%s
         write(u,'(A9,'//xyzifmt//')') ' data index = ',p%i
         write(u,'(A8,'//xyzfmt//')')  ' desired location = ',p%h_desired
         write(u,'(A8,'//xyzfmt//')')  ' probed location = ',p%h
         write(u,'(A8,'//xyzfmt//')')  ' component = ',p%h
         write(u,'(A8,'//xyzfmt//')')  ' Nsteps = ',p%Nsteps

         if (p%errorProbe) then
           write(u,'(A8,'//xyzfmt//')')  ' error = ',getL2(p%e)
         else
           write(u,'(A8,'//xyzfmt//')')  ' value = ',p%u(p%j)
         endif
       end subroutine


       end module