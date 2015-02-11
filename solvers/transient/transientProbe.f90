       module transientProbe_mod
       ! To probe the center of face-based data, try something like
       ! call apply(p,gd,s,i1,i2,Nmax)
       ! s = shape(u)
       ! k = mod(Ni,2)
       ! call apply(p,gd,s,(/s(1)/2+k(1),s(1)/2+k(1),s(1)/2+k(1)/),&
       !                   (/s(1)/2+k(1),s(1)/2+k(1),s(1)/2+k(1)/),Nmax)
       use simParams_mod
       use constants_mod
       use myIO_mod
       use myDebug_mod
       use myAllocate_mod
       use scalarField_mod
       use vectorField_mod

       use griddata_mod
       use myError_mod

       implicit none

       private
       public :: initialize,delete,apply
       public :: export, printProbe

       type errorProbe
         type(myError) :: e
       end type

       type indexProbe
         real(dpn),dimension(3) :: h            ! x,y,z
         integer,dimension(3) :: i              ! index for x,y,z
       end type

       type centerProbe
         real(dpn),dimension(3) :: h            ! actual  x,y,z
         integer,dimension(3) :: i              ! index for x,y,z
       end type

       type interpProbe
         real(dpn),dimension(3) :: h            ! actual  x,y,z
         integer,dimension(3) :: i              ! index for x,y,z
         integer :: x,y,z                       ! matrix to define direction of component
       end type

       type probe
         real(dpn),dimension(:),alloatable :: d ! data
         integer,dimension(3) :: s              ! size of data
         type(centerProbe) :: cp
         type(indexProbe) :: ip
         type(interpProbe) :: intp
         type(errorProbe) :: ep
         integer :: Nmax                        ! max number of steps
         integer :: n                           ! nth step
         logical :: TF_cp,TF_ip,TF_intp,TF_ep
       end type

       interface initialize;    module procedure initializeIndexProbe;      end interface
       interface initialize;    module procedure initializeInterpProbe;     end interface
       interface initialize;    module procedure initializeErrorProbe;      end interface
       interface initialize;    module procedure initializeCenterProbe;     end interface
       interface apply          module procedure applyProbe;                end interface
       ! interface apply          module procedure applyIndexProbe;           end interface
       ! interface apply          module procedure applyInterpProbe;          end interface
       ! interface apply          module procedure applyErrorProbe;           end interface
       ! interface apply          module procedure applyCenterProbe;          end interface

       interface delete;        module procedure deleteProbe;               end interface
       interface export;        module procedure exportProbe;               end interface

       contains

        ! ------------------ INITIALIZE PROBE -----------------------

        subroutine initializeIndexProbe(p,gd,s,i,Nmax)
          implicit none
          type(probe),intent(inout) :: p
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          integer,intent(in) :: Nmax
          
          call delete(p)
          allocate(p%d(Nmax))
          p%Nmax = Nmax
          p%n = 1
          call defineH(p,gd,s)

          p%ip%i = i
          p%TF_ip = .true.
        end subroutine

        subroutine initializeInterpProbe(p,gd,s,i,component,Nmax)
          implicit none
          type(probe),intent(inout) :: p
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          integer,intent(in) :: component,Nmax

          call delete(p)
          allocate(p%d(Nmax))
          p%Nmax = Nmax
          p%n = 1
          call defineH(p,gd,s)

          p%intp%i = i
          select case (component)
          case (1); p%intp%x=1;p%intp%y=0;p%intp%z=0
          case (1); p%intp%x=0;p%intp%y=1;p%intp%z=0
          case (1); p%intp%x=0;p%intp%y=0;p%intp%z=1
          case default
            write(*,*) 'Error: component must = 1,2,3 in initializeInterpProbe.';stop
          end select
          p%TF_intp = .true.
        end subroutine

        subroutine initializeCenterProbe(p,s,Nmax)
          implicit none
          type(probe),intent(inout) :: p
          integer,dimension(3),intent(in) :: s
          integer,intent(in) :: Nmax
          
          call delete(p)
          allocate(p%d(Nmax))
          p%Nmax = Nmax
          p%n = 1
          call defineH(p,gd,s)

          p%cp%i = (s+1)/2
          p%TF_cp = .true.
        end subroutine

        subroutine initializeErorrProbe(p,Nmax)
          implicit none
          type(probe),intent(inout) :: p
          integer,intent(in) :: Nmax
          call delete(p)
          allocate(p%d(Nmax))
          p%Nmax = Nmax
          p%n = 1
          p%TF_ep = .true.
        end subroutine

        ! ------------------ APPLY PROBE -----------------------

        subroutine applyProbe(p,u)
         implicit none
          type(probe),intent(inout) :: p
          real(dpn),dimension(:,:,:),intent(in) :: u
          if (p%TF_ip) then
            p%d(n) = u(p%ip%i(1),p%ip%i(2),p%ip%i(3))
          elseif (p%TF_intp) then
            p%d(n) = (u(p%intp%i(1),p%intp%i(2),p%intp%i(3)) + &
            u(p%intp%i(1)+p%intp%x,p%intp%i(2)+p%intp%y,p%intp%i(3)+p%intp%z))/2.0d0
          elseif (p%TF_ep) then
            call computeError(p%ep%e,u,zero)
            p%d(n) = getL2(p%ep%e)
          elseif (p%TF_cp) then
            p%d(n) = u(p%cp%i(1),p%cp%i(2),p%cp%i(3))
          endif
          p%n = p%n + 1
        end subroutine

        ! subroutine applyIndexProbe(p,u)
        !   implicit none
        !   type(probe),intent(inout) :: p
        !   real(dpn),dimension(:,:,:),intent(in) :: u
        !   p%d(n) = u(p%ip%i(1),p%ip%i(2),p%ip%i(3))
        !   p%n = p%n + 1
        ! end subroutine
        ! subroutine applyInterpProbe(p,u)
        !   implicit none
        !   type(probe),intent(inout) :: p
        !   real(dpn),dimension(:,:,:),intent(in) :: u
        !   p%d(n) = (u(p%intp%i1(1),p%intp%i1(2),p%intp%i1(3)) + &
        !             u(p%intp%i2(1),p%intp%i2(2),p%intp%i2(3)))/2.0d0
        !   p%n = p%n + 1
        ! end subroutine
        ! subroutine applyIndexProbe(p,u)
        !   implicit none
        !   type(probe),intent(inout) :: p
        !   real(dpn),dimension(:,:,:),intent(in) :: u
        !   p%d(n) = u(p%cp%i(1),p%cp%i(2),p%cp%i(3))
        !   p%n = p%n + 1
        ! end subroutine
        ! subroutine applyErrorProbe(p,u)
        !   implicit none
        !   type(probe),intent(inout) :: p
        !   real(dpn),dimension(:,:,:),intent(in) :: u
        !   call computeError(p%ep%e,u,zero)
        !   p%d(n) = getL2(p%ep%e)
        !   p%n = p%n + 1
        ! end subroutine

        ! ------------------ DELETE PROBE / DEFINE H -----------------------

        subroutine deleteProbe(p)
          implicit none
          type(probe),intent(inout) :: p
          if (allocated(p%d)) deallocate(p%d)
          p%TF_ip = .false.
          p%TF_intp = .false.
          p%TF_cp = .false.
          p%TF_ep = .false.
        end subroutine

        subroutine defineH(p,gd,s,gridType)
         ! Uses the index in probe p to define the location h
         implicit none
          type(probe),intent(inout) :: p
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s
          integer,intent(in) :: gridType
          ! The following procedure may result in 
          ! false values of h if Nwall = 1 along any direction:
          select case (gridType)
          case (1) ! cell centered data
            if (s(1).eq.Ni(1)+2) p%ip%h(1) = gd%xci(p%ip%i(1))
            if (s(1).eq.N(1)+2)  p%ip%h(1) = gd%xct(p%ip%i(1))
            if (s(2).eq.Ni(2)+2) p%ip%h(2) = gd%yci(p%ip%i(2))
            if (s(2).eq.N(2)+2)  p%ip%h(2) = gd%yct(p%ip%i(2))
            if (s(3).eq.Ni(3)+2) p%ip%h(3) = gd%zci(p%ip%i(3))
            if (s(3).eq.N(3)+2)  p%ip%h(3) = gd%zct(p%ip%i(3))
          case (2) ! node centered data
            if (s(1).eq.Ni(1)+1) p%ip%h(1) = gd%xni(p%ip%i(1))
            if (s(1).eq.N(1)+1)  p%ip%h(1) = gd%xnt(p%ip%i(1))
            if (s(2).eq.Ni(2)+1) p%ip%h(2) = gd%yni(p%ip%i(2))
            if (s(2).eq.N(2)+1)  p%ip%h(2) = gd%ynt(p%ip%i(2))
            if (s(3).eq.Ni(3)+1) p%ip%h(3) = gd%zni(p%ip%i(3))
            if (s(3).eq.N(3)+1)  p%ip%h(3) = gd%znt(p%ip%i(3))
          case default
          write(*,*) 'Error: gridType must = 1,2 in initializeIndexProbe.';stop
          end select
        end subroutine

        ! ------------------ PRINT / EXPORT PROBE ---------------------

       subroutine printProbe(p)
         implicit none
         type(probe), intent(in) :: p
         call writeProbeToFileOrScreen(p,6)
       end subroutine

       subroutine exportProbe(p,name,dir)
         implicit none
         type(probe), intent(in) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         call writeToFile(p%n,p%d,dir//'parameter/','transient_'//name,.false.)
       end subroutine

       subroutine writeProbeToFileOrScreen(p,u)
         implicit none
         type(probea), intent(in) :: p
         integer,intent(in) :: u

         write(u,*) '----------------PROBE--------------'
         write(u,'(A9,'//xyzifmt//')') ' data shape = ',p%s
         write(u,'(A10,'//xyzfmt//')') ' Nsteps = ',p%Nsteps
         write(u,'(A9,'//xyzfmt//')')  ' value = ',p%d(n)
         
         if (p%TF_ip)   write(u,'(A15,'//xyzifmt//')') ' probe index = ',p%ip%i
         if (p%TF_cp)   write(u,'(A15,'//xyzifmt//')') ' probe index = ',p%cp%i
         if (p%TF_intp) write(u,'(A19,'//xyzifmt//')') ' probe index (1) = ',p%intp%i1
         if (p%TF_intp) write(u,'(A19,'//xyzifmt//')') ' probe index (2) = ',p%intp%i2

         if (p%TF_ip)   write(u,'(A18,'//xyzifmt//')') ' probe location = ',p%ip%h
         if (p%TF_cp)   write(u,'(A18,'//xyzifmt//')') ' probe location = ',p%cp%h
         if (p%TF_intp) write(u,'(A18,'//xyzifmt//')') ' probe location = ',p%intp%h

         if (p%TF_ip)   write(u,'(A9,'//xyzifmt//')') ' error = ',getL2(p%ep%e)
       end subroutine


       end module