       module transientProbe_mod
       ! Implementation:
       ! 
       ! indexProbe:
       !       type(indexProbe) :: p
       !       call initialize(p,gd,s,i,dir,name,TF_freshStart)
       !       call apply(p,u)
       !       call delete(p)
       ! 
       ! centerProbe:
       !       type(centerProbe) :: p
       !       call initialize(p,s,dir,name,TF_freshStart)
       !       call apply(p,u)
       !       call delete(p)
       ! 
       ! aveProbe:
       !       type(aveProbe) :: p
       !       call initialize(p,gd,s,i,component,dir,name,TF_freshStart)
       !       call apply(p,u)
       !       call delete(p)
       ! 
       ! planeErrorProbe:
       !       type(planeErrorProbe) :: p
       !       call initialize(p,s,n)
       !       call apply(p,u)
       !       call delete(p)
       ! 
       ! errorProbe:
       !       type(errorProbe) :: p
       !       call initialize(p,s,n)
       !       call apply(p,u)
       !       call delete(p)
       ! 
       ! NOTE: initialize prints the index location if one exists.
       ! 
       use simParams_mod
       use constants_mod
       use transientProbe_mod
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

       ! Base classes:

       type probe
         real(dpn) :: d                               ! transient data
         integer :: n                                 ! n associated with data
         character(len=7) :: dir                      ! directory/name of probe
         character(len=2) :: name                     ! directory/name of probe
         logical :: TF_freshStart                     ! simulation starts from t=0
       end type

       type indexProbe
         type(probe) :: p                             ! probe
         real(dpn),dimension(3) :: h                  ! probe location
         integer,dimension(3) :: i                    ! index of location
       end type

       ! Derived classes:

       type centerProbe
         type(indexProbe) :: ip                       ! index probe
       end type

       type aveProbe
         type(indexProbe) :: ip                       ! index probe
         integer :: x,y,z                             ! matrix for direction of average
       end type

       ! Base classes:

       type errorProbe
         type(probe) :: p                             ! probe
         type(myError) :: e                           ! for computing data
       end type

       ! Derived classes:

       type planeErrorProbe
         type(errorProbe) :: p                        ! probe
         integer :: i,dir                             ! index for plane, direction of plane
         real(dpn) :: h                               ! location of plane
       end type

       interface initialize;    module procedure initializeIndexProbe;      end interface
       interface initialize;    module procedure initializeCenterProbe;     end interface
       interface initialize;    module procedure initializeAveProbe;        end interface

       interface initialize;    module procedure initializePlaneErrorProbe; end interface
       interface initialize;    module procedure initializeErrorProbe;      end interface
       interface apply;         module procedure applyIndexProbe;           end interface
       interface apply;         module procedure applyCenterProbe;          end interface
       interface apply;         module procedure applyaveProbe;             end interface
       interface apply;         module procedure applyErrorProbe;           end interface
       interface apply;         module procedure applyPlaneErrorProbe;      end interface
       interface delete;        module procedure deleteProbe;               end interface
       interface export;        module procedure exportProbe;               end interface

       contains

        subroutine initializeProbe(p,dir,name,TF_freshStart)
          implicit none
          type(probe),intent(inout) :: p
          character(len=7),intent(in) :: dir
          character(len=2),intent(in) :: name
          p%dir = dir
          p%name = name
          p%TF_freshStart = TF_freshStart
        end subroutine

        subroutine initializeIndexProbe(ip,gd,s,i,dir,name,TF_freshStart)
          implicit none
          type(indexProbe),intent(inout) :: ip
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          character(len=7),intent(in) :: dir
          character(len=2),intent(in) :: name
          call initialize(ip%p,dir,name,TF_freshStart)
          call defineH(p,gd,s,ip%h)
          ip%i = i
        end subroutine

        ! ------------------ INITIALIZE PROBE -----------------------

        ! Index based

        subroutine initializeCenterProbe(cp,s,dir,name,TF_freshStart)
          implicit none
          type(centerProbe),intent(inout) :: cp
          integer,dimension(3),intent(in) :: s
          character(len=7),intent(in) :: dir
          character(len=2),intent(in) :: name
          call initialize(cp%ip,dir,name,TF_freshStart)
          call defineH(p,gd,s,cp%ip%h)
          cp%ip%i = (s+1)/2
        end subroutine

        subroutine initializeAveProbe(ap,gd,s,i,component,dir,name,TF_freshStart)
          implicit none
          type(aveProbe),intent(inout) :: ap
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          integer,intent(in) :: component
          character(len=7),intent(in) :: dir
          character(len=2),intent(in) :: name
          call initialize(ap%ip,dir,name,TF_freshStart)
          call defineH(ap%ip,gd,s,ap%ip%h)

          ap%ip%i = i
          select case (component)
          case (1); ap%x=1;ap%y=0;ap%z=0
          case (2); ap%x=0;ap%y=1;ap%z=0
          case (3); ap%x=0;ap%y=0;ap%z=1
          case default
            write(*,*) 'Error: component must = 1,2,3 in initializeaveProbe.';stop
          end select
        end subroutine

        ! Error based

        subroutine initializePlaneErorrProbe(p,dir,name,TF_freshStart)
          implicit none
          type(probe),intent(inout) :: p
          character(len=7),intent(in) :: dir
          character(len=2),intent(in) :: name
          call initialize(ap%p,dir,name,TF_freshStart)
          call defineH(ap%p,gd,s,ap%h)

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
          integer,intent(in) :: n
          character(len=*),intent(in) :: name
          character(len=*),intent(in) :: dir
          call writeTransientToFile(n,u,p%dir,p%name,TF)
        end subroutine

        subroutine applyIndexProbe(p,u)
          implicit none
          type(probe),intent(inout) :: p
          real(dpn),dimension(:,:,:),intent(in) :: u
          p%d(n) = u(p%ip%i(1),p%ip%i(2),p%ip%i(3))
          p%n = p%n + 1
        end subroutine
        ! subroutine applyaveProbe(p,u)
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
            if (s(1).eq.Ni(1)+2) p%h(1) = gd%xci(p%ip%i(1))
            if (s(1).eq.N(1)+2)  p%h(1) = gd%xct(p%ip%i(1))
            if (s(2).eq.Ni(2)+2) p%h(2) = gd%yci(p%ip%i(2))
            if (s(2).eq.N(2)+2)  p%h(2) = gd%yct(p%ip%i(2))
            if (s(3).eq.Ni(3)+2) p%h(3) = gd%zci(p%ip%i(3))
            if (s(3).eq.N(3)+2)  p%h(3) = gd%zct(p%ip%i(3))
          case (2) ! node centered data
            if (s(1).eq.Ni(1)+1) p%h(1) = gd%xni(p%ip%i(1))
            if (s(1).eq.N(1)+1)  p%h(1) = gd%xnt(p%ip%i(1))
            if (s(2).eq.Ni(2)+1) p%h(2) = gd%yni(p%ip%i(2))
            if (s(2).eq.N(2)+1)  p%h(2) = gd%ynt(p%ip%i(2))
            if (s(3).eq.Ni(3)+1) p%h(3) = gd%zni(p%ip%i(3))
            if (s(3).eq.N(3)+1)  p%h(3) = gd%znt(p%ip%i(3))
          case default
          write(*,*) 'Error: gridType must = 1,2 in initializeIndexProbe.';stop
          end select
        end subroutine

        subroutine defineH(p,gd,s,h)
         implicit none
          type(centerProbe),intent(inout) :: p
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s
          real(dpn),intent(inout) :: h
          if (s(1).eq.Ni(1)+2) h(1) = gd%xci(p%ip%i(1))
          if (s(1).eq.N(1)+2)  h(1) = gd%xct(p%ip%i(1))
          if (s(2).eq.Ni(2)+2) h(2) = gd%yci(p%ip%i(2))
          if (s(2).eq.N(2)+2)  h(2) = gd%yct(p%ip%i(2))
          if (s(3).eq.Ni(3)+2) h(3) = gd%zci(p%ip%i(3))
          if (s(3).eq.N(3)+2)  h(3) = gd%zct(p%ip%i(3))

          if (s(1).eq.Ni(1)+1) h(1) = gd%xni(p%ip%i(1))
          if (s(1).eq.N(1)+1)  h(1) = gd%xnt(p%ip%i(1))
          if (s(2).eq.Ni(2)+1) h(2) = gd%yni(p%ip%i(2))
          if (s(2).eq.N(2)+1)  h(2) = gd%ynt(p%ip%i(2))
          if (s(3).eq.Ni(3)+1) h(3) = gd%zni(p%ip%i(3))
          if (s(3).eq.N(3)+1)  h(3) = gd%znt(p%ip%i(3))
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