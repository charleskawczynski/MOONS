       module derivedProbes_mod
       ! Implementation:
       ! 
       ! iProbe:
       !       type(iProbe) :: p
       !       call initialize(p,gd,s,i,dir,name,TF_freshStart)
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       ! centerProbe:
       !       type(centerProbe) :: p
       !       call initialize(p,s,dir,name,TF_freshStart)
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       ! aveProbe:
       !       type(aveProbe) :: p
       !       call initialize(p,gd,s,i,component,dir,name,TF_freshStart)
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       ! planeErrorProbe:
       !       type(planeErrorProbe) :: p
       !       call initialize(p,s,n)
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       ! errorProbe:
       !       type(errorProbe) :: p
       !       call initialize(p,s,n)
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       ! NOTE: initialize prints the index location if one exists.
       ! 
       use simParams_mod
       use constants_mod
       use transientProbe_mod
       use baseProbes_mod
       use myIO_mod

       use griddata_mod
       use myError_mod

       implicit none

       private
       public :: centerProbe,aveProbe
       public :: planeErrorProbe,avePlaneErrorProbe
       public :: initialize,apply
       public :: export, print, delete

       type centerProbe
         type(indexProbe) :: ip                       ! index probe
       end type

       type aveProbe
         type(indexProbe) :: ip                       ! index probe
         integer :: x,y,z                             ! matrix for direction of average
       end type

       type planeErrorProbe
         type(errorProbe) :: ep                       ! probe
         integer :: i,dir                             ! index for plane, direction of plane
         real(dpn) :: h                               ! location of plane
       end type

       type avePlaneErrorProbe
         type(errorProbe) :: ep                       ! probe
         integer :: i,dir                             ! index for plane, direction of plane
         real(dpn) :: h                               ! location of plane
       end type

       interface initialize;    module procedure initializeCenterProbe;         end interface
       interface initialize;    module procedure initializeAveProbe;            end interface
       interface initialize;    module procedure initializePlaneErrorProbe;     end interface
       interface initialize;    module procedure initializeAvePlaneErrorProbe;  end interface

       interface apply;         module procedure applyCenterProbe;              end interface
       interface apply;         module procedure applyAveProbe;                 end interface
       interface apply;         module procedure applyPlaneErrorProbe;          end interface
       interface apply;         module procedure applyAvePlaneErrorProbe;       end interface

       interface export;        module procedure exportCenterProbe;             end interface
       interface export;        module procedure exportAveProbe;                end interface
       interface export;        module procedure exportPlaneErrorProbe;         end interface
       interface export;        module procedure exportAvePlaneErrorProbe;      end interface

       interface print;         module procedure printCenterProbe;              end interface
       interface print;         module procedure printAveProbe;                 end interface
       interface print;         module procedure printPlaneErrorProbe;          end interface
       interface print;         module procedure printAvePlaneErrorProbe;       end interface

       interface delete;        module procedure deleteCenterProbe;             end interface
       interface delete;        module procedure deleteAveProbe;                end interface
       interface delete;        module procedure deletePlaneErrorProbe;         end interface
       interface delete;        module procedure deleteAvePlaneErrorProbe;      end interface

       contains

        ! ------------------ INITIALIZE PROBE -----------------------

        subroutine initializeCenterProbe(p,dir,name,TF_freshStart,s,gd)
          implicit none
          type(centerProbe),intent(inout) :: p
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          integer,dimension(3),intent(in) :: s
          type(griddata),intent(in) :: gd
          call initialize(p%ip,dir,name,TF_freshStart,s,(s+1)/2,gd)
        end subroutine

        subroutine initializeAveProbe(p,dir,name,TF_freshStart,s,i,gd,component)
          implicit none
          type(aveProbe),intent(inout) :: p
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          integer,dimension(3),intent(in) :: s,i
          type(griddata),intent(in) :: gd
          integer,intent(in) :: component
          ! Local variables
          real(dpn),dimension(3) :: h1,h2,h
          integer,dimension(3) :: i1,i2
          call initialize(p%ip,dir,name,TF_freshStart,s,i,gd)

          select case (component)
          case (1); p%x=1;p%y=0;p%z=0
          case (2); p%x=0;p%y=1;p%z=0
          case (3); p%x=0;p%y=0;p%z=1
          case default
            write(*,*) 'Error: component must = 1,2,3 in initializeaveProbe.';stop
          end select

          ! Define location based on average:
          i1 = i; h1 = p%ip%h
          i2 = (/i1(1)+p%x,i1(2)+p%y,i1(3)+p%z/)
          call defineH(i2,gd,s,h2)
          h = real(0.5,dpn)*(h1 + h2)
          call resetH(p%ip,h)
        end subroutine

        subroutine initializePlaneErrorProbe(p,dir,name,TF_freshStart,s,i,gd,direction)
          implicit none
          type(planeErrorProbe),intent(inout) :: p
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          integer,intent(in) :: direction
          ! Local variables
          real(dpn),dimension(3) :: h
          call initialize(p%ep,dir,name,TF_freshStart)
          p%i = i(direction)
          p%dir = direction
          ! DANGER: this passes nonsense to directions orthogonal to 'direction':
          call defineH((/i(direction),i(direction),i(direction)/),gd,s,h)
          p%h = h(direction)
        end subroutine

        subroutine initializeAvePlaneErrorProbe(p,dir,name,TF_freshStart,s,i,gd,direction)
          implicit none
          type(avePlaneErrorProbe),intent(inout) :: p
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          integer,dimension(3),intent(in) :: s,i
          type(griddata),intent(in) :: gd
          integer,intent(in) :: direction
          ! Local variables
          real(dpn),dimension(3) :: h1,h2

          call initialize(p%ep,dir,name,TF_freshStart)
          p%i = i(direction)
          p%dir = direction

          ! DANGER: this passes nonsense to directions orthogonal to 'direction':
          call defineH((/i(direction),i(direction),i(direction)/),gd,s,h1)
          call defineH((/i(direction)+1,i(direction)+1,i(direction)+1/),gd,s,h2)

          ! Define location based on average:
          p%h = real(0.5,dpn)*(h1(direction) + h2(direction))
        end subroutine

        ! ------------------ APPLY PROBE -----------------------

        subroutine applyCenterProbe(p,n,u)
         implicit none
          type(centerProbe),intent(inout) :: p
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          call set(p%ip,n,u(p%ip%i(1),p%ip%i(2),p%ip%i(3)))
          call apply(p%ip)
        end subroutine

        subroutine applyAveProbe(p,n,u)
         implicit none
          type(aveProbe),intent(inout) :: p
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          real(dpn) :: d
          d = real(0.5,dpn)*(u(p%ip%i(1)    ,p%ip%i(2)    ,p%ip%i(3)) +&
                             u(p%ip%i(1)+p%x,p%ip%i(2)+p%y,p%ip%i(3)+p%z))
          call set(p%ip,n,d)
          call apply(p%ip)
        end subroutine

        subroutine applyPlaneErrorProbe(p,n,u)
          implicit none
          type(planeErrorProbe),intent(inout) :: p
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          select case (p%dir)
          case (1); call computeError(p%ep%e,zero,u(p%i,:,:))
          case (2); call computeError(p%ep%e,zero,u(:,p%i,:))
          case (3); call computeError(p%ep%e,zero,u(:,:,p%i))
          case default
            write(*,*) 'Error: dir must = 1,2,3 in applyPlaneErorrProbe.';stop
          end select
          call set(p%ep,n,getL2(p%ep%e))
          call apply(p%ep)
        end subroutine

        subroutine applyAvePlaneErrorProbe(p,n,u)
          implicit none
          type(avePlaneErrorProbe),intent(inout) :: p
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          select case (p%dir)
          case (1); call computeError(p%ep%e,zero,real(0.5,dpn)*(u(p%i,:,:)+u(p%i+1,:,:)))
          case (2); call computeError(p%ep%e,zero,real(0.5,dpn)*(u(:,p%i,:)+u(:,p%i+1,:)))
          case (3); call computeError(p%ep%e,zero,real(0.5,dpn)*(u(:,:,p%i)+u(:,:,p%i+1)))
          case default
            write(*,*) 'Error: dir must = 1,2,3 in applyPlaneErorrProbe.';stop
          end select

          call set(p%ep,n,getL2(p%ep%e))
          call apply(p%ep)
        end subroutine

        ! ------------------ PRINT / EXPORT PROBE --------------------- Center probe

       subroutine printCenterProbe(p)
         implicit none
         type(centerProbe),intent(in) :: p
         call print(p%ip)
       end subroutine

       subroutine exportCenterProbe(p)
         implicit none
         type(centerProbe), intent(in) :: p
         call export(p%ip)
       end subroutine

        ! ------------------ PRINT / EXPORT PROBE --------------------- Ave probe

       subroutine printAveProbe(p)
         implicit none
         type(aveProbe),intent(in) :: p
         call print(p%ip)
         call writeAveProbeToFileOrScreen(p,6)
       end subroutine

       subroutine exportAveProbe(p,u)
         implicit none
         type(aveProbe), intent(in) :: p
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(p%ip%p%dir,p%ip%p%name//'_info')
           write(newU,*) ' ---------------- AVERAGE PROBE -------------- '
         else; newU = u
         endif

         call export(p%ip,newU)
         call writeAveProbeToFileOrScreen(p,newU)

         if (.not.present(u)) then
           call closeAndMessage(newU,p%ip%p%name//'_info',p%ip%p%dir)
         endif
       end subroutine

       subroutine writeAveProbeToFileOrScreen(p,u)
         implicit none
         type(aveProbe), intent(in) :: p
         integer,intent(in) :: u
         write(u,*) ' x,y,z = ',p%x,p%y,p%z
         write(u,*) ' i1 = ',p%ip%i
         write(u,*) ' i2 = ',(/p%ip%i(1)+p%x,p%ip%i(2)+p%y,p%ip%i(3)+p%z/)
       end subroutine

        ! ------------------ PRINT / EXPORT PROBE --------------------- Plane error probe

       subroutine printPlaneErrorProbe(p)
         implicit none
         type(planeErrorProbe),intent(in) :: p
         call print(p%ep)
         call writePlaneErrorProbeToFileOrScreen(p,6)
       end subroutine

       subroutine exportPlaneErrorProbe(p,u)
         implicit none
         type(planeErrorProbe), intent(in) :: p
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(p%ep%p%dir,p%ep%p%name//'_info')
           write(newU,*) ' ---------------- PLANE ERROR PROBE -------------- '
         else; newU = u
         endif
         call export(p%ep%p,newU)
         call writePlaneErrorProbeToFileOrScreen(p,newU)
         if (.not.present(u)) then
           call closeAndMessage(newU,p%ep%p%name//'_info',p%ep%p%dir)
         endif
       end subroutine

       subroutine writePlaneErrorProbeToFileOrScreen(p,u)
         implicit none
         type(planeErrorProbe), intent(in) :: p
         integer,intent(in) :: u
         write(u,*) ' i = ',p%i
         write(u,*) ' dir = ',p%dir
         write(u,*) ' h = ',p%h
       end subroutine

        ! ------------------ PRINT / EXPORT PROBE --------------------- Ave plane error probe

       subroutine printAvePlaneErrorProbe(p)
         implicit none
         type(avePlaneErrorProbe),intent(in) :: p
         call print(p%ep)
         call writeAvePlaneErrorProbeToFileOrScreen(p,6)
       end subroutine

       subroutine exportAvePlaneErrorProbe(p,u)
         implicit none
         type(avePlaneErrorProbe), intent(in) :: p
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(p%ep%p%dir,p%ep%p%name//'_info')
           write(newU,*) ' ---------------- AVERAGE PLANE ERROR PROBE -------------- '
         else; newU = u
         endif

         call export(p%ep%p,newU)
         call writeAvePlaneErrorProbeToFileOrScreen(p,newU)
         if (.not.present(u)) then
           call closeAndMessage(newU,p%ep%p%name//'_info',p%ep%p%dir)
         endif
       end subroutine

       subroutine writeAvePlaneErrorProbeToFileOrScreen(p,u)
         implicit none
         type(avePlaneErrorProbe), intent(in) :: p
         integer,intent(in) :: u
         write(u,*) ' i = ',p%i
         write(u,*) ' dir = ',p%dir
         write(u,*) ' h = ',p%h
       end subroutine


       ! -------------------------- DELETE ROUTINES --------------------

        subroutine deleteCenterProbe(p)
          implicit none
          type(centerProbe),intent(in) :: p
          call delete(p%ip)
        end subroutine

        subroutine deleteAveProbe(p)
          implicit none
          type(aveProbe),intent(in) :: p
          call delete(p%ip)
        end subroutine

        subroutine deletePlaneErrorProbe(p)
          implicit none
          type(planeErrorProbe),intent(in) :: p
          call delete(p%ep)
        end subroutine

        subroutine deleteAvePlaneErrorProbe(p)
          implicit none
          type(avePlaneErrorProbe),intent(in) :: p
          call delete(p%ep)
        end subroutine

       end module