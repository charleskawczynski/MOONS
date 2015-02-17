       module baseProbes_mod
       ! Implementation:
       ! 
       ! indexProbe:
       !       type(indexProbe) :: p                              ! 
       !       call initialize(p,gd,s,i,dir,name,TF_freshStart)   ! enables other routines
       !       call resetH(p,h)                                   ! if h needs to be reset
       !       call print(p)                                      ! prints basic info (no data)
       !       call export(p)                                     ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                  ! sets data to be exported
       !         call set(p,n,u)                                  ! sets data to be exported
       !         call apply(p)                                    ! exports transient data (n,d)
       !       enddo
       ! 
       ! errorProbe:
       !       type(errorProbe) :: p                              ! 
       !       call initialize(p,dir,name,TF_freshStart)          ! enables other routines
       !       call resetH(p,h)                                   ! if h needs to be reset
       !       call print(p)                                      ! prints basic info (no data)
       !       call export(p)                                     ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,u)                                  ! sets data to be exported
       !         call set(p,n,d)                                  ! sets data to be exported
       !         call apply(p)                                    ! exports transient data (n,d)
       !       enddo
       ! 
       ! NOTE: initialize prints the index location if one exists.
       ! 
       use simParams_mod
       use constants_mod
       use transientProbe_mod
       use myIO_mod
       use griddata_mod
       use myError_mod

       implicit none

       private
       public :: indexProbe,errorProbe
       public :: initialize,set,apply
       public :: export, print
       public :: defineH,resetH,delete

       type indexProbe
         type(probe) :: p                             ! probe
         real(dpn),dimension(3) :: h                  ! probe location
         integer,dimension(3) :: i                    ! index of location
       end type

       type errorProbe
         type(probe) :: p                             ! probe
         type(myError) :: e                           ! for computing data
       end type

       interface initialize;    module procedure initializeIndexProbe;      end interface
       interface initialize;    module procedure initializeErrorProbe;      end interface

       interface set;           module procedure setIndexData;              end interface
       interface set;           module procedure setIndexData1;             end interface

       interface set;           module procedure setErrorData;              end interface
       interface set;           module procedure setErrorData1;             end interface
       interface set;           module procedure setErrorData2;             end interface
       interface set;           module procedure setErrorData3;             end interface

       interface apply;         module procedure applyIndexProbe;           end interface
       interface apply;         module procedure applyErrorProbe;           end interface

       interface apply;         module procedure setApplyIndexProbe;        end interface
       interface apply;         module procedure setApplyErrorProbe;        end interface

       interface print;         module procedure printIndexProbe;           end interface
       interface print;         module procedure printErrorProbe;           end interface

       interface export;        module procedure exportIndexProbe;          end interface
       interface export;        module procedure exportErrorProbe;          end interface

       interface delete;        module procedure deleteIndexProbe;          end interface
       interface delete;        module procedure deleteErrorProbe;          end interface

       contains

        subroutine initializeIndexProbe(ip,dir,name,TF_freshStart,s,i,gd)
          implicit none
          type(indexProbe),intent(inout) :: ip
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s,i
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          call initialize(ip%p,dir,name,TF_freshStart)
          call defineH(i,gd,s,ip%h)
          ip%i = i
        end subroutine

        subroutine initializeErrorProbe(ep,dir,name,TF_freshStart)
          implicit none
          type(errorProbe),intent(inout) :: ep
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          call initialize(ep%p,dir,name,TF_freshStart)
        end subroutine

        subroutine resetH(ip,h)
         implicit none
          type(indexProbe),intent(inout) :: ip
          real(dpn),dimension(3),intent(in) :: h
          ip%h = h
        end subroutine

        subroutine setIndexData(ip,n,d)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(dpn),intent(in) :: d
          call set(ip%p,n,d)
        end subroutine

        subroutine setIndexData1(ip,n,u)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          call set(ip%p,n,u(ip%i(1),ip%i(2),ip%i(3)))
        end subroutine

        subroutine setErrorData(ep,n,d)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(dpn),intent(in) :: d
          call set(ep%p,n,d)
        end subroutine

        subroutine setErrorData1(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(dpn),dimension(:),intent(in) :: u
          call computeError(ep%e,zero,u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine setErrorData2(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(dpn),dimension(:,:),intent(in) :: u
          call computeError(ep%e,zero,u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine setErrorData3(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          call computeError(ep%e,zero,u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine applyIndexProbe(ip,n,u)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          call set(ip,n,u)
          call apply(ip%p)
        end subroutine

        subroutine setApplyIndexProbe(ip)
         implicit none
          type(indexProbe),intent(inout) :: ip
          call apply(ip%p)
        end subroutine

        subroutine applyErrorProbe(ep)
         implicit none
          type(errorProbe),intent(inout) :: ep
          call apply(ep%p)
        end subroutine

        subroutine setApplyErrorProbe(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(dpn),dimension(:,:,:),intent(in) :: u
          call set(ep,n,u)
          call apply(ep%p)
        end subroutine

        subroutine defineH(i,gd,s,h)
         implicit none
          integer,dimension(3),intent(in) :: i
          type(griddata),intent(in) :: gd
          integer,dimension(3),intent(in) :: s
          real(dpn),dimension(3),intent(inout) :: h
          integer,dimension(3) :: Ni,N
          call getNi(gd,Ni)
          call getN(gd,N)
          h = 0
          if (s(1).eq.Ni(1)+2) h(1) = gd%xci(i(1))
          if (s(1).eq.N(1)+2)  h(1) = gd%xct(i(1))
          if (s(2).eq.Ni(2)+2) h(2) = gd%yci(i(2))
          if (s(2).eq.N(2)+2)  h(2) = gd%yct(i(2))
          if (s(3).eq.Ni(3)+2) h(3) = gd%zci(i(3))
          if (s(3).eq.N(3)+2)  h(3) = gd%zct(i(3))

          if (s(1).eq.Ni(1)+1) h(1) = gd%xni(i(1))
          if (s(1).eq.N(1)+1)  h(1) = gd%xnt(i(1))
          if (s(2).eq.Ni(2)+1) h(2) = gd%yni(i(2))
          if (s(2).eq.N(2)+1)  h(2) = gd%ynt(i(2))
          if (s(3).eq.Ni(3)+1) h(3) = gd%zni(i(3))
          if (s(3).eq.N(3)+1)  h(3) = gd%znt(i(3))
        end subroutine


        ! ------------------ PRINT / EXPORT PROBE --------------------- index probe

       subroutine printIndexProbe(ip,u)
         implicit none
         type(indexProbe), intent(in) :: ip
         integer,intent(in),optional :: u
         if (.not.present(u)) then
           write(*,*) ' ---------- INDEX PROBE ---------'
           call printProbe(ip%p,6)
           call writeIndexProbeToFileOrScreen(ip,6)
         else
           call printProbe(ip%p,u)
           call writeIndexProbeToFileOrScreen(ip,u)
         endif
       end subroutine

       subroutine exportIndexProbe(ip,u)
         implicit none
         type(indexProbe), intent(in) :: ip
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
          newU = newAndOpen(ip%p%dir,ip%p%name//'_info')
           write(newU,*) ' ---------------- INDEX PROBE -------------- '
         else; newU = u
         endif

         call export(ip%p,newU)
         call writeIndexProbeToFileOrScreen(ip,newU)
         if (.not.present(u)) then
           call closeAndMessage(newU,ip%p%name//'_info',ip%p%dir)
         endif
       end subroutine

       subroutine writeIndexProbeToFileOrScreen(ip,u)
         implicit none
         type(indexProbe), intent(in) :: ip
         integer,intent(in) :: u
         write(u,*) ' index = ',ip%i
         write(u,*) ' location = ',ip%h
       end subroutine

        ! ------------------ PRINT / EXPORT PROBE --------------------- error probe

       subroutine printErrorProbe(ep,u)
         implicit none
         type(errorProbe), intent(in) :: ep
         integer,intent(in),optional :: u
         if (.not.present(u)) then
           write(*,*) ' ---------- ERROR PROBE ---------'
           call printProbe(ep%p,6)
         else
           call printProbe(ep%p)
         endif
       end subroutine

       subroutine exportErrorProbe(ep,u)
         implicit none
         type(errorProbe), intent(in) :: ep
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(ep%p%dir,ep%p%name//'_info')
           write(newU,*) ' ---------------- ERROR PROBE -------------- '
         else; newU = u
         endif

         call export(ep%p,newU)

         if (.not.present(u)) then
           call closeAndMessage(newU,ep%p%name//'_info',ep%p%dir)
         endif
       end subroutine

       subroutine deleteIndexProbe(ip)
         implicit none
         type(indexProbe), intent(in) :: ip
         call delete(ip%p)
       end subroutine

       subroutine deleteErrorProbe(ep)
         implicit none
         type(errorProbe), intent(in) :: ep
         call delete(ep%p)
       end subroutine

       end module