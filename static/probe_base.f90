       module probe_base_mod
       ! Implementation:
       ! 
       ! indexProbe:
       !       type(indexProbe) :: p                              ! 
       !       call init(p,g,s,i,dir,name,TF_freshStart)          ! enables other routines
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
       !       call init(p,dir,name,TF_freshStart)                ! enables other routines
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
       ! NOTE: init prints the index location if one exists.
       ! 
       use probe_transient_mod
       use IO_tools_mod
       use grid_mod
       use norms_mod

       implicit none

       private
       public :: indexProbe,errorProbe
       public :: init,set,apply
       public :: export, print
       public :: defineH,resetH,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       type indexProbe
         type(probe) :: p                             ! probe
         real(cp),dimension(3) :: h                   ! probe location
         integer,dimension(3) :: i                    ! index of location
       end type

       type errorProbe
         type(probe) :: p                             ! probe
         type(norms) :: e                             ! for computing data
       end type

       interface init;          module procedure initIndexProbe;            end interface
       interface init;          module procedure initErrorProbe;            end interface

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

        subroutine initIndexProbe(ip,dir,name,TF_freshStart,s,i,g)
          implicit none
          type(indexProbe),intent(inout) :: ip
          type(grid),intent(in) :: g
          integer,dimension(3),intent(in) :: s,i
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          integer :: k
          call init(ip%p,dir,name,TF_freshStart)
          call defineH(i,g,s,ip%h)
          ip%i = (/(maxval((/i(k),1/)),k=1,3)/)
        end subroutine

        subroutine initErrorProbe(ep,dir,name,TF_freshStart)
          implicit none
          type(errorProbe),intent(inout) :: ep
          character(len=*),intent(in) :: dir
          character(len=*),intent(in) :: name
          logical,intent(in) :: TF_freshStart
          call init(ep%p,dir,name,TF_freshStart)
        end subroutine

        subroutine resetH(ip,h)
         implicit none
          type(indexProbe),intent(inout) :: ip
          real(cp),dimension(3),intent(in) :: h
          ip%h = h
        end subroutine

        subroutine setIndexData(ip,n,d)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(cp),intent(in) :: d
          call set(ip%p,n,d)
        end subroutine

        subroutine setIndexData1(ip,n,u)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(cp),dimension(:,:,:),intent(in) :: u
          integer,dimension(3) :: temp
          temp(1) = maxval((/ip%i(1),1/))
          temp(2) = maxval((/ip%i(2),1/))
          temp(3) = maxval((/ip%i(3),1/))
          call set(ip%p,n,u(temp(1),temp(2),temp(3)))
        end subroutine

        subroutine setErrorData(ep,n,d)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(cp),intent(in) :: d
          call set(ep%p,n,d)
        end subroutine

        subroutine setErrorData1(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(cp),dimension(:),intent(in) :: u
          call compute(ep%e,real(0.0,cp),u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine setErrorData2(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(cp),dimension(:,:),intent(in) :: u
          call compute(ep%e,real(0.0,cp),u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine setErrorData3(ep,n,u)
         implicit none
          type(errorProbe),intent(inout) :: ep
          integer,intent(in) :: n
          real(cp),dimension(:,:,:),intent(in) :: u
          call compute(ep%e,real(0.0,cp),u)
          call set(ep%p,n,getL2(ep%e))
        end subroutine

        subroutine applyIndexProbe(ip,n,u)
         implicit none
          type(indexProbe),intent(inout) :: ip
          integer,intent(in) :: n
          real(cp),dimension(:,:,:),intent(in) :: u
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
          real(cp),dimension(:,:,:),intent(in) :: u
          call set(ep,n,u)
          call apply(ep%p)
        end subroutine

        subroutine defineH(i,g,s,h)
         implicit none
          integer,dimension(3),intent(in) :: i
          type(grid),intent(in) :: g
          integer,dimension(3),intent(in) :: s
          real(cp),dimension(3),intent(inout) :: h
          h = 0
          if (s(1).eq.g%c(1)%sc) h(1) = g%c(1)%hc(i(1))
          if (s(1).eq.g%c(1)%sc) h(1) = g%c(1)%hc(i(1))
          if (s(2).eq.g%c(2)%sc) h(2) = g%c(2)%hc(i(2))
          if (s(2).eq.g%c(2)%sc) h(2) = g%c(2)%hc(i(2))
          if (s(3).eq.g%c(3)%sc) h(3) = g%c(3)%hc(i(3))
          if (s(3).eq.g%c(3)%sc) h(3) = g%c(3)%hc(i(3))

          if (s(1).eq.g%c(1)%sn) h(1) = g%c(1)%hn(i(1))
          if (s(1).eq.g%c(1)%sn) h(1) = g%c(1)%hn(i(1))
          if (s(2).eq.g%c(2)%sn) h(2) = g%c(2)%hn(i(2))
          if (s(2).eq.g%c(2)%sn) h(2) = g%c(2)%hn(i(2))
          if (s(3).eq.g%c(3)%sn) h(3) = g%c(3)%hn(i(3))
          if (s(3).eq.g%c(3)%sn) h(3) = g%c(3)%hn(i(3))
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