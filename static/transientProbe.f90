       module transientProbe_mod
       ! Implementation:
       ! 
       !       type(probe) :: p
       !       call initialize(p,dir,name,TF_freshStart)        ! enables print/export
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       use simParams_mod
       use constants_mod
       use myIO_mod

       implicit none

       private
       public :: probe
       public :: initialize,set,apply
       public :: export, printProbe,delete

       type probe
         real(dpn) :: d                       ! transient data
         integer :: n                         ! n associated with data
         character(len=16) :: dir             ! probe directory
         character(len=16) :: name            ! probe name
         logical :: TF_freshStart             ! simulation starts from t=0
       end type

       interface initialize;    module procedure initializeProbe;           end interface
       interface set;           module procedure setProbeData;              end interface
       interface apply;         module procedure applyProbe;                end interface
       interface export;        module procedure exportProbe;               end interface
       interface printProbe;    module procedure printTransientProbe;       end interface
       interface delete;        module procedure deleteProbe;               end interface

       contains

       subroutine initializeProbe(p,dir,name,TF_freshStart)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: TF_freshStart
         p%dir = dir
         p%name = name
         p%TF_freshStart = TF_freshStart
       end subroutine

       subroutine setProbeData(p,n,d)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: n
         real(dpn),intent(in) :: d
         p%n = n; p%d = d
       end subroutine

       subroutine applyProbe(p)
         implicit none
         type(probe),intent(inout) :: p
         call writeTransientToFile(p%n,p%d,adjustl(trim(p%dir)),&
          adjustl(trim(p%name)),p%TF_freshStart)
         p%TF_freshStart = .false.
       end subroutine

       subroutine deleteProbe(p)
        implicit none
        type(probe),intent(in) :: p
        integer :: utemp
        utemp = getUnit(trim(adjustl(p%dir)),trim(adjustl(p%name)))
        close(utemp)
      end subroutine

       subroutine printTransientProbe(p,u)
         implicit none
         type(probe), intent(in) :: p
         integer,intent(in),optional :: u
         if (.not.present(u)) then
           write(u,*) ' ---------------- PROBE -------------- '
           call writeProbeToFileOrScreen(p,6)
         else
           call writeProbeToFileOrScreen(p,6)
         endif
       end subroutine

       subroutine exportProbe(p,u)
         implicit none
         type(probe), intent(in) :: p
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(p%dir,p%name//'_info')
           write(newU,*) ' ---------------- PROBE -------------- '
         else; newU = u
         endif

         call writeProbeToFileOrScreen(p,newU)

         if (.not.present(u)) then
           call closeAndMessage(newU,p%name//'_info',p%dir)
         endif
       end subroutine

       subroutine writeProbeToFileOrScreen(p,u)
         implicit none
         type(probe), intent(in) :: p
         integer,intent(in) :: u
         write(u,*) ' directory = ',p%dir
         write(u,*) ' name = ',p%name
         write(u,*) ' fresh start = ',p%TF_freshStart
       end subroutine

       end module