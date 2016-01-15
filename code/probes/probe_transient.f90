       module probe_transient_mod
       ! Implementation:
       ! 
       !       type(probe) :: p
       !       call init(p,dir,name,TF_freshStart)              ! enables print/export
       !       call print(p)                                    ! prints basic info (no data)
       !       call export(p)                                   ! exports basic probe info (no data)
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call apply(p)                                  ! exports transient data (n,d)
       !       enddo
       ! 
       use IO_tools_mod

       implicit none

       private
       public :: probe
       public :: init,set,apply
       public :: export, printProbe,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type probe
         real(cp) :: d                        ! transient data
         integer :: n                         ! n associated with data
         character(len=25) :: dir             ! probe directory
         character(len=25) :: name            ! probe name
         integer :: un_d                      ! file unit number for data
         integer :: un_i                      ! file unit number for info file
         logical :: TF_freshStart             ! simulation starts from t=0
       end type

       interface init;        module procedure initProbe;            end interface
       interface set;         module procedure setProbeData;         end interface
       interface apply;       module procedure applyProbe;           end interface

       interface export;      module procedure export_Probe_info;    end interface
       interface printProbe;  module procedure printTransientProbe;  end interface
       interface delete;      module procedure deleteProbe;          end interface

       contains

       subroutine initProbe(p,dir,name,TF_freshStart)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: TF_freshStart
         p%dir = dir
         p%name = name
         p%TF_freshStart = TF_freshStart
         if (p%TF_freshStart) then;          p%un_d = newAndOpen(dir,name)
         write(p%un_d,*) 'TITLE = "probe for '//name//'"'
         write(p%un_d,*) 'VARIABLES = N,'//name
         write(p%un_d,*) 'ZONE DATAPACKING = POINT'
         elseif (.not.p%TF_freshStart) then; p%un_d = openToAppend(dir,name)
         else; stop 'Error: no case found in initProbe in probe_transient.f90'
         endif
       end subroutine

       subroutine setProbeData(p,n,d)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: n
         real(cp),intent(in) :: d
         p%n = n; p%d = d
       end subroutine

       subroutine applyProbe(p)
         implicit none
         type(probe),intent(inout) :: p
         write(p%un_d,'(2'//arrfmt//')') real(p%n,cp),p%d
         p%TF_freshStart = .false.
       end subroutine

       subroutine deleteProbe(p)
         implicit none
         type(probe),intent(in) :: p
         close(p%un_d); close(p%un_i)
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

       subroutine export_Probe_info(p,u)
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
       end subroutine

       end module