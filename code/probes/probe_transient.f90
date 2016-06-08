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
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       use is_nan_mod

       implicit none

       private
       public :: probe
       public :: init,set,apply
       public :: export, printProbe,delete

       type probe
         real(cp) :: d                        ! transient data
         real(cp) :: t                        ! t associated with data
         integer :: n                         ! n associated with data
         type(string) :: dir,name             ! probe directory and name
         integer :: un_d                      ! file unit number for data
         integer :: un_i                      ! file unit number for info file
         logical :: TF_freshStart             ! simulation starts from t=0
         real(cp) :: NaN,infinity
       end type

       interface init;        module procedure initProbe;            end interface
       interface set;         module procedure setProbeData;         end interface
       interface apply;       module procedure applyProbe;           end interface

       interface export;      module procedure export_TP_info;       end interface
       interface printProbe;  module procedure printTransientProbe;  end interface
       interface delete;      module procedure deleteProbe;          end interface

       contains

       subroutine initProbe(p,dir,name,TF_freshStart)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: TF_freshStart
         call init(p%dir,dir)
         call init(p%name,name)
         p%TF_freshStart = TF_freshStart
         if (p%TF_freshStart) then
           p%un_d = newAndOpen(dir,name)
           write(p%un_d,*) 'TITLE = "probe for '//name//'"'
           write(p%un_d,*) 'VARIABLES = t,'//name//',N'
           write(p%un_d,*) 'ZONE DATAPACKING = POINT'
           flush(p%un_d)
         elseif (.not.p%TF_freshStart) then
           p%un_d = openToAppend(dir,name)
         else; stop 'Error: no case found in initProbe in probe_transient.f90'
         endif
         p%TF_freshStart = .false.
         p%infinity = huge(1.0_cp)
       end subroutine

       subroutine setProbeData(p,n,t,d)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: n
         real(cp),intent(in) :: t,d
         p%n = n; p%t = t; p%d = d
       end subroutine

       subroutine applyProbe(p)
         implicit none
         type(probe),intent(inout) :: p
         if (p%d.gt.p%infinity) then
         write(*,*) 'Error: data>infinity in probe: ',str(p%name)
         stop 'Divergence error. Sorry!'
         endif
         if (is_nan(p%d)) then
         write(*,*) 'Error: NaN in data in probe: ',str(p%name)
         stop 'Divergence error. Sorry!'
         endif

         write(p%un_d,'(3'//arrfmt//')') p%t,p%d,real(p%n,cp)
         flush(p%un_d)
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
           call export_TP(p,6)
         else
           call export_TP(p,6)
         endif
       end subroutine

       subroutine export_TP_info(p,u)
         implicit none
         type(probe), intent(in) :: p
         integer,intent(in),optional :: u
         integer :: newU
         if (.not.present(u)) then
           newU = newAndOpen(str(p%dir),str(p%name)//'_info')
           write(newU,*) ' ---------------- PROBE -------------- '
         else; newU = u
         endif

         call export_TP(p,newU)

         if (.not.present(u)) then
           call closeAndMessage(newU,str(p%name)//'_info',str(p%dir))
         endif
       end subroutine

       subroutine export_TP(p,u)
         implicit none
         type(probe), intent(in) :: p
         integer,intent(in) :: u
         write(u,*) ' directory = ',str(p%dir)
         write(u,*) ' name = ',str(p%name)
       end subroutine

       end module