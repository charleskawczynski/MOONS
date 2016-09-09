       module probe_mod
       ! Implementation:
       ! 
       !       type(probe) :: p
       !       call init(p,dir,name,restart)                    ! enables print/export
       ! 
       !       do i=1,1000
       !         call set(p,n,d)                                ! sets data to be exported
       !         call export(p)                                 ! exports transient data (n,d)
       !       enddo
       ! 
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       use fmt_mod
       use is_nan_mod

       implicit none

       private
       public :: probe
       public :: init,delete,export
       public :: set

       type probe
         real(cp) :: d                        ! transient data
         real(cp) :: t                        ! t associated with data
         integer :: n                         ! n associated with data
         type(string) :: dir,name             ! probe directory and name
         integer :: un                        ! file unit number
         logical :: restart                   ! restart probe and continue appending
         real(cp) :: NaN,infinity             ! for checking divergent data
       end type

       interface init;        module procedure init_probe;           end interface
       interface delete;      module procedure delete_probe;         end interface
       interface delete;      module procedure delete_probe_many;    end interface
       interface export;      module procedure export_probe;         end interface

       interface set;         module procedure set_probe;            end interface

       contains

       subroutine init_probe(p,dir,name,restart)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: restart
         call init(p%dir,dir)
         call init(p%name,name)
         p%restart = restart
         if (.not.p%restart) then
           p%un = new_and_open(dir,name)
           write(p%un,*) 'TITLE = "probe for '//name//'"'
           write(p%un,*) 'VARIABLES = t,'//name//',N'
           write(p%un,*) 'ZONE DATAPACKING = POINT'
           flush(p%un)
         elseif (p%restart) then
           p%un = open_to_append(dir,name)
         else; stop 'Error: no case found in init_probe in probe.f90'
         endif
         p%restart = .true.
         p%infinity = huge(1.0_cp)
       end subroutine

       subroutine delete_probe(p)
         implicit none
         type(probe),intent(inout) :: p
         call delete(p%dir)
         call delete(p%name)
         close(p%un)
       end subroutine

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i,s
         s = size(p)
         if (s.gt.0) then; do i=1,s; call delete(p(i)); enddo; endif
       end subroutine

       subroutine export_probe(p)
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

       subroutine set_probe(p,n,t,d)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: n
         real(cp),intent(in) :: t,d
         p%n = n; p%t = t; p%d = d
       end subroutine

       end module