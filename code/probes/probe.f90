       module probe_mod
       ! Implementation:
       ! 
       !       type(probe) :: p
       !       call init(p,dir,name,restart)
       ! 
       !       do i=1,1000
       !         call export(p,n,t,d)                        ! sets data to be exported
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

       type probe
         type(string) :: dir,name             ! directory and name
         real(cp) :: d                        ! data
         real(cp) :: t                        ! time
         integer :: un                        ! file unit
         logical :: restart                   ! restart probe (append existing)
         real(cp) :: NaN,infinity             ! for checking divergent data
       end type

       interface init;        module procedure init_probe;           end interface
       interface delete;      module procedure delete_probe;         end interface
       interface delete;      module procedure delete_probe_many;    end interface
       interface export;      module procedure export_probe;         end interface

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
           write(p%un,*) 'TITLE = "'//name//' probe"'
           write(p%un,*) 'VARIABLES = t,'//name
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

       subroutine export_probe(p,t,d)
         implicit none
         type(probe),intent(inout) :: p
         real(cp),intent(in) :: t,d
         p%t = t; p%d = d
         if (p%d.gt.p%infinity) then
         write(*,*) 'Error: data>infinity in probe: ',str(p%name)
         stop 'Divergence error. Sorry!'
         endif
         if (is_nan(p%d)) then
         write(*,*) 'Error: NaN in data in probe: ',str(p%name)
         stop 'Divergence error. Sorry!'
         endif
         write(p%un,'(2'//arrfmt//')') p%t,p%d
         flush(p%un)
       end subroutine

       end module