       module probe_mod
       ! Implementation:
       !       type(probe) :: p
       !       call init(p,dir,name,restart)
       !       do i=1,1000
       !         call export(p,n,t,d)      ! sets data to be exported
       !       enddo
       use current_precision_mod
       use string_mod
       use array_mod
       use IO_tools_mod
       use sim_params_mod
       use fmt_mod
       use time_marching_params_mod
       use is_nan_mod

       implicit none

       private
       public :: probe
       public :: init,delete,export,import
       public :: get_data

       type probe
         private
         type(string) :: dir,name                              ! directory and name
         real(cp) :: d = 0.0_cp                                ! data
         real(cp) :: d_data_dt = 0.0_cp                        ! change in data over time (Euler)
         real(cp) :: d_amax = 0.0_cp                           ! max(d) over time
         real(cp) :: t = 0.0_cp                                ! time
         integer :: un = 0                                     ! file unit
         integer(li) :: n_step = 0                             ! time step
         logical :: restart = .false.                          ! restart probe (append existing)
         logical :: simple = .false.                           ! simple probe (only data)
       end type

       interface init;          module procedure init_probe;           end interface
       interface delete;        module procedure delete_probe;         end interface
       interface delete;        module procedure delete_probe_many;    end interface
       interface export;        module procedure export_probe;         end interface
       interface import;        module procedure import_probe;         end interface

       interface export;        module procedure export_probe_data;    end interface
       interface get_data;      module procedure get_data_probe;       end interface

       contains

       subroutine init_probe(p,dir,name,restart,simple)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: restart,simple
         type(string) :: s
         call delete(p)
         call init(p%dir,dir)
         call init(p%name,name)
         p%restart = restart
         p%simple = simple
         if (.not.p%restart) then
           p%un = new_and_open(dir,name)
           write(p%un,*) 'TITLE = "'//name//' probe"'
           call init(s,'VARIABLES = t')
           call append(s,','//name)
           if (.not.p%simple) call append(s,',d('//name//')/dt')
           if (.not.p%simple) call append(s,',|d('//name//')/dt|')
           if (.not.p%simple) call append(s,',|d('//name//')/dt|/max(d)_used')
           if (.not.p%simple) call append(s,',max(d)')
           write(p%un,*) str(s)
           call delete(s)
           write(p%un,*) 'ZONE DATAPACKING = POINT'
           flush(p%un)
         elseif (p%restart) then
           p%un = open_to_append(dir,name)
           ! call import(p,dir,name) ! obviously need this.
         else; stop 'Error: no case found in init_probe in probe.f90'
         endif
         p%n_step = 0
         p%d_amax = 0.0_cp
       end subroutine

       subroutine delete_probe(p)
         implicit none
         type(probe),intent(inout) :: p
         call delete(p%dir)
         call delete(p%name)
         close(p%un)
         p%n_step = 0
         p%d_amax = 0.0_cp
         p%d = 0.0_cp
         p%d_data_dt = 0.0_cp
         p%t = 0.0_cp
         p%un = 0
         p%restart = .false.
         p%simple = .false.
       end subroutine

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i,s
         s = size(p)
         if (s.gt.0) then; do i=1,s; call delete(p(i)); enddo; endif
       end subroutine

       subroutine export_probe(p,un)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: un
         call export(p%dir,un)
         call export(p%name,un)
       end subroutine

       subroutine import_probe(p,un)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: un
         call import(p%dir,un)
         call import(p%name,un)
       end subroutine

       subroutine export_probe_data(p,TMP,d)
         implicit none
         type(probe),intent(inout) :: p
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: d
         real(cp) :: abs_d_data_dt,abs_d_data_dt_by_dmax
         ! Breaks if double exported data (TMP%t = p%t)
         p%d_data_dt = (d - p%d)/(TMP%t - p%t)
         if (p%t.lt.10.0_cp**(-10.0_cp)) p%d_data_dt = 0.0_cp
         abs_d_data_dt = abs(p%d_data_dt)
         p%t = TMP%t
         p%d = d
         p%d_amax = maxval((/p%d_amax,p%d,abs(d)/))
         abs_d_data_dt_by_dmax = abs_d_data_dt/p%d_amax
         if (p%d_amax.lt.10.0_cp**(-10.0_cp)) abs_d_data_dt_by_dmax = abs_d_data_dt

         if (p%n_step.eq.TMP%n_step) then
          write(*,*) 'Error: cannot export probe '//str(p%name)//' consecutively.'
          stop 'Done'
         endif
         p%n_step = TMP%n_step
         call check_nans_probe(p)
         if (.not.p%simple) then
         write(p%un,*) p%t,p%d,p%d_data_dt,abs_d_data_dt,abs_d_data_dt_by_dmax,p%d_amax
         else; write(p%un,*) p%t,p%d
         endif
         flush(p%un)
       end subroutine

       subroutine check_nans_probe(p)
         implicit none
         type(probe),intent(in) :: p
         if (is_nan(p%d)) then
           write(*,*) 'Error: NaN in data in probe: ',str(p%name)
           write(*,*) 'data = ',p%d
           stop 'Divergence error. Sorry!'
         endif
       end subroutine

       function get_data_probe(p) result(d)
         implicit none
         type(probe),intent(in) :: p
         real(cp) :: d
         d = p%d
       end function

       end module