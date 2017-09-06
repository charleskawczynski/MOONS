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
         type(string) :: dir,name        ! directory and name
         real(cp) :: d = 0.0_cp          ! data
         real(cp) :: d_data_dt = 0.0_cp  ! change in data over time (Euler)
         real(cp) :: d_amax = 0.0_cp     ! max(d) over time
         real(cp) :: t = 0.0_cp          ! time
         integer :: un = 0               ! file unit
         integer :: cols = 0             ! columns of data
         integer(li) :: n_step = 0       ! time step
         logical :: restart = .false.    ! restart probe (append existing)
         logical :: simple = .false.     ! simple probe (only data)
       end type

       interface init;     module procedure init_probe;               end interface
       interface delete;   module procedure delete_probe;             end interface
       interface delete;   module procedure delete_probe_many;        end interface
       interface export;   module procedure export_probe;             end interface
       interface export;   module procedure export_probe_wrapper;     end interface
       interface export;   module procedure export_probe_wrapper_dim; end interface
       interface import;   module procedure import_probe;             end interface
       interface import;   module procedure import_probe_wrapper;     end interface
       interface import;   module procedure import_probe_wrapper_dim; end interface

       interface export;   module procedure export_probe_data;        end interface
       interface get_data; module procedure get_data_probe;           end interface

       contains

       subroutine init_probe(p,dir,name,restart,simple,TMP)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir,name
         logical,intent(in) :: restart,simple
         type(time_marching_params),intent(in) :: TMP
         integer :: i_last
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
           p%cols = 2
           if (.not.p%simple) then
             p%cols = p%cols+1;call append(s,',d('//name//')/dt')
             p%cols = p%cols+1;call append(s,',|d('//name//')/dt|')
             p%cols = p%cols+1;call append(s,',|d('//name//')/dt|/max(d)_used')
             p%cols = p%cols+1;call append(s,',max(d)')
           endif
           write(p%un,*) str(s)
           call delete(s)
           write(p%un,*) 'ZONE DATAPACKING = POINT'
           flush(p%un)
           p%n_step = 0
           p%d_amax = 0.0_cp
         elseif (p%restart) then
           ! call truncate_data_in_open_file(p,TMP,dir,name,i_last)
           i_last = get_last_data_point_location(dir,name)
           ! p%un = open_to_append(dir,name,i_last) ! Does not work yet.
           p%un = open_to_append(dir,name)
         else; stop 'Error: no case found in init_probe in probe.f90'
         endif
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
         p%cols = 0
         p%restart = .false.
         p%simple = .false.
       end subroutine

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call delete(p(i)); enddo
         endif
       end subroutine

       subroutine export_probe(p,un)
         implicit none
         type(probe),intent(in) :: p
         integer,intent(in) :: un
         call export(p%dir,un)
         call export(p%name,un)
         write(un,*) p%d
         write(un,*) p%d_data_dt
         write(un,*) p%d_amax
         write(un,*) p%t
         write(un,*) p%un
         write(un,*) p%cols
         write(un,*) p%n_step
         write(un,*) p%restart
         write(un,*) p%simple
       end subroutine

       subroutine import_probe(p,un)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: un
         call import(p%dir,un)
         call import(p%name,un)
         read(un,*) p%d
         read(un,*) p%d_data_dt
         read(un,*) p%d_amax
         read(un,*) p%t
         read(un,*) p%un
         read(un,*) p%cols
         read(un,*) p%n_step
         read(un,*) p%restart
         read(un,*) p%simple
       end subroutine

       subroutine export_probe_wrapper(p,dir,name)
         implicit none
         type(probe),intent(in) :: p
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(p,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine export_probe_wrapper_dim(p,dir,name)
         implicit none
         type(probe),dimension(:),intent(in) :: p
         character(len=*),intent(in) :: dir,name
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call export(p(i),dir,name); enddo
         endif
       end subroutine

       subroutine import_probe_wrapper(p,dir,name)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(p,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_probe_wrapper_dim(p,dir,name)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         character(len=*),intent(in) :: dir,name
         integer :: i
         if (size(p).gt.0) then
         do i=1,size(p); call import(p(i),dir,name); enddo
         endif
       end subroutine

       subroutine export_probe_data(p,TMP,d)
         implicit none
         type(probe),intent(inout) :: p
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: d
         real(cp) :: abs_d_data_dt,abs_d_data_dt_by_dmax
         ! Breaks if double exported data (TMP%t = p%t)
         p%d_data_dt = (d - p%d)/(TMP%t - p%t)
         if (TMP%n_step.eq.0) p%d_data_dt = 0.0_cp
         if (p%t.lt.10.0_cp**(-10.0_cp)) p%d_data_dt = 0.0_cp
         abs_d_data_dt = abs(p%d_data_dt)
         p%t = TMP%t
         p%d = d
         p%d_amax = maxval((/p%d_amax,p%d,abs(d)/))
         abs_d_data_dt_by_dmax = abs_d_data_dt/p%d_amax
         if (p%d_amax.lt.10.0_cp**(-10.0_cp)) abs_d_data_dt_by_dmax = abs_d_data_dt
         if (p%n_step.eq.TMP%n_step.and.(TMP%n_step.gt.0)) then
          write(*,*) 'Error: cannot export probe '//str(p%name)//' consecutively.'
          write(*,*) 'TMP%n_step = ',TMP%n_step
          write(*,*) 'p%n_step = ',p%n_step
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

       subroutine truncate_data_in_open_file(p,TMP,dir,name,i_last)
         implicit none
         type(probe),intent(in) :: p
         type(time_marching_params),intent(in) :: TMP
         character(len=*),intent(in) :: dir,name
         integer,intent(inout) :: i_last
         real(cp),dimension(:),allocatable :: d
         integer(li) :: i,i_first_to_delete,i_EOF
         integer :: un,stat
         un = open_to_read(dir,name)
         read(un,*); read(un,*); read(un,*)
         allocate(d(p%cols))
         i_first_to_delete = 1
         i_EOF = 1
         do i=1,TMP%n_step
           read(un,*,iostat=stat) d
           if (TMP%t.gt.d(1)) then; i_first_to_delete = i; endif
           if (stat .lt. 0) then; i_EOF = i; exit; endif
         enddo
         close(un)
         un = open_to_read_write(dir,name)
         read(un,*); read(un,*); read(un,*)
         if (i_first_to_delete.ne.TMP%n_step) then
           do i=1,i_first_to_delete; read(un,*); enddo
           do i=i_first_to_delete+1,i_EOF; write(un,*) ''; enddo
         endif
         deallocate(d)
         close(un)
         i_last = int(i_first_to_delete)+3
       end subroutine

       function get_last_data_point_location(dir,name) result(i_last)
         implicit none
         character(len=*),intent(in) :: dir,name
         integer(li) :: i,i_EOF
         integer :: i_last
         integer :: un,stat
         logical :: L
         un = open_to_read(dir,name)
         i_EOF = 1
         i = 1
         L = .true.
         do while (L)
           read(un,*,iostat=stat)
           i_EOF = i
           L = .not.(stat .lt. 0)
           i=i+1
         enddo
         close(un)
         i_last = int(i_EOF)
       end function

       end module