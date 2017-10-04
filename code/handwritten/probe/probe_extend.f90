       module probe_extend_mod
       use probe_mod
       ! Implementation:
       !       type(probe) :: p
       !       call init(p,tec_dir,tec_name,restart)
       !       do i=1,1000
       !         call export(p,n,t,d)      ! sets data to be exported
       !       enddo
       use current_precision_mod
       use string_mod
       use array_mod
       use IO_tools_mod
       use sim_params_mod
       use time_marching_params_mod
       use is_nan_mod

       implicit none
       private
       public :: init,delete,export,import
       public :: get_data

       interface init;     module procedure init_probe;               end interface
       interface delete;   module procedure delete_probe_many;        end interface
       interface export;   module procedure export_probe_wrapper_dim; end interface
       interface import;   module procedure import_probe_wrapper_dim; end interface

       interface export;   module procedure export_probe_data;        end interface
       interface get_data; module procedure get_data_probe;           end interface

       contains

       subroutine init_probe(p,tec_dir,tec_name,restart,simple,TMP)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: tec_dir,tec_name
         logical,intent(in) :: restart,simple
         type(time_marching_params),intent(in) :: TMP
         call delete(p)
         if (restart) then
           call import(p)
           call init_probe_restart(p,TMP)
         else
           call init_probe_fresh(p,tec_dir,tec_name,simple)
         endif
       end subroutine

       subroutine init_probe_fresh(p,tec_dir,tec_name,simple)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: tec_dir,tec_name
         logical,intent(in) :: simple
         type(string) :: s
         call init(p%tec_dir,tec_dir)
         call init(p%tec_name,tec_name)
         p%simple = simple
         p%restart = .false.
         p%un = new_and_open(tec_dir,tec_name)
         write(p%un,*) 'TITLE = "'//tec_name//' probe"'
         call init(s,'VARIABLES = t')
         call append(s,','//tec_name)
         p%cols = 2
         if (.not.p%simple) then
           p%cols = p%cols+1;call append(s,',d('//tec_name//')/dt')
           p%cols = p%cols+1;call append(s,',|d('//tec_name//')/dt|')
           p%cols = p%cols+1;call append(s,',|d('//tec_name//')/dt|/max(d)_used')
           p%cols = p%cols+1;call append(s,',max(d)')
         endif
         write(p%un,*) str(s)
         call delete(s)
         write(p%un,*) 'ZONE DATAPACKING = POINT'
         flush(p%un)
         p%n_step = 0
         p%d_amax = 0.0_cp
       end subroutine

       subroutine init_probe_restart(p,TMP)
         implicit none
         type(probe),intent(inout) :: p
         type(time_marching_params),intent(in) :: TMP
         integer :: i_last
         p%restart = .true.
         call truncate_data_in_open_file(p,TMP,i_last)
         i_last = get_last_data_point_location(str(p%tec_dir),str(p%tec_name))
         ! p%un = open_to_append(p%tec_dir,p%tec_name,i_last) ! Does not work yet.
         p%un = open_to_append(str(p%tec_dir),str(p%tec_name))
         call export(p) ! So that restart is passed to next sim.
       end subroutine

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call delete(p(i)); enddo
         endif
       end subroutine

       subroutine export_probe_wrapper_dim(p,tec_dir,tec_name)
         implicit none
         type(probe),dimension(:),intent(in) :: p
         character(len=*),intent(in) :: tec_dir,tec_name
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call export(p(i),tec_dir,tec_name); enddo
         endif
       end subroutine

       subroutine import_probe_wrapper_dim(p,tec_dir,tec_name)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         character(len=*),intent(in) :: tec_dir,tec_name
         integer :: i
         if (size(p).gt.0) then
         do i=1,size(p); call import(p(i),tec_dir,tec_name); enddo
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
          write(*,*) 'Error: cannot export probe '//str(p%tec_name)//' consecutively.'
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
           write(*,*) 'Error: NaN in data in probe: ',str(p%tec_name)
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

       subroutine truncate_data_in_open_file(p,TMP,i_last)
         implicit none
         type(probe),intent(in) :: p
         type(time_marching_params),intent(in) :: TMP
         integer,intent(inout) :: i_last
         real(cp),dimension(:),allocatable :: d
         integer(li) :: i,i_first_to_delete,i_EOF
         integer :: un,stat
         un = open_to_read(str(p%tec_dir),str(p%tec_name))
         write(*,*) 'truncate_data_in_open_file: '
         call print(p%tec_dir)
         call print(p%tec_name)
         write(*,*) 'p%cols = ',p%cols
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
         un = open_to_read_write(str(p%tec_dir),str(p%tec_name))
         read(un,*); read(un,*); read(un,*)
         if (i_first_to_delete.ne.TMP%n_step) then
           do i=1,i_first_to_delete; read(un,*); enddo
           do i=i_first_to_delete+1,i_EOF; write(un,*) ''; enddo
         endif
         deallocate(d)
         close(un)
         i_last = int(i_first_to_delete)+3
       end subroutine

       function get_last_data_point_location(tec_dir,tec_name) result(i_last)
         implicit none
         character(len=*),intent(in) :: tec_dir,tec_name
         integer(li) :: i,i_EOF
         integer :: i_last
         integer :: un,stat
         logical :: L
         un = open_to_read(tec_dir,tec_name)
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