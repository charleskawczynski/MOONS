       module probe_extend_mod
       use probe_mod
       ! Implementation:
       !       type(probe) :: p
       !       call init(p,tec_dir,tec_name)
       !       call set_names(p,(/new('t'),&
       !                          new('B^2'),&
       !                          new('t')/))
       !       do i=1,1000
       !         call update(p,n,t,d)      ! sets data to be exported
       !         call export(p)            ! exports data
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
       public :: init
       public :: set_var_names
       public :: export
       public :: update

       interface init;          module procedure init_probe;          end interface
       interface set_var_names; module procedure set_var_names_probe; end interface
       interface update;        module procedure update_data_probe;   end interface
       interface export;        module procedure export_data_probe;   end interface

       contains

       subroutine init_probe(p,tec_dir,tec_name)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: tec_dir,tec_name
         call init(p%tec_dir,tec_dir)
         call init(p%tec_name,tec_name)
         p%un = new_and_open(tec_dir,tec_name)
         write(p%un,*) 'TITLE = "'//tec_name//' probe"'
         flush(p%un)
       end subroutine

       subroutine set_var_names(p,var_names)
         implicit none
         type(probe),intent(inout) :: p
         type(string),dimension(:),intent(in) :: var_names
         type(string) :: s
         integer :: i
         call init(s,'VARIABLES = ')
         p%cols = size(var_names)
         if (.not.(p%cols.gt.0)) then
           write(*,*) 'Error: p%cols < 1 in set_var_names in probe_extend.f90'
           stop 'Done in probe_extend.f90'
         endif
         do i=1,p%cols-1
           call append(s,var_names(i)//',')
         endif
         call append(s,var_names(p%cols))
         write(p%un,*) str(s)
         call delete(s)
         write(p%un,*) 'ZONE DATAPACKING = POINT'
         flush(p%un)
       end subroutine

       subroutine update_data_probe(p,d)
         implicit none
         type(probe),intent(inout) :: p
         real(cp),dimension(p%cols),intent(in) :: d
         if (abs(d(1)-p%d(1)).lt.machine_epsilon) then
          write(*,*) 'Error: cannot export probe '//str(p%tec_name)//' consecutively.'
          write(*,*) 'd = ',d
          write(*,*) 'p%d = ',p%d
          stop 'Done'
         endif
         p%d = d
         call check_nans_probe(p)
       end subroutine

       subroutine export_data_probe(p)
         implicit none
         type(probe),intent(in) :: p
         write(p%un,*) p%d
         flush(p%un)
       end subroutine

       subroutine check_nans_probe(p)
         implicit none
         type(probe),intent(in) :: p
         integer :: i
         do i=1,p%cols
           if (is_nan(p%d(i))) then
             write(*,*) 'Error: NaN in data in probe: ',str(p%tec_name)
             write(*,*) 'data(i) = ',p%d(i)
             write(*,*) 'data = ',p%d
             stop 'Divergence error. Sorry!'
           endif
         enddo
       end subroutine

       subroutine init_probe_restart(p,TMP)
         implicit none
         type(probe),intent(inout) :: p
         type(time_marching_params),intent(in) :: TMP
         integer(li) :: n_lines
         p%restart = .true.
         p%restart = file_exists(dir,name)
         call truncate_data_in_open_file(p,TMP,i_last)
         p%un = open_to_append(str(p%tec_dir),str(p%tec_name))
         call export(p) ! So that restart is passed to next sim.
       end subroutine

       end module