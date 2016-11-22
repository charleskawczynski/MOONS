       module time_marching_params_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: time_marching_params
       public :: init,delete,export,import,display,print
       public :: iterate_step
       public :: couple_time_step
       public :: prolongate

       type time_marching_params
         integer(li) :: n_step        ! nth time step
         integer(li) :: n_step_stop   ! nth time step to stop
         integer(li) :: n_step_start  ! nth time step to start
         real(cp) :: t                ! time, or pseudo time
         real(cp) :: dt               ! time step, or pseudo time step
         integer :: un                ! file unit
         type(string) :: dir,name     ! directory / name
       end type

       interface init;             module procedure init_TMP;             end interface
       interface init;             module procedure init_copy_TMP;        end interface
       interface delete;           module procedure delete_TMP;           end interface
       interface export;           module procedure export_TMP;           end interface
       interface import;           module procedure import_TMP;           end interface
       interface display;          module procedure display_TMP;          end interface
       interface print;            module procedure print_TMP;            end interface
       interface iterate_step;     module procedure iterate_step_TMP;     end interface

       interface couple_time_step; module procedure couple_time_step_TMP; end interface

       interface prolongate;       module procedure prolongate_TMP;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_TMP(TMP,n_step_stop,dt,dir,name)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer(li),intent(in) :: n_step_stop
         real(cp),intent(in) :: dt
         character(len=*),intent(in) :: dir,name
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%n_step_stop = n_step_stop
         TMP%t = 0.0_cp
         TMP%dt = dt
         call init(TMP%dir,dir)
         call init(TMP%name,name)
       end subroutine

       subroutine init_copy_TMP(TMP_out,TMP_in)
         implicit none
         type(time_marching_params),intent(inout) :: TMP_out
         type(time_marching_params),intent(in) :: TMP_in
         TMP_out%n_step_start = TMP_in%n_step_start
         TMP_out%n_step = TMP_in%n_step
         TMP_out%n_step_stop = TMP_in%n_step_stop
         TMP_out%t = TMP_in%t
         TMP_out%dt = TMP_in%dt
         call init(TMP_out%dir,TMP_in%dir)
         call init(TMP_out%name,TMP_in%name)
         TMP_out%un = TMP_in%un
       end subroutine

       subroutine delete_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         TMP%n_step_start = 0
         TMP%n_step = 0
         TMP%n_step_stop = 1
         TMP%t = 0.0_cp
         TMP%dt = 10.0_cp**(-10.0_cp)
         call delete(TMP%dir)
         call delete(TMP%name)
         TMP%un = 0
       end subroutine

       subroutine export_TMP(TMP)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         integer :: un
         un = new_and_open(str(TMP%dir),str(TMP%name))
         write(un,*) 'n_step_start = ';write(un,*) TMP%n_step_start
         write(un,*) 'n_step = ';      write(un,*) TMP%n_step
         write(un,*) 'n_step_stop = '; write(un,*) TMP%n_step_stop
         write(un,*) 't = ';           write(un,*) TMP%t
         write(un,*) 'dt = ';          write(un,*) TMP%dt
         call close_and_message(un,str(TMP%dir),str(TMP%name))
       end subroutine

       subroutine import_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer :: un
         un = open_to_read(str(TMP%dir),str(TMP%name))
         read(un,*); read(un,*) TMP%n_step_start
         read(un,*); read(un,*) TMP%n_step
         read(un,*); read(un,*) TMP%n_step_stop
         read(un,*); read(un,*) TMP%t
         read(un,*); read(un,*) TMP%dt
         call close_and_message(un,str(TMP%dir),str(TMP%name))
       end subroutine

       subroutine display_TMP(TMP,un)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         integer,intent(in) :: un
         call display(TMP%dir,un)
         call display(TMP%name,un)
         write(un,*) 'n_step_start = ',TMP%n_step_start
         write(un,*) 'n_step = ',TMP%n_step
         write(un,*) 'n_step_stop = ',TMP%n_step_stop
         write(un,*) 't = ',TMP%t
         write(un,*) 'dt = ',TMP%dt
       end subroutine

       subroutine print_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         call display(TMP,6)
       end subroutine

       subroutine iterate_step_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         TMP%n_step = TMP%n_step + 1
         TMP%t = TMP%t + TMP%dt
       end subroutine

       subroutine couple_time_step_TMP(TMP,coupled)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         type(time_marching_params),intent(in) :: coupled
         TMP%t = coupled%t
         TMP%dt = coupled%dt
         TMP%n_step_start = coupled%n_step_start
         TMP%n_step_stop = coupled%n_step_stop
         TMP%n_step = coupled%n_step
       end subroutine

       subroutine prolongate_TMP(TMP)
         implicit none
         type(time_marching_params),intent(inout) :: TMP
         integer :: factor
         factor = 4                               ! to address dt restriction: dt ~ 1/dx^2
         TMP%dt = TMP%dt*1.0_cp/(real(factor,cp)) ! reduce dt for finer mesh
         TMP%n_step_stop = TMP%n_step_stop*factor ! increase n_step_stop to reach final time
       end subroutine

       end module