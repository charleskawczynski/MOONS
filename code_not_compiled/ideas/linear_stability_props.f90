       module linear_stability_analysis_params_mod
       use current_precision_mod
       use time_marching_params_mod

       implicit none
       private

       public :: linear_stability_analysis_params
       public :: init,delete,display,print,export,import ! Essentials

       public :: update

       type linear_stability_analysis_params
         logical :: perform = .false.
         logical :: start_now = .false.
         real(cp) :: t_start = 0.0_cp
         real(cp) :: TGR = 0.0_cp
         real(cp) :: TGR_sum = 0.0_cp
       end type

       interface init;    module procedure init_LSAP;      end interface
       interface init;    module procedure init_copy_LSAP; end interface
       interface delete;  module procedure delete_LSAP;    end interface
       interface display; module procedure display_LSAP;   end interface
       interface print;   module procedure print_LSAP;     end interface
       interface export;  module procedure export_LSAP;    end interface
       interface import;  module procedure import_LSAP;    end interface

       interface update;  module procedure update_LSAP;    end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_LSAP(LSAP,perform,t_start)
         implicit none
         type(linear_stability_analysis_params),intent(inout) :: LSAP
         logical,intent(in) :: perform
         real(cp),intent(in) :: t_start
         call delete(LSAP)
         LSAP%perform = perform
         LSAP%start_now = .false.
         LSAP%t_start = t_start
         LSAP%TGR = 0.0_cp
         LSAP%TGR_sum = 0.0_cp
       end subroutine

       subroutine init_copy_LSAP(LSAP,LSAP_in)
         implicit none
         type(linear_stability_analysis_params),intent(inout) :: LSAP
         type(linear_stability_analysis_params),intent(in) :: LSAP_in
         LSAP%perform = LSAP_in%perform
         LSAP%start_now = LSAP_in%start_now
         LSAP%t_start = LSAP_in%t_start
         LSAP%TGR = LSAP_in%TGR
         LSAP%TGR_sum = LSAP_in%TGR_sum
       end subroutine

       subroutine delete_LSAP(LSAP)
         implicit none
         type(linear_stability_analysis_params),intent(inout) :: LSAP
         LSAP%perform = .false.
         LSAP%start_now = .false.
         LSAP%t_start = 0.0_cp
         LSAP%TGR = 0.0_cp
         LSAP%TGR_sum = 0.0_cp
       end subroutine

       subroutine display_LSAP(LSAP,un)
         implicit none
         type(linear_stability_analysis_params),intent(in) :: LSAP
         integer,intent(in) :: un
         write(un,*) ' perform = ',LSAP%perform
         write(un,*) ' start_now = ',LSAP%start_now
         write(un,*) ' t_start = ',LSAP%t_start
         write(un,*) ' TGR = ',LSAP%TGR
         write(un,*) ' TGR_sum = ',LSAP%TGR_sum
       end subroutine

       subroutine print_LSAP(LSAP)
         implicit none
         type(linear_stability_analysis_params),intent(in) :: LSAP
         call display(LSAP,6)
       end subroutine

       subroutine export_LSAP(LSAP,un)
         implicit none
         type(linear_stability_analysis_params),intent(in) :: LSAP
         integer,intent(in) :: un
         write(un,*) ' perform = '; write(un,*) LSAP%perform
         write(un,*) ' start_now = '; write(un,*) LSAP%start_now
         write(un,*) ' t_start = '; write(un,*) LSAP%t_start
         write(un,*) ' TGR = '; write(un,*) LSAP%TGR
         write(un,*) ' TGR_sum = '; write(un,*) LSAP%TGR_sum
       end subroutine

       subroutine import_LSAP(LSAP,un)
         implicit none
         type(linear_stability_analysis_params),intent(inout) :: LSAP
         integer,intent(in) :: un
         read(un,*); read(un,*) LSAP%perform
         read(un,*); read(un,*) LSAP%start_now
         read(un,*); read(un,*) LSAP%t_start
         read(un,*); read(un,*) LSAP%TGR
         read(un,*); read(un,*) LSAP%TGR_sum
       end subroutine

       subroutine update_LSAP(LSAP,TMP)
         implicit none
         type(linear_stability_analysis_params),intent(inout) :: LSAP
         type(time_marching_params),intent(in) :: TMP
         logical,dimension(2) :: L
         L(1) = TMP%t.gt.LSAP%t_start
         L(2) = LSAP%perform
         SP%start_now = all(L)
       end subroutine

       end module