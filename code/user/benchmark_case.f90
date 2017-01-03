       module benchmark_case_mod
       use current_precision_mod
       use var_set_mod
       use string_mod
       implicit none

       private
       public :: benchmark_case
       public :: init,delete,export,import,display,print

       type benchmark_case
         type(var_set) :: VS
         integer :: geometry = 0
         integer,dimension(3) :: periodic_dir = 0
         real(cp) :: cw = 0.0_cp
       end type

       interface init;      module procedure init_BMC;      end interface
       interface init;      module procedure init_copy_BMC; end interface
       interface delete;    module procedure delete_BMC;    end interface
       interface export;    module procedure export_BMC;    end interface
       interface import;    module procedure import_BMC;    end interface
       interface display;   module procedure display_BMC;   end interface
       interface print;     module procedure print_BMC;     end interface

       contains

       ! **********************************************************
       ! ********************* EBMCENTIALS *************************
       ! **********************************************************

       subroutine init_BMC(BMC,VS,cw,geometry,periodic_dir)
         implicit none
         type(benchmark_case),intent(inout) :: BMC
         type(var_set),intent(in) :: VS
         real(cp),intent(in) :: cw
         integer,intent(in) :: geometry
         integer,dimension(3),intent(in) :: periodic_dir
         call init(BMC%VS,VS)
         BMC%cw = cw
         BMC%geometry = geometry
         BMC%periodic_dir = periodic_dir
       end subroutine

       subroutine init_copy_BMC(BMC,BMC_in)
         implicit none
         type(benchmark_case),intent(inout) :: BMC
         type(benchmark_case),intent(in) :: BMC_in
         call delete(BMC)
         call init(BMC%VS,BMC_in%VS)
         BMC%geometry = BMC_in%geometry
         BMC%cw = BMC_in%cw
         BMC%periodic_dir = BMC_in%periodic_dir
       end subroutine

       subroutine delete_BMC(BMC)
         implicit none
         type(benchmark_case),intent(inout) :: BMC
         call delete(BMC%VS)
         BMC%cw = 0
         BMC%geometry = 0
         BMC%periodic_dir = 0
       end subroutine

       subroutine export_BMC(BMC,un)
         implicit none
         type(benchmark_case),intent(in) :: BMC
         integer,intent(in) :: un
         call export(BMC%VS,un)
         write(un,*) BMC%cw
         write(un,*) BMC%geometry
         write(un,*) BMC%periodic_dir
       end subroutine

       subroutine import_BMC(BMC,un)
         implicit none
         type(benchmark_case),intent(inout) :: BMC
         integer,intent(in) :: un
         call import(BMC%VS,un)
         read(un,*) BMC%cw
         read(un,*) BMC%geometry
         read(un,*) BMC%periodic_dir
       end subroutine

       subroutine display_BMC(BMC,un)
         implicit none
         type(benchmark_case),intent(in) :: BMC
         integer,intent(in) :: un
         call display(BMC%VS,un)
         write(un,*) 'cw = ',BMC%cw
         write(un,*) 'geometry = ',BMC%geometry
         write(un,*) 'periodic_dir = ',BMC%periodic_dir
       end subroutine

       subroutine print_BMC(BMC)
         implicit none
         type(benchmark_case),intent(inout) :: BMC
         call display(BMC,6)
       end subroutine

       end module