       module physical_sub_domain_mod
       use current_precision_mod
       use overlap_mod
       use overlap_extend_mod
       use grid_mod
       use face_edge_corner_indexing_mod
       use sub_domain_mod
       use sub_domain_extend_mod
       use data_location_mod
       use coordinates_mod

       implicit none

       private
       public :: physical_sub_domain
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_mixed

       public :: pick_extrema_bot
       public :: pick_extrema_top

       interface init;             module procedure init_PSD;              end interface
       interface init;             module procedure init_copy_PSD;         end interface
       interface delete;           module procedure delete_PSD;            end interface
       interface display;          module procedure display_PSD;           end interface
       interface print;            module procedure print_PSD;             end interface
       interface export;           module procedure export_PSD;            end interface
       interface import;           module procedure import_PSD;            end interface

       interface init_mixed;       module procedure init_mixed_PSD;        end interface

       interface pick_extrema_bot; module procedure pick_extrema_bot_PSD;  end interface
       interface pick_extrema_top; module procedure pick_extrema_top_PSD;  end interface

       type physical_sub_domain
         type(sub_domain) :: total
         type(sub_domain) :: physical
         logical :: defined = .false.
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_PSD(PS,g_R1,g_R2,g_R1_id,g_R2_id)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_R1_id,g_R2_id
         real(cp) :: tol
         tol = 10.0_cp**(-12.0_cp)
         call init(PS%total,g_R1,g_R2,g_R1_id,g_R2_id,tol,0)
         call init(PS%physical,g_R1,g_R2,g_R1_id,g_R2_id,tol,1)
         ! PS%defined = PS%total%defined.and.PS%physical%defined
         PS%defined = PS%physical%defined
       end subroutine

       subroutine init_copy_PSD(PS,PS_in)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         type(physical_sub_domain),intent(in) :: PS_in
         call init(PS%total,PS_in%total)
         call init(PS%physical,PS_in%physical)
         PS%defined = PS_in%defined
       end subroutine

       subroutine delete_PSD(PS)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         call delete(PS%total)
         call delete(PS%physical)
         PS%defined = .false.
       end subroutine

       subroutine display_PSD(PS,name,u)
         implicit none
         type(physical_sub_domain),intent(in) :: PS
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         write(u,*) ' ********** physical_sub_domain ************ '//name
         write(u,*) 'defined = ',PS%defined
         call display(PS%total,name,u)
         call display(PS%physical,name,u)
         write(u,*) ' ********************************* '
       end subroutine

       subroutine print_PSD(PS,name)
         implicit none
         type(physical_sub_domain),intent(in) :: PS
         character(len=*),intent(in) :: name
         call display(PS,name,6)
       end subroutine

       subroutine export_PSD(PS,u)
         implicit none
         type(physical_sub_domain),intent(in) :: PS
         integer,intent(in) :: u
         write(u,*) 'defined = '; write(u,*) PS%defined
         call export(PS%total,u)
         call export(PS%physical,u)
       end subroutine

       subroutine import_PSD(PS,u)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         integer,intent(in) :: u
         read(u,*); read(u,*) PS%defined
         call import(PS%total,u)
         call import(PS%physical,u)
       end subroutine

       ! **********************************************************************
       ! **********************************************************************
       ! **********************************************************************

       subroutine init_mixed_PSD(PS,DL)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         type(data_location),intent(in) :: DL
         call init_mixed(PS%total%M,PS%total%C,PS%total%N,DL)
         call init_mixed(PS%physical%M,PS%physical%C,PS%physical%N,DL)
       end subroutine

       subroutine pick_extrema_bot_PSD(PS,dir)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         integer,intent(in) :: dir
         call pick_extrema_bot(PS%physical%C(dir),PS%total%C(dir))
         call pick_extrema_bot(PS%physical%N(dir),PS%total%N(dir))
         call pick_extrema_bot(PS%physical%M(dir),PS%total%M(dir))
       end subroutine

       subroutine pick_extrema_top_PSD(PS,dir)
         implicit none
         type(physical_sub_domain),intent(inout) :: PS
         integer,intent(in) :: dir
         call pick_extrema_top(PS%physical%C(dir),PS%total%C(dir))
         call pick_extrema_top(PS%physical%N(dir),PS%total%N(dir))
         call pick_extrema_top(PS%physical%M(dir),PS%total%M(dir))
       end subroutine

       end module