       module geometry_props_mod
       use current_precision_mod
       use IO_tools_mod

       implicit none
       private
       public :: geometry_props
       public :: init,delete,display,print,export,import ! essentials

       type geometry_props
         integer :: geometry = 0                   ! Initialized
         real(cp) :: tw = 0.0_cp                   ! Initialized
         integer,dimension(3) :: periodic_dir = 0  ! Initialized
         integer,dimension(6) :: apply_BC_order    ! Initialized
       end type

       interface init;    module procedure init_GP;           end interface
       interface init;    module procedure init_copy_GP;      end interface
       interface delete;  module procedure delete_GP;         end interface
       interface display; module procedure display_GP;        end interface
       interface print;   module procedure print_GP;          end interface
       interface export;  module procedure export_GP;         end interface
       interface import;  module procedure import_GP;         end interface
       interface export;  module procedure export_GP_wrapper; end interface
       interface import;  module procedure import_GP_wrapper; end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_GP(GP,geometry,tw,periodic_dir,apply_BC_order)
         implicit none
         type(geometry_props),intent(inout) :: GP
         integer,intent(in) :: geometry
         real(cp),intent(in) :: tw
         integer,dimension(3),intent(in) :: periodic_dir
         integer,dimension(6),intent(in) :: apply_BC_order
         GP%geometry       = geometry
         GP%tw             = tw
         GP%periodic_dir   = periodic_dir
         GP%apply_BC_order = apply_BC_order
       end subroutine

       subroutine init_copy_GP(GP,GP_in)
         implicit none
         type(geometry_props),intent(inout) :: GP
         type(geometry_props),intent(in) :: GP_in
         GP%geometry       = GP_in%geometry
         GP%tw             = GP_in%tw
         GP%periodic_dir   = GP_in%periodic_dir
         GP%apply_BC_order = GP_in%apply_BC_order
       end subroutine

       subroutine delete_GP(GP)
         implicit none
         type(geometry_props),intent(inout) :: GP
         GP%geometry       = 0
         GP%tw             = 0.0_cp
         GP%periodic_dir   = 0
         GP%apply_BC_order = 0
       end subroutine

       subroutine display_GP(GP,un)
         implicit none
         type(geometry_props),intent(in) :: GP
         integer,intent(in) :: un
         write(un,*) 'geometry       = ',GP%geometry
         write(un,*) 'tw             = ',GP%tw
         write(un,*) 'periodic_dir   = ',GP%periodic_dir
         write(un,*) 'apply_BC_order = ',GP%apply_BC_order
       end subroutine

       subroutine print_GP(GP)
         implicit none
         type(geometry_props),intent(in) :: GP
         call display(GP,6)
       end subroutine

       subroutine export_GP(GP,un)
         implicit none
         type(geometry_props),intent(in) :: GP
         integer,intent(in) :: un
         write(un,*) GP%geometry
         write(un,*) GP%tw
         write(un,*) GP%periodic_dir
         write(un,*) GP%apply_BC_order
       end subroutine

       subroutine import_GP(GP,un)
         implicit none
         type(geometry_props),intent(inout) :: GP
         integer,intent(in) :: un
         read(un,*) GP%geometry
         read(un,*) GP%tw
         read(un,*) GP%periodic_dir
         read(un,*) GP%apply_BC_order
       end subroutine

       subroutine export_GP_wrapper(GP,dir,name)
         implicit none
         type(geometry_props),intent(in) :: GP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(GP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_GP_wrapper(GP,dir,name)
         implicit none
         type(geometry_props),intent(inout) :: GP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(GP,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       end module