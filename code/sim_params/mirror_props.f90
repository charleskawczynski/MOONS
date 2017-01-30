       module mirror_props_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use IO_tools_mod

       implicit none
       private
       public :: mirror_props
       public :: init,delete,display,print,export,import ! essentials

       public :: anti_mirror

       type mirror_props
         logical :: mirror = .false.
         integer :: mirror_face = 0
         real(cp),dimension(3) :: mirror_sign = (/1.0_cp,1.0_cp,1.0_cp/)
         real(cp),dimension(3) :: mirror_sign_a = (/1.0_cp,1.0_cp,1.0_cp/)
       end type

       interface init;           module procedure init_MP;           end interface
       interface init;           module procedure init_copy_MP;      end interface
       interface delete;         module procedure delete_MP;         end interface
       interface display;        module procedure display_MP;        end interface
       interface print;          module procedure print_MP;          end interface
       interface export;         module procedure export_MP;         end interface
       interface import;         module procedure import_MP;         end interface
       interface export;         module procedure export_MP_wrapper; end interface
       interface import;         module procedure import_MP_wrapper; end interface

       interface anti_mirror;    module procedure anti_mirror_MP;    end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_MP(MP,mirror,mirror_face)
         implicit none
         type(mirror_props),intent(inout) :: MP
         logical,intent(in) :: mirror
         integer,intent(in) :: mirror_face
         integer :: dir
         MP%mirror      = mirror
         MP%mirror_face = mirror_face
         MP%mirror_sign = (/1.0_cp,1.0_cp,1.0_cp/)
         dir = dir_given_face(mirror_face)
         MP%mirror_sign_a = -MP%mirror_sign
         MP%mirror_sign_a(dir) = 1.0_cp
         MP%mirror_sign(dir) = -1.0_cp
       end subroutine

       subroutine init_copy_MP(MP,GP_in)
         implicit none
         type(mirror_props),intent(inout) :: MP
         type(mirror_props),intent(in) :: GP_in
         MP%mirror      = GP_in%mirror
         MP%mirror_face = GP_in%mirror_face
         MP%mirror_sign = GP_in%mirror_sign
         MP%mirror_sign_a = GP_in%mirror_sign_a
       end subroutine

       subroutine delete_MP(MP)
         implicit none
         type(mirror_props),intent(inout) :: MP
         MP%mirror = .false.
         MP%mirror_sign = (/1.0_cp,1.0_cp,1.0_cp/)
         MP%mirror_sign_a = (/1.0_cp,1.0_cp,1.0_cp/)
         MP%mirror_face = 0
       end subroutine

       subroutine display_MP(MP,un)
         implicit none
         type(mirror_props),intent(in) :: MP
         integer,intent(in) :: un
         write(un,*) 'mirror        = ',MP%mirror
         write(un,*) 'mirror_face   = ',MP%mirror_face
         write(un,*) 'mirror_sign   = ',MP%mirror_sign
         write(un,*) 'mirror_sign_a = ',MP%mirror_sign_a
       end subroutine

       subroutine print_MP(MP)
         implicit none
         type(mirror_props),intent(in) :: MP
         call display(MP,6)
       end subroutine

       subroutine export_MP(MP,un)
         implicit none
         type(mirror_props),intent(in) :: MP
         integer,intent(in) :: un
         write(un,*) MP%mirror
         write(un,*) MP%mirror_face
         write(un,*) MP%mirror_sign
         write(un,*) MP%mirror_sign_a
       end subroutine

       subroutine import_MP(MP,un)
         implicit none
         type(mirror_props),intent(inout) :: MP
         integer,intent(in) :: un
         read(un,*) MP%mirror
         read(un,*) MP%mirror_face
         read(un,*) MP%mirror_sign
         read(un,*) MP%mirror_sign_a
       end subroutine

       subroutine export_MP_wrapper(MP,dir,name)
         implicit none
         type(mirror_props),intent(in) :: MP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_MP_wrapper(MP,dir,name)
         implicit none
         type(mirror_props),intent(inout) :: MP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(MP,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       function anti_mirror_MP(MP_in) result(MP)
         implicit none
         type(mirror_props),intent(in) :: MP_in
         real(cp),dimension(3) :: temp
         type(mirror_props) :: MP
         call init(MP,MP_in)
         temp = MP%mirror_sign
         MP%mirror_sign = MP%mirror_sign_a
         MP%mirror_sign_a = temp
       end function

       end module