       module simple_int_tensor_mod
       use face_edge_corner_indexing_mod

       implicit none
       private
       public :: simple_int_tensor
       public :: init,delete,display,print,export,import ! essentials

       type simple_int_tensor
         integer,dimension(3) :: eye
       end type

       interface init;           module procedure init_SIT;           end interface
       interface init;           module procedure init_copy_SIT;      end interface
       interface delete;         module procedure delete_SIT;         end interface
       interface display;        module procedure display_SIT;        end interface
       interface print;          module procedure print_SIT;          end interface
       interface export;         module procedure export_SIT;         end interface
       interface import;         module procedure import_SIT;         end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_SIT(SIT,dir)
         implicit none
         type(simple_int_tensor),intent(inout) :: SIT
         integer,intent(in) :: dir
         SIT%eye = eye_given_dir(dir)
       end subroutine

       subroutine init_copy_SIT(SIT,SIT_in)
         implicit none
         type(simple_int_tensor),intent(inout) :: SIT
         type(simple_int_tensor),intent(in) :: SIT_in
         SIT%eye = SIT_in%eye
       end subroutine

       subroutine delete_SIT(SIT)
         implicit none
         type(simple_int_tensor),intent(inout) :: SIT
         SIT%eye = 0
       end subroutine

       subroutine display_SIT(SIT,un)
         implicit none
         type(simple_int_tensor),intent(in) :: SIT
         integer,intent(in) :: un
         write(un,*) 'SIT%eye = ',SIT%eye
       end subroutine

       subroutine print_SIT(SIT)
         implicit none
         type(simple_int_tensor),intent(in) :: SIT
         call display(SIT,6)
       end subroutine

       subroutine export_SIT(SIT,un)
         implicit none
         type(simple_int_tensor),intent(in) :: SIT
         integer,intent(in) :: un
         write(un,*) SIT%eye
       end subroutine

       subroutine import_SIT(SIT,un)
         implicit none
         type(simple_int_tensor),intent(inout) :: SIT
         integer,intent(in) :: un
         read(un,*) SIT%eye
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       end module