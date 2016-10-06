      module block_field_mod
        use current_precision_mod
        use grid_mod
        use GF_mod
        use block_mod
        use BCs_mod
        implicit none
        private

        public :: block_field
        public :: init,delete,display,print,export,import ! Essentials

        type block_field
          type(grid_field) :: GF ! bulk
          ! type(BCs) :: b
          ! type(boundary_conditions) :: BCs%GF%Dirichlet
          ! type(stitches) :: st
        end type

       interface init_CC;            module procedure init_CC_BF;                     end interface
       interface init;               module procedure init_block_field_copy;          end interface
       interface delete;             module procedure delete_block_field;             end interface
       interface display;            module procedure display_block_field;            end interface
       interface print;              module procedure print_block_field;              end interface
       interface export;             module procedure export_block_field;             end interface
       interface import;             module procedure import_block_field;             end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_CC_BF(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         call init_CC(BF%GF,B%g)
       end subroutine

       subroutine init_block_field_copy(b_out,b_in)
         implicit none
         type(block_field),intent(inout) :: b_out
         type(block_field),intent(in) :: b_in
         call init(b_out%GF,b_in%GF)
       end subroutine

       subroutine delete_block_field(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%GF)
       end subroutine

       subroutine display_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call display(BF%GF,un)
       end subroutine

       subroutine print_block_field(BF)
         implicit none
         type(block_field),intent(in) :: BF
         call print(BF%GF)
       end subroutine

       subroutine export_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call export(BF%GF,un)
       end subroutine

       subroutine import_block_field(BF,un)
         implicit none
         type(block_field),intent(inout) :: BF
         integer,intent(in) :: un
         call import(BF%GF,un)
       end subroutine

      end module