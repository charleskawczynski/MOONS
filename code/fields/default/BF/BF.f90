      module block_field_mod
        use current_precision_mod
        use grid_mod
        use BCs_mod
        implicit none
        private

        public :: block_field

        type block_field
          type(grid_field) :: B ! bulk
          type(boundary_conditions) :: BCs
          type(stitches) :: st
        end type

       interface init;               module procedure init_block_field;               end interface
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

       subroutine init_block_field(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         integer :: i
         call init(BF%B,B)
         do i=1,6;  call init(b%f(i),g); enddo
         do i=1,12; call init(b%e(i),g); enddo
         do i=1,8;  call init(b%c(i),g); enddo
       end subroutine

       subroutine init_block_field_copy(b_out,b_in)
         implicit none
         type(block_field),intent(inout) :: b_out
         type(block_field),intent(in) :: b_in
         integer :: i
         call init(b_out%b,b_in%b)
         do i=1,6;  call init(b_out%f(i),b_in%f(i)); enddo
         do i=1,12; call init(b_out%e(i),b_in%e(i)); enddo
         do i=1,8;  call init(b_out%c(i),b_in%c(i)); enddo
       end subroutine

       subroutine delete_block_field(b)
         implicit none
         type(block_field),intent(inout) :: b
         call delete(b%b)
         do i=1,6;  call delete(b%f(i))); enddo
         do i=1,12; call delete(b%e(i))); enddo
         do i=1,8;  call delete(b%c(i))); enddo
       end subroutine

       subroutine display_block_field(b,un)
         implicit none
         type(block_field),intent(in) :: b
         integer,intent(in) :: un
         call display(b%b,un)
         do i=1,6;  call display(b%f(i),un); enddo
         do i=1,12; call display(b%e(i),un); enddo
         do i=1,8;  call display(b%c(i),un); enddo
       end subroutine

       subroutine print_block_field(b)
         implicit none
         type(block_field),intent(in) :: b
         call print(b%b)
         do i=1,6;  call print(b%f(i)); enddo
         do i=1,12; call print(b%e(i)); enddo
         do i=1,8;  call print(b%c(i)); enddo
       end subroutine

       subroutine export_block_field(b,un)
         implicit none
         type(block_field),intent(in) :: b
         integer,intent(in) :: un
         call export(b%b,un)
         do i=1,6;  call export(b%f(i),un); enddo
         do i=1,12; call export(b%e(i),un); enddo
         do i=1,8;  call export(b%c(i),un); enddo
       end subroutine

       subroutine import_block_field(b,un)
         implicit none
         type(block_field),intent(in) :: b
         integer,intent(in) :: un
         call import(b%b,un)
         do i=1,6;  call import(b%f(i),un); enddo
         do i=1,12; call import(b%e(i),un); enddo
         do i=1,8;  call import(b%c(i),un); enddo
       end subroutine

      end module