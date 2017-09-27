       module mesh_block_extend_mod
       use mesh_block_mod
       ! This module is used to export the block to visualize
       ! each grid on the block (all FECs).
       use mesh_extend_mod
       use block_extend_mod
       use block_mod
       use IO_tools_mod
       implicit none

       private
       public :: mesh_block
       public :: init,delete,display,print,export,import ! Essentials

       interface init;   module procedure init_mesh_block;   end interface

       contains

       subroutine init_mesh_block(MB,B)
         implicit none
         type(mesh_block),intent(inout) :: MB
         type(block),intent(in) :: B
         type(block) :: temp
         integer :: i
         call delete(MB%m)
         call delete(MB%B)
         call init(MB%m,B%g)
         call init(temp,B%g)
         call init_FEC(temp)
         do i=1,6;  call add(MB%m,temp%f(i)); enddo
         ! do i=1,12; call add(MB%m,temp%e(i)); enddo
         ! do i=1,8;  call add(MB%m,temp%c(i)); enddo

         ! do i=1,6;  call add(MB%m,temp%fb(i)); enddo
         ! do i=1,12; call add(MB%m,temp%eb(i)); enddo
         ! do i=1,8;  call add(MB%m,temp%cb(i)); enddo
         call delete(temp)
       end subroutine

       end module