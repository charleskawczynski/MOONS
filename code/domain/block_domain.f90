       module block_domain_mod
       use IO_tools_mod
       use block_mod
       use domain_mod
       use mesh_mod
       implicit none

       private
       public :: block_domain
       public :: init,delete,display,print,export,import ! Essentials

       interface init;        module procedure init_block_domain;           end interface
       interface init;        module procedure init_block_domain_copy;      end interface
       interface delete;      module procedure delete_block_domain;         end interface
       interface display;     module procedure display_block_domain;        end interface
       interface print;       module procedure print_block_domain;          end interface
       interface export;      module procedure export_block_domain;         end interface
       interface import;      module procedure import_block_domain;         end interface
       interface export;      module procedure export_block_domain_wrapper; end interface
       interface import;      module procedure import_block_domain_wrapper; end interface

       type block_domain
         type(domain),dimension(6) :: f   ! Faces, includes first exterior and first interior cells
         type(domain),dimension(6) :: fg,fb,fi   ! Faces
         type(domain),dimension(12) :: eg,eb,ei  ! Edges
         type(domain),dimension(8) :: cg,cb,ci   ! Corners
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_block_domain(BD,B)
         implicit none
         type(block_domain),intent(inout) :: BD
         type(block),intent(in) :: B
         integer :: i
         call delete(BD)
         do i=1,6;  call add(BD%f(i),B%g,B%f(i),1,i); enddo

         do i=1,6;  call add(BD%fg(i),B%g,B%fg(i),1,i); enddo
         do i=1,12; call add(BD%eg(i),B%g,B%eg(i),1,i); enddo
         do i=1,8;  call add(BD%cg(i),B%g,B%cg(i),1,i); enddo

         do i=1,6;  call add(BD%fb(i),B%g,B%fb(i),1,i); enddo
         do i=1,12; call add(BD%eb(i),B%g,B%eb(i),1,i); enddo
         do i=1,8;  call add(BD%cb(i),B%g,B%cb(i),1,i); enddo

         do i=1,6;  call add(BD%fi(i),B%g,B%fi(i),1,i); enddo
         do i=1,12; call add(BD%ei(i),B%g,B%ei(i),1,i); enddo
         do i=1,8;  call add(BD%ci(i),B%g,B%ci(i),1,i); enddo
       end subroutine

       subroutine init_block_domain_copy(BD_out,BD_in)
         implicit none
         type(block_domain),intent(inout) :: BD_out
         type(block_domain),intent(in) :: BD_in
         integer :: i
         call delete(BD_out)
         do i=1,6;  call init(BD_out%f(i),BD_in%f(i)); enddo

         do i=1,6;  call init(BD_out%fg(i),BD_in%fg(i)); enddo
         do i=1,12; call init(BD_out%eg(i),BD_in%eg(i)); enddo
         do i=1,8;  call init(BD_out%cg(i),BD_in%cg(i)); enddo

         do i=1,6;  call init(BD_out%fb(i),BD_in%fb(i)); enddo
         do i=1,12; call init(BD_out%eb(i),BD_in%eb(i)); enddo
         do i=1,8;  call init(BD_out%cb(i),BD_in%cb(i)); enddo

         do i=1,6;  call init(BD_out%fi(i),BD_in%fi(i)); enddo
         do i=1,12; call init(BD_out%ei(i),BD_in%ei(i)); enddo
         do i=1,8;  call init(BD_out%ci(i),BD_in%ci(i)); enddo
       end subroutine

       subroutine delete_block_domain(BD)
         implicit none
         type(block_domain),intent(inout) :: BD
         integer :: i
         do i=1,6;  call delete(BD%f(i)); enddo

         do i=1,6;  call delete(BD%fg(i)); enddo
         do i=1,12; call delete(BD%eg(i)); enddo
         do i=1,8;  call delete(BD%cg(i)); enddo

         do i=1,6;  call delete(BD%fb(i)); enddo
         do i=1,12; call delete(BD%eb(i)); enddo
         do i=1,8;  call delete(BD%cb(i)); enddo

         do i=1,6;  call delete(BD%fi(i)); enddo
         do i=1,12; call delete(BD%ei(i)); enddo
         do i=1,8;  call delete(BD%ci(i)); enddo

       end subroutine

       subroutine print_block_domain(BD,name)
         implicit none
         type(block_domain),intent(in) :: BD
         character(len=*),intent(in) :: name
         integer :: i
         do i=1,6;  call print(BD%f(i),name); enddo

         do i=1,6;  call print(BD%fg(i),name); enddo
         do i=1,12; call print(BD%eg(i),name); enddo
         do i=1,8;  call print(BD%cg(i),name); enddo

         do i=1,6;  call print(BD%fb(i),name); enddo
         do i=1,12; call print(BD%eb(i),name); enddo
         do i=1,8;  call print(BD%cb(i),name); enddo

         do i=1,6;  call print(BD%fi(i),name); enddo
         do i=1,12; call print(BD%ei(i),name); enddo
         do i=1,8;  call print(BD%ci(i),name); enddo
       end subroutine

       subroutine display_block_domain(BD,un)
         implicit none
         type(block_domain),intent(inout) :: BD
         integer,intent(in) :: un
         integer :: i
         do i=1,6;  call display(BD%f(i),un); enddo

         do i=1,6;  call display(BD%fg(i),un); enddo
         do i=1,12; call display(BD%eg(i),un); enddo
         do i=1,8;  call display(BD%cg(i),un); enddo

         do i=1,6;  call display(BD%fb(i),un); enddo
         do i=1,12; call display(BD%eb(i),un); enddo
         do i=1,8;  call display(BD%cb(i),un); enddo

         do i=1,6;  call display(BD%fi(i),un); enddo
         do i=1,12; call display(BD%ei(i),un); enddo
         do i=1,8;  call display(BD%ci(i),un); enddo
       end subroutine

       subroutine export_block_domain(BD,un)
         implicit none
         type(block_domain),intent(in) :: BD
         integer,intent(in) :: un
         integer :: i
         do i=1,6;  call export(BD%f(i),un); enddo

         do i=1,6;  call export(BD%fg(i),un); enddo
         do i=1,12; call export(BD%eg(i),un); enddo
         do i=1,8;  call export(BD%cg(i),un); enddo

         do i=1,6;  call export(BD%fb(i),un); enddo
         do i=1,12; call export(BD%eb(i),un); enddo
         do i=1,8;  call export(BD%cb(i),un); enddo

         do i=1,6;  call export(BD%fi(i),un); enddo
         do i=1,12; call export(BD%ei(i),un); enddo
         do i=1,8;  call export(BD%ci(i),un); enddo
       end subroutine

       subroutine export_block_domain_wrapper(BD,dir,name)
         implicit none
         type(block_domain),intent(in) :: BD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(BD,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_block_domain(BD,un)
         implicit none
         type(block_domain),intent(inout) :: BD
         integer,intent(in) :: un
         integer :: i
         do i=1,6;  call import(BD%f(i),un); enddo

         do i=1,6;  call import(BD%fg(i),un); enddo
         do i=1,12; call import(BD%eg(i),un); enddo
         do i=1,8;  call import(BD%cg(i),un); enddo

         do i=1,6;  call import(BD%fb(i),un); enddo
         do i=1,12; call import(BD%eb(i),un); enddo
         do i=1,8;  call import(BD%cb(i),un); enddo

         do i=1,6;  call import(BD%fi(i),un); enddo
         do i=1,12; call import(BD%ei(i),un); enddo
         do i=1,8;  call import(BD%ci(i),un); enddo
       end subroutine

       subroutine import_block_domain_wrapper(BD,dir,name)
         implicit none
         type(block_domain),intent(inout) :: BD
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(BD,un)
         call close_and_message(un,dir,name)
       end subroutine

       end module