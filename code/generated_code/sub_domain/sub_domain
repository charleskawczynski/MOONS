       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sub_domain_mod
       use IO_tools_mod
       use overlap_mod
       implicit none

       private
       public :: sub_domain
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_sub_domain;           end interface
       interface delete; module procedure delete_sub_domain;         end interface
       interface display;module procedure display_sub_domain;        end interface
       interface display;module procedure display_wrapper_sub_domain;end interface
       interface print;  module procedure print_sub_domain;          end interface
       interface export; module procedure export_sub_domain;         end interface
       interface import; module procedure import_sub_domain;         end interface
       interface export; module procedure export_wrapper_sub_domain; end interface
       interface import; module procedure import_wrapper_sub_domain; end interface

       type sub_domain
         type(overlap),dimension(3) :: c
         type(overlap),dimension(3) :: n
         type(overlap),dimension(3) :: m
         logical :: defined = .false.
         integer :: g_r1_id = 0
         integer :: g_r2_id = 0
       end type

       contains

       subroutine init_sub_domain(this,that)
         implicit none
         type(sub_domain),intent(inout) :: this
         type(sub_domain),intent(in) :: that
         integer :: i_c
         integer :: i_n
         integer :: i_m
         integer :: s_c
         integer :: s_n
         integer :: s_m
         call delete(this)
         s_c = size(that%c)
         do i_c=1,s_c
           call init(this%c(i_c),that%c(i_c))
         enddo
         s_n = size(that%n)
         do i_n=1,s_n
           call init(this%n(i_n),that%n(i_n))
         enddo
         s_m = size(that%m)
         do i_m=1,s_m
           call init(this%m(i_m),that%m(i_m))
         enddo
         this%defined = that%defined
         this%g_r1_id = that%g_r1_id
         this%g_r2_id = that%g_r2_id
       end subroutine

       subroutine delete_sub_domain(this)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer :: i_c
         integer :: i_n
         integer :: i_m
         integer :: s_c
         integer :: s_n
         integer :: s_m
         s_c = size(this%c)
         do i_c=1,s_c
           call delete(this%c(i_c))
         enddo
         s_n = size(this%n)
         do i_n=1,s_n
           call delete(this%n(i_n))
         enddo
         s_m = size(this%m)
         do i_m=1,s_m
           call delete(this%m(i_m))
         enddo
         this%defined = .false.
         this%g_r1_id = 0
         this%g_r2_id = 0
       end subroutine

       subroutine display_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- sub_domain'
         integer :: i_c
         integer :: i_n
         integer :: i_m
         integer :: s_c
         integer :: s_n
         integer :: s_m
         s_c = size(this%c)
         do i_c=1,s_c
           call display(this%c(i_c),un)
         enddo
         s_n = size(this%n)
         do i_n=1,s_n
           call display(this%n(i_n),un)
         enddo
         s_m = size(this%m)
         do i_m=1,s_m
           call display(this%m(i_m),un)
         enddo
         write(un,*) 'defined = ',this%defined
         write(un,*) 'g_r1_id = ',this%g_r1_id
         write(un,*) 'g_r2_id = ',this%g_r2_id
       end subroutine

       subroutine print_sub_domain(this)
         implicit none
         type(sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: i_n
         integer :: i_m
         integer :: s_c
         integer :: s_n
         integer :: s_m
         s_c = size(this%c)
         write(un,*) s_c
         do i_c=1,s_c
           call export(this%c(i_c),un)
         enddo
         s_n = size(this%n)
         write(un,*) s_n
         do i_n=1,s_n
           call export(this%n(i_n),un)
         enddo
         s_m = size(this%m)
         write(un,*) s_m
         do i_m=1,s_m
           call export(this%m(i_m),un)
         enddo
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 'g_r1_id  = ';write(un,*) this%g_r1_id
         write(un,*) 'g_r2_id  = ';write(un,*) this%g_r2_id
       end subroutine

       subroutine import_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: i_n
         integer :: i_m
         integer :: s_c
         integer :: s_n
         integer :: s_m
         call delete(this)
         read(un,*) s_c
         do i_c=1,s_c
           call import(this%c(i_c),un)
         enddo
         read(un,*) s_n
         do i_n=1,s_n
           call import(this%n(i_n),un)
         enddo
         read(un,*) s_m
         do i_m=1,s_m
           call import(this%m(i_m),un)
         enddo
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%g_r1_id
         read(un,*); read(un,*) this%g_r2_id
       end subroutine

       subroutine display_wrapper_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module