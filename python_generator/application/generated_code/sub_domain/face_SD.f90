       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module face_sd_mod
       use current_precision_mod
       use IO_tools_mod
       use index_2D_mod
       use sub_domain_mod
       implicit none

       private
       public :: face_sd
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_face_sd;           end interface
       interface delete; module procedure delete_face_sd;         end interface
       interface display;module procedure display_face_sd;        end interface
       interface display;module procedure display_wrapper_face_sd;end interface
       interface print;  module procedure print_face_sd;          end interface
       interface export; module procedure export_face_sd;         end interface
       interface import; module procedure import_face_sd;         end interface
       interface export; module procedure export_wrapper_face_sd; end interface
       interface import; module procedure import_wrapper_face_sd; end interface

       type face_sd
         integer :: s = 0
         type(sub_domain),dimension(6) :: g
         type(sub_domain),dimension(6) :: g_periodic_n
         type(sub_domain),dimension(6) :: b
         type(sub_domain),dimension(6) :: i
         type(sub_domain),dimension(6) :: i_opp
         type(sub_domain),dimension(6) :: i_opp_periodic_n
         type(index_2d),dimension(6) :: i_2d
         real(cp),dimension(6) :: dh = 0.0_cp
         real(cp),dimension(6) :: nhat = 0.0_cp
         real(cp),dimension(6) :: c_w = 0.0_cp
         real(cp),dimension(6) :: robin_coeff = 0.0_cp
       end type

       contains

       subroutine init_face_sd(this,that)
         implicit none
         type(face_sd),intent(inout) :: this
         type(face_sd),intent(in) :: that
         integer :: i_g
         integer :: i_g_periodic_n
         integer :: i_b
         integer :: i_i
         integer :: i_i_opp
         integer :: i_i_opp_periodic_n
         integer :: i_i_2d
         integer :: s_g
         integer :: s_g_periodic_n
         integer :: s_b
         integer :: s_i
         integer :: s_i_opp
         integer :: s_i_opp_periodic_n
         integer :: s_i_2d
         call delete(this)
         this%s = that%s
         s_g = size(that%g)
         do i_g=1,s_g
           call init(this%g(i_g),that%g(i_g))
         enddo
         s_g_periodic_n = size(that%g_periodic_n)
         do i_g_periodic_n=1,s_g_periodic_n
           call init(this%g_periodic_n(i_g_periodic_n),that%g_periodic_n(i_g_periodic_n))
         enddo
         s_b = size(that%b)
         do i_b=1,s_b
           call init(this%b(i_b),that%b(i_b))
         enddo
         s_i = size(that%i)
         do i_i=1,s_i
           call init(this%i(i_i),that%i(i_i))
         enddo
         s_i_opp = size(that%i_opp)
         do i_i_opp=1,s_i_opp
           call init(this%i_opp(i_i_opp),that%i_opp(i_i_opp))
         enddo
         s_i_opp_periodic_n = size(that%i_opp_periodic_n)
         do i_i_opp_periodic_n=1,s_i_opp_periodic_n
           call init(this%i_opp_periodic_n(i_i_opp_periodic_n),that%i_opp_periodic_n(i_i_opp_periodic_n))
         enddo
         s_i_2d = size(that%i_2d)
         do i_i_2d=1,s_i_2d
           call init(this%i_2d(i_i_2d),that%i_2d(i_i_2d))
         enddo
         this%dh = that%dh
         this%nhat = that%nhat
         this%c_w = that%c_w
         this%robin_coeff = that%robin_coeff
       end subroutine

       subroutine delete_face_sd(this)
         implicit none
         type(face_sd),intent(inout) :: this
         integer :: i_g
         integer :: i_g_periodic_n
         integer :: i_b
         integer :: i_i
         integer :: i_i_opp
         integer :: i_i_opp_periodic_n
         integer :: i_i_2d
         integer :: s_g
         integer :: s_g_periodic_n
         integer :: s_b
         integer :: s_i
         integer :: s_i_opp
         integer :: s_i_opp_periodic_n
         integer :: s_i_2d
         this%s = 0
         s_g = size(this%g)
         do i_g=1,s_g
           call delete(this%g(i_g))
         enddo
         s_g_periodic_n = size(this%g_periodic_n)
         do i_g_periodic_n=1,s_g_periodic_n
           call delete(this%g_periodic_n(i_g_periodic_n))
         enddo
         s_b = size(this%b)
         do i_b=1,s_b
           call delete(this%b(i_b))
         enddo
         s_i = size(this%i)
         do i_i=1,s_i
           call delete(this%i(i_i))
         enddo
         s_i_opp = size(this%i_opp)
         do i_i_opp=1,s_i_opp
           call delete(this%i_opp(i_i_opp))
         enddo
         s_i_opp_periodic_n = size(this%i_opp_periodic_n)
         do i_i_opp_periodic_n=1,s_i_opp_periodic_n
           call delete(this%i_opp_periodic_n(i_i_opp_periodic_n))
         enddo
         s_i_2d = size(this%i_2d)
         do i_i_2d=1,s_i_2d
           call delete(this%i_2d(i_i_2d))
         enddo
         this%dh = 0.0_cp
         this%nhat = 0.0_cp
         this%c_w = 0.0_cp
         this%robin_coeff = 0.0_cp
       end subroutine

       subroutine display_face_sd(this,un)
         implicit none
         type(face_sd),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- face_sd'
         integer :: i_g
         integer :: i_g_periodic_n
         integer :: i_b
         integer :: i_i
         integer :: i_i_opp
         integer :: i_i_opp_periodic_n
         integer :: i_i_2d
         integer :: s_g
         integer :: s_g_periodic_n
         integer :: s_b
         integer :: s_i
         integer :: s_i_opp
         integer :: s_i_opp_periodic_n
         integer :: s_i_2d
         write(un,*) 's                = ',this%s
         s_g = size(this%g)
         do i_g=1,s_g
           call display(this%g(i_g),un)
         enddo
         s_g_periodic_n = size(this%g_periodic_n)
         do i_g_periodic_n=1,s_g_periodic_n
           call display(this%g_periodic_n(i_g_periodic_n),un)
         enddo
         s_b = size(this%b)
         do i_b=1,s_b
           call display(this%b(i_b),un)
         enddo
         s_i = size(this%i)
         do i_i=1,s_i
           call display(this%i(i_i),un)
         enddo
         s_i_opp = size(this%i_opp)
         do i_i_opp=1,s_i_opp
           call display(this%i_opp(i_i_opp),un)
         enddo
         s_i_opp_periodic_n = size(this%i_opp_periodic_n)
         do i_i_opp_periodic_n=1,s_i_opp_periodic_n
           call display(this%i_opp_periodic_n(i_i_opp_periodic_n),un)
         enddo
         s_i_2d = size(this%i_2d)
         do i_i_2d=1,s_i_2d
           call display(this%i_2d(i_i_2d),un)
         enddo
         write(un,*) 'dh               = ',this%dh
         write(un,*) 'nhat             = ',this%nhat
         write(un,*) 'c_w              = ',this%c_w
         write(un,*) 'robin_coeff      = ',this%robin_coeff
       end subroutine

       subroutine print_face_sd(this)
         implicit none
         type(face_sd),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_face_sd(this,un)
         implicit none
         type(face_sd),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_g
         integer :: i_g_periodic_n
         integer :: i_b
         integer :: i_i
         integer :: i_i_opp
         integer :: i_i_opp_periodic_n
         integer :: i_i_2d
         integer :: s_g
         integer :: s_g_periodic_n
         integer :: s_b
         integer :: s_i
         integer :: s_i_opp
         integer :: s_i_opp_periodic_n
         integer :: s_i_2d
         write(un,*) 's                 = ';write(un,*) this%s
         s_g = size(this%g)
         write(un,*) s_g
         do i_g=1,s_g
           call export(this%g(i_g),un)
         enddo
         s_g_periodic_n = size(this%g_periodic_n)
         write(un,*) s_g_periodic_n
         do i_g_periodic_n=1,s_g_periodic_n
           call export(this%g_periodic_n(i_g_periodic_n),un)
         enddo
         s_b = size(this%b)
         write(un,*) s_b
         do i_b=1,s_b
           call export(this%b(i_b),un)
         enddo
         s_i = size(this%i)
         write(un,*) s_i
         do i_i=1,s_i
           call export(this%i(i_i),un)
         enddo
         s_i_opp = size(this%i_opp)
         write(un,*) s_i_opp
         do i_i_opp=1,s_i_opp
           call export(this%i_opp(i_i_opp),un)
         enddo
         s_i_opp_periodic_n = size(this%i_opp_periodic_n)
         write(un,*) s_i_opp_periodic_n
         do i_i_opp_periodic_n=1,s_i_opp_periodic_n
           call export(this%i_opp_periodic_n(i_i_opp_periodic_n),un)
         enddo
         s_i_2d = size(this%i_2d)
         write(un,*) s_i_2d
         do i_i_2d=1,s_i_2d
           call export(this%i_2d(i_i_2d),un)
         enddo
         write(un,*) 'dh                = ';write(un,*) this%dh
         write(un,*) 'nhat              = ';write(un,*) this%nhat
         write(un,*) 'c_w               = ';write(un,*) this%c_w
         write(un,*) 'robin_coeff       = ';write(un,*) this%robin_coeff
       end subroutine

       subroutine import_face_sd(this,un)
         implicit none
         type(face_sd),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_g
         integer :: i_g_periodic_n
         integer :: i_b
         integer :: i_i
         integer :: i_i_opp
         integer :: i_i_opp_periodic_n
         integer :: i_i_2d
         integer :: s_g
         integer :: s_g_periodic_n
         integer :: s_b
         integer :: s_i
         integer :: s_i_opp
         integer :: s_i_opp_periodic_n
         integer :: s_i_2d
         call delete(this)
         read(un,*); read(un,*) this%s
         read(un,*) s_g
         do i_g=1,s_g
           call import(this%g(i_g),un)
         enddo
         read(un,*) s_g_periodic_n
         do i_g_periodic_n=1,s_g_periodic_n
           call import(this%g_periodic_n(i_g_periodic_n),un)
         enddo
         read(un,*) s_b
         do i_b=1,s_b
           call import(this%b(i_b),un)
         enddo
         read(un,*) s_i
         do i_i=1,s_i
           call import(this%i(i_i),un)
         enddo
         read(un,*) s_i_opp
         do i_i_opp=1,s_i_opp
           call import(this%i_opp(i_i_opp),un)
         enddo
         read(un,*) s_i_opp_periodic_n
         do i_i_opp_periodic_n=1,s_i_opp_periodic_n
           call import(this%i_opp_periodic_n(i_i_opp_periodic_n),un)
         enddo
         read(un,*) s_i_2d
         do i_i_2d=1,s_i_2d
           call import(this%i_2d(i_i_2d),un)
         enddo
         read(un,*); read(un,*) this%dh
         read(un,*); read(un,*) this%nhat
         read(un,*); read(un,*) this%c_w
         read(un,*); read(un,*) this%robin_coeff
       end subroutine

       subroutine display_wrapper_face_sd(this,dir,name)
         implicit none
         type(face_sd),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_face_sd(this,dir,name)
         implicit none
         type(face_sd),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_face_sd(this,dir,name)
         implicit none
         type(face_sd),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module