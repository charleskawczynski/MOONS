       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sub_domain_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use overlap_mod
       use string_mod
       implicit none

       private
       public :: sub_domain
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_sub_domain;        end interface
       interface delete;           module procedure delete_sub_domain;           end interface
       interface display;          module procedure display_sub_domain;          end interface
       interface display_short;    module procedure display_short_sub_domain;    end interface
       interface display;          module procedure display_wrap_sub_domain;     end interface
       interface print;            module procedure print_sub_domain;            end interface
       interface print_short;      module procedure print_short_sub_domain;      end interface
       interface export;           module procedure export_sub_domain;           end interface
       interface export_primitives;module procedure export_primitives_sub_domain;end interface
       interface export_restart;   module procedure export_restart_sub_domain;   end interface
       interface import;           module procedure import_sub_domain;           end interface
       interface import_restart;   module procedure import_restart_sub_domain;   end interface
       interface import_primitives;module procedure import_primitives_sub_domain;end interface
       interface export;           module procedure export_wrap_sub_domain;      end interface
       interface import;           module procedure import_wrap_sub_domain;      end interface
       interface make_restart_dir; module procedure make_restart_dir_sub_domain; end interface
       interface suppress_warnings;module procedure suppress_warnings_sub_domain;end interface

       type sub_domain
         type(overlap),dimension(3) :: C
         type(overlap),dimension(3) :: N
         type(overlap),dimension(3) :: M
         logical :: defined = .false.
         integer :: g_R1_id = 0
         integer :: g_R2_id = 0
       end type

       contains

       subroutine init_copy_sub_domain(this,that)
         implicit none
         type(sub_domain),intent(inout) :: this
         type(sub_domain),intent(in) :: that
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         call delete(this)
         s_C = size(that%C)
         do i_C=1,s_C
           call init(this%C(i_C),that%C(i_C))
         enddo
         s_N = size(that%N)
         do i_N=1,s_N
           call init(this%N(i_N),that%N(i_N))
         enddo
         s_M = size(that%M)
         do i_M=1,s_M
           call init(this%M(i_M),that%M(i_M))
         enddo
         this%defined = that%defined
         this%g_R1_id = that%g_R1_id
         this%g_R2_id = that%g_R2_id
       end subroutine

       subroutine delete_sub_domain(this)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         s_C = size(this%C)
         do i_C=1,s_C
           call delete(this%C(i_C))
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call delete(this%N(i_N))
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call delete(this%M(i_M))
         enddo
         this%defined = .false.
         this%g_R1_id = 0
         this%g_R2_id = 0
       end subroutine

       subroutine display_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         s_C = size(this%C)
         do i_C=1,s_C
           call display(this%C(i_C),un)
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call display(this%N(i_N),un)
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call display(this%M(i_M),un)
         enddo
         write(un,*) 'defined = ',this%defined
         write(un,*) 'g_R1_id = ',this%g_R1_id
         write(un,*) 'g_R2_id = ',this%g_R2_id
       end subroutine

       subroutine display_short_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         s_C = size(this%C)
         do i_C=1,s_C
           call display(this%C(i_C),un)
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call display(this%N(i_N),un)
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call display(this%M(i_M),un)
         enddo
         write(un,*) 'defined = ',this%defined
         write(un,*) 'g_R1_id = ',this%g_R1_id
         write(un,*) 'g_R2_id = ',this%g_R2_id
       end subroutine

       subroutine display_wrap_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_sub_domain(this)
         implicit none
         type(sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_sub_domain(this)
         implicit none
         type(sub_domain),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 'g_R1_id  = ';write(un,*) this%g_R1_id
         write(un,*) 'g_R2_id  = ';write(un,*) this%g_R2_id
       end subroutine

       subroutine export_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         s_C = size(this%C)
         write(un,*) s_C
         do i_C=1,s_C
           call export(this%C(i_C),un)
         enddo
         s_N = size(this%N)
         write(un,*) s_N
         do i_N=1,s_N
           call export(this%N(i_N),un)
         enddo
         s_M = size(this%M)
         write(un,*) s_M
         do i_M=1,s_M
           call export(this%M(i_M),un)
         enddo
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 'g_R1_id  = ';write(un,*) this%g_R1_id
         write(un,*) 'g_R2_id  = ';write(un,*) this%g_R2_id
       end subroutine

       subroutine import_primitives_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%g_R1_id
         read(un,*); read(un,*) this%g_R2_id
       end subroutine

       subroutine import_sub_domain(this,un)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         call delete(this)
         read(un,*) s_C
         do i_C=1,s_C
           call import(this%C(i_C),un)
         enddo
         read(un,*) s_N
         do i_N=1,s_N
           call import(this%N(i_N),un)
         enddo
         read(un,*) s_M
         do i_M=1,s_M
           call import(this%M(i_M),un)
         enddo
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%g_R1_id
         read(un,*); read(un,*) this%g_R2_id
       end subroutine

       subroutine export_wrap_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_sub_domain(this,dir,name)
         implicit none
         type(sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_sub_domain(this,dir)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         s_C = size(this%C)
         do i_C=1,s_C
           call make_restart_dir(this%C(i_C),&
           dir//fortran_PS//'C_'//int2str(i_C))
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call make_restart_dir(this%N(i_N),&
           dir//fortran_PS//'N_'//int2str(i_N))
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call make_restart_dir(this%M(i_M),&
           dir//fortran_PS//'M_'//int2str(i_M))
         enddo
       end subroutine

       subroutine export_restart_sub_domain(this,dir)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         s_C = size(this%C)
         do i_C=1,s_C
           call export_restart(this%C(i_C),&
           dir//fortran_PS//'C_'//int2str(i_C))
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call export_restart(this%N(i_N),&
           dir//fortran_PS//'N_'//int2str(i_N))
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call export_restart(this%M(i_M),&
           dir//fortran_PS//'M_'//int2str(i_M))
         enddo
       end subroutine

       subroutine import_restart_sub_domain(this,dir)
         implicit none
         type(sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_C
         integer :: i_N
         integer :: i_M
         integer :: s_C
         integer :: s_N
         integer :: s_M
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         s_C = size(this%C)
         do i_C=1,s_C
           call import_restart(this%C(i_C),&
           dir//fortran_PS//'C_'//int2str(i_C))
         enddo
         s_N = size(this%N)
         do i_N=1,s_N
           call import_restart(this%N(i_N),&
           dir//fortran_PS//'N_'//int2str(i_N))
         enddo
         s_M = size(this%M)
         do i_M=1,s_M
           call import_restart(this%M(i_M),&
           dir//fortran_PS//'M_'//int2str(i_M))
         enddo
       end subroutine

       subroutine suppress_warnings_sub_domain(this)
         implicit none
         type(sub_domain),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module