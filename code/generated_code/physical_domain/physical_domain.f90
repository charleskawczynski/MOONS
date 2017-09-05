       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module physical_domain_mod
       use IO_tools_mod
       use physical_sub_domain_mod
       implicit none

       private
       public :: physical_domain
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_physical_domain;      end interface
       interface delete;       module procedure delete_physical_domain;         end interface
       interface display;      module procedure display_physical_domain;        end interface
       interface display_short;module procedure display_short_physical_domain;  end interface
       interface display;      module procedure display_wrapper_physical_domain;end interface
       interface print;        module procedure print_physical_domain;          end interface
       interface print_short;  module procedure print_short_physical_domain;    end interface
       interface export;       module procedure export_physical_domain;         end interface
       interface import;       module procedure import_physical_domain;         end interface
       interface export;       module procedure export_wrapper_physical_domain; end interface
       interface import;       module procedure import_wrapper_physical_domain; end interface

       type physical_domain
         integer :: s = 0
         type(physical_sub_domain),dimension(:),allocatable :: sd
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_physical_domain(this,that)
         implicit none
         type(physical_domain),intent(inout) :: this
         type(physical_domain),intent(in) :: that
         integer :: i_sd
         integer :: s_sd
         call delete(this)
         this%s = that%s
         if (allocated(that%sd)) then
           s_sd = size(that%sd)
           if (s_sd.gt.0) then
             allocate(this%sd(s_sd))
             do i_sd=1,s_sd
               call init(this%sd(i_sd),that%sd(i_sd))
             enddo
           endif
         endif
         this%defined = that%defined
       end subroutine

       subroutine delete_physical_domain(this)
         implicit none
         type(physical_domain),intent(inout) :: this
         integer :: i_sd
         integer :: s_sd
         this%s = 0
         if (allocated(this%sd)) then
           s_sd = size(this%sd)
           do i_sd=1,s_sd
             call delete(this%sd(i_sd))
           enddo
           deallocate(this%sd)
         endif
         this%defined = .false.
       end subroutine

       subroutine display_physical_domain(this,un)
         implicit none
         type(physical_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_sd
         integer :: s_sd
         write(un,*) ' -------------------- physical_domain'
         write(un,*) 's       = ',this%s
         if (allocated(this%sd)) then
           s_sd = size(this%sd)
           do i_sd=1,s_sd
             call display(this%sd(i_sd),un)
           enddo
         endif
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_short_physical_domain(this,un)
         implicit none
         type(physical_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_sd
         integer :: s_sd
         write(un,*) 's       = ',this%s
         if (allocated(this%sd)) then
           s_sd = size(this%sd)
           do i_sd=1,s_sd
             call display(this%sd(i_sd),un)
           enddo
         endif
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine print_physical_domain(this)
         implicit none
         type(physical_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_physical_domain(this)
         implicit none
         type(physical_domain),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_physical_domain(this,un)
         implicit none
         type(physical_domain),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_sd
         integer :: s_sd
         write(un,*) 's        = ';write(un,*) this%s
         if (allocated(this%sd)) then
           s_sd = size(this%sd)
           write(un,*) s_sd
           do i_sd=1,s_sd
             call export(this%sd(i_sd),un)
           enddo
         endif
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_physical_domain(this,un)
         implicit none
         type(physical_domain),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_sd
         integer :: s_sd
         call delete(this)
         read(un,*); read(un,*) this%s
         if (allocated(this%sd)) then
           read(un,*) s_sd
           do i_sd=1,s_sd
             call import(this%sd(i_sd),un)
           enddo
         endif
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_physical_domain(this,dir,name)
         implicit none
         type(physical_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_physical_domain(this,dir,name)
         implicit none
         type(physical_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_physical_domain(this,dir,name)
         implicit none
         type(physical_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module