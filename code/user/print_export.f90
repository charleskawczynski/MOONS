       module print_export_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: print_export
       public :: init,delete,display,print,export,import
       public :: update

       integer,parameter :: i_default_info = 2
       integer,parameter :: i_default_transient_0D = 2
       integer,parameter :: i_default_transient_2D = 4
       integer,parameter :: i_default_solution = 6

       interface init;    module procedure init_PE;           end interface
       interface init;    module procedure init_PE_copy;      end interface
       interface delete;  module procedure delete_PE;         end interface
       interface display; module procedure display_PE;        end interface
       interface print;   module procedure print_PE;          end interface
       interface export;  module procedure export_PE;         end interface
       interface export;  module procedure export_PE_wrapper; end interface
       interface import;  module procedure import_PE;         end interface
       interface import;  module procedure import_PE_wrapper; end interface

       interface update;  module procedure update_PE;         end interface

       type print_export
         logical :: solution = .false.
         logical :: info = .false.
         logical :: transient_0D = .false.
         logical :: transient_2D = .false.

         integer :: i_info = i_default_info
         integer :: i_transient_0D = i_default_transient_0D
         integer :: i_transient_2D = i_default_transient_2D
         integer :: i_solution = i_default_solution

         logical :: export_planar = .false.
         integer :: un = 0
         type(string) :: dir,name
       end type

       contains

       subroutine init_PE(PE,i_info,i_transient_0D,i_transient_2D,i_solution,&
         export_planar,dir,name)
         implicit none
         type(print_export),intent(inout) :: PE
         integer,intent(in) :: i_info,i_transient_0D,i_transient_2D,i_solution
         logical,intent(in) :: export_planar
         character(len=*),intent(in) :: dir,name
         PE%info = .false.
         PE%transient_0D = .false.
         PE%transient_2D = .false.
         PE%solution = .false.
         PE%i_info = i_info
         PE%i_transient_0D = i_transient_0D
         PE%i_transient_2D = i_transient_2D
         PE%i_solution = i_solution

         PE%export_planar = export_planar
         call init(PE%dir,dir)
         call init(PE%name,name)
       end subroutine

       subroutine init_PE_copy(PE,PE_in)
         implicit none
         type(print_export),intent(inout) :: PE
         type(print_export),intent(in) :: PE_in
         PE%info = PE_in%info
         PE%transient_0D = PE_in%transient_0D
         PE%transient_2D = PE_in%transient_2D
         PE%solution = PE_in%solution
         PE%i_info = PE_in%i_info
         PE%i_transient_0D = PE_in%i_transient_0D
         PE%i_transient_2D = PE_in%i_transient_2D
         PE%i_solution = PE_in%i_solution
         PE%export_planar = PE_in%export_planar
         call init(PE%dir,PE_in%dir)
         call init(PE%name,PE_in%name)
       end subroutine

       subroutine delete_PE(PE)
         implicit none
         type(print_export),intent(inout) :: PE
         PE%info = .false.
         PE%transient_0D = .false.
         PE%transient_2D = .false.
         PE%solution = .false.
         PE%i_info = i_default_info
         PE%i_transient_0D = i_default_transient_0D
         PE%i_transient_2D = i_default_transient_2D
         PE%i_solution = i_default_solution
         call delete(PE%dir)
         call delete(PE%name)
       end subroutine

       subroutine display_PE(PE,un)
         implicit none
         type(print_export),intent(in) :: PE
         integer,intent(in) :: un
         write(un,*) 'i_info = ',PE%i_info
         write(un,*) 'i_transient_0D = ',PE%i_transient_0D
         write(un,*) 'i_transient_2D = ',PE%i_transient_2D
         write(un,*) 'i_solution = ',PE%i_solution
         write(un,*) 'export_planar = ',PE%export_planar
       end subroutine

       subroutine print_PE(PE)
         implicit none
         type(print_export),intent(in) :: PE
         call display(PE,6)
       end subroutine

       subroutine export_PE(PE,un)
         implicit none
         type(print_export),intent(in) :: PE
         integer,intent(in) :: un
         write(un,*) 'i_info = ';         write(un,*) PE%i_info
         write(un,*) 'i_transient_0D = '; write(un,*) PE%i_transient_0D
         write(un,*) 'i_transient_2D = '; write(un,*) PE%i_transient_2D
         write(un,*) 'i_solution = ';     write(un,*) PE%i_solution
         write(un,*) 'export_planar = ';  write(un,*) PE%export_planar
       end subroutine

       subroutine import_PE(PE,un)
         implicit none
         type(print_export),intent(inout) :: PE
         integer,intent(in) :: un
         read(un,*) ; read(un,*) PE%i_info
         read(un,*) ; read(un,*) PE%i_transient_0D
         read(un,*) ; read(un,*) PE%i_transient_2D
         read(un,*) ; read(un,*) PE%i_solution
         read(un,*) ; read(un,*) PE%export_planar
         call safegaurd_import(PE)
       end subroutine

       subroutine export_PE_wrapper(PE)
         implicit none
         type(print_export),intent(in) :: PE
         integer :: un
         un = new_and_open(str(PE%dir),str(PE%name))
         call export(PE,un)
         close(un)
       end subroutine

       subroutine import_PE_wrapper(PE)
         implicit none
         type(print_export),intent(inout) :: PE
         integer :: un
         un = open_to_read(str(PE%dir),str(PE%name))
         call import(PE,un)
         close(un)
       end subroutine

       subroutine safegaurd_import(PE)
         implicit none
         type(print_export),intent(inout) :: PE
         if ((PE%i_info.lt.0).or.(PE%i_info.gt.6)) PE%i_info = i_default_info
         if ((PE%i_transient_0D.lt.0).or.(PE%i_transient_0D.gt.6)) PE%i_transient_0D = i_default_transient_0D
         if ((PE%i_transient_2D.lt.0).or.(PE%i_transient_2D.gt.6)) PE%i_transient_2D = i_default_transient_2D
         if ((PE%i_solution.lt.0).or.(PE%i_solution.gt.6)) PE%i_solution = i_default_solution
       end subroutine

       subroutine update_PE(PE,n_step)
         implicit none
         type(print_export),intent(inout) :: PE
         integer(li),intent(in) :: n_step
         logical,dimension(0:6) :: temp1,temp2
         integer :: i
         logical :: first_step,past_first_step
         first_step = n_step.eq.0
         past_first_step = n_step.gt.1
         temp1(0) = .true.
         temp2(0) = .true.
         temp1(1:6) = (/((mod(n_step,1*10**i).eq.1).and.past_first_step,i=1,6)/)
         temp2(1:6) = (/((mod(n_step,5*10**i).eq.1).and.past_first_step,i=1,6)/)
         PE%info         = temp1(PE%i_info).or.first_step
         PE%transient_0D = temp1(PE%i_transient_0D).or.first_step
         PE%transient_2D = PE%export_planar.and.(temp1(PE%i_transient_2D).or.first_step)
         PE%solution     = temp1(PE%i_solution).and.past_first_step
       end subroutine

       end module