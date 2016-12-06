       module print_export_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: print_export
       public :: init,delete,export,import
       public :: update

       integer :: i_default_info = 2
       integer :: i_default_transient_0D = 2
       integer :: i_default_transient_2D = 4
       integer :: i_default_solution = 6

       interface init;               module procedure init_PE;             end interface
       interface delete;             module procedure delete_PE;           end interface
       interface export;             module procedure export_PE;           end interface
       interface import;             module procedure import_PE;           end interface

       interface update;             module procedure update_PE;           end interface

       type print_export
         logical :: solution = .false.
         logical :: info = .false.
         logical :: transient_0D = .false.
         logical :: transient_2D = .false.

         integer :: i_solution
         integer :: i_info
         integer :: i_transient_0D
         integer :: i_transient_2D

         logical :: export_planar = .false.
         integer :: un
         type(string) :: dir,name
       end type

       contains

       subroutine init_PE(PE,export_planar,dir,name)
         implicit none
         type(print_export),intent(inout) :: PE
         logical,intent(in) :: export_planar
         character(len=*),intent(in) :: dir,name
         PE%info = .false.
         PE%transient_0D = .false.
         PE%transient_2D = .false.
         PE%solution = .false.
         PE%i_info = i_default_info
         PE%i_transient_0D = i_default_transient_0D
         PE%i_transient_2D = i_default_transient_2D
         PE%i_solution = i_default_solution

         PE%export_planar = export_planar
         call init(PE%dir,dir)
         call init(PE%name,name)
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

       subroutine export_PE(PE)
         implicit none
         type(print_export),intent(in) :: PE
         integer :: un
         un = new_and_open(str(PE%dir),str(PE%name))
         write(un,*) 'i_info = ';         write(un,*) PE%i_info
         write(un,*) 'i_transient_0D = '; write(un,*) PE%i_transient_0D
         write(un,*) 'i_transient_2D = '; write(un,*) PE%i_transient_2D
         write(un,*) 'i_solution = ';     write(un,*) PE%i_solution
         write(un,*) 'export_planar = ';  write(un,*) PE%export_planar ! DO NOT IMPORT (DANGEROUS)
         close(un)
       end subroutine

       subroutine import_PE(PE)
         implicit none
         type(print_export),intent(inout) :: PE
         integer :: un
         un = open_to_read(str(PE%dir),str(PE%name))
         read(un,*) ; read(un,*) PE%i_info
         read(un,*) ; read(un,*) PE%i_transient_0D
         read(un,*) ; read(un,*) PE%i_transient_2D
         read(un,*) ; read(un,*) PE%i_solution
         close(un)

         ! The following is a safegaurd against a bad import:
         if ((PE%i_info.lt.0).or.(PE%i_info.gt.6)) PE%i_info = i_default_info
         if ((PE%i_transient_0D.lt.0).or.(PE%i_transient_0D.gt.6)) PE%i_transient_0D = i_default_transient_0D
         if ((PE%i_transient_2D.lt.0).or.(PE%i_transient_2D.gt.6)) PE%i_transient_2D = i_default_transient_2D
         if ((PE%i_solution.lt.0).or.(PE%i_solution.gt.6)) PE%i_solution = i_default_solution
       end subroutine

       subroutine update_PE(PE,n_step)
         implicit none
         type(print_export),intent(inout) :: PE
         integer(li),intent(in) :: n_step
         logical,dimension(0:6) :: temp,temp2
         integer :: i
         temp(0) = .true.
         temp(1:6) = (/((mod(n_step,10**i).eq.1).and.(n_step.ne.1),i=1,6)/)
         temp2(1:6) = (/((mod(n_step,500).eq.1).and.(n_step.ne.1),i=1,6)/)
         ! temp(1:6) = (/((mod(n_step+1,10**i).eq.1).and.(n_step.ne.0),i=1,6)/)

         PE%info = temp(PE%i_info)
         PE%transient_0D = temp(PE%i_transient_0D).or.(n_step.eq.0)
         ! PE%transient_2D = temp2(PE%i_transient_2D).or.(n_step.eq.0)
         PE%transient_2D = PE%export_planar.and.(temp(PE%i_transient_2D).or.n_step.eq.1)
         PE%solution = temp(PE%i_solution).and.(n_step.gt.1)
       end subroutine

       end module