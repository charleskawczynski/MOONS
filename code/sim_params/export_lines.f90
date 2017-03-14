       module export_lines_mod
       use export_line_mod
       implicit none

       private
       public :: export_lines
       public :: init,delete,export,import,display,print

       public :: add

       type export_lines
         integer :: N = 0
         type(export_line),dimension(:),allocatable :: EL
       end type

       interface init;      module procedure init_ELS;      end interface
       interface init;      module procedure init_copy_ELS; end interface
       interface delete;    module procedure delete_ELS;    end interface
       interface export;    module procedure export_ELS;    end interface
       interface import;    module procedure import_ELS;    end interface
       interface display;   module procedure display_ELS;   end interface
       interface print;     module procedure print_ELS;     end interface

       interface add;       module procedure add_ELS;       end interface
       interface add;       module procedure add_ELS_EL;   end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_ELS(ELS,export_ever,dir,line,suffix)
         implicit none
         type(export_lines),intent(inout) :: ELS
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,dimension(2),intent(in) :: line
         character(len=1),intent(in) :: suffix
         call delete(ELS)
         allocate(ELS%EL(1))
         call init(ELS%EL(1),export_ever,dir,line,suffix)
         ELS%N = 1
       end subroutine

       subroutine add_ELS(ELS,export_ever,dir,line,suffix)
         implicit none
         type(export_lines),intent(inout) :: ELS
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,dimension(2),intent(in) :: line
         character(len=1),intent(in) :: suffix
         type(export_lines) :: temp
         integer :: i
         if (allocated(ELS%EL)) then
           if (size(ELS%EL).gt.0) then
             call init(temp,ELS)
             call delete(ELS)
             allocate(ELS%EL(temp%N+1))
             ELS%N = temp%N+1
             do i=1,temp%N
               call init(ELS%EL(i),temp%EL(i))
             enddo
             call init(ELS%EL(ELS%N),export_ever,dir,line,suffix)
             call delete(temp)
           else; call init(ELS,export_ever,dir,line,suffix)
           endif
         else; call init(ELS,export_ever,dir,line,suffix)
         endif
       end subroutine

       subroutine add_ELS_EL(ELS,EL_in)
         implicit none
         type(export_lines),intent(inout) :: ELS
         type(export_line),intent(in) :: EL_in
         call add(ELS,EL_in%export_ever,EL_in%dir,EL_in%line,EL_in%suffix)
       end subroutine

       subroutine init_copy_ELS(ELS,ELS_in)
         implicit none
         type(export_lines),intent(inout) :: ELS
         type(export_lines),intent(in) :: ELS_in
         integer :: i
         call delete(ELS)
         allocate(ELS%EL(ELS_in%N))
         ELS%N = ELS_in%N
         do i=1,ELS_in%N
           call init(ELS%EL(i),ELS_in%EL(i))
         enddo
       end subroutine

       subroutine delete_ELS(ELS)
         implicit none
         type(export_lines),intent(inout) :: ELS
         integer :: i
         if (allocated(ELS%EL)) then
         do i=1,size(ELS%EL); call delete(ELS%EL(i)); enddo
         deallocate(ELS%EL)
         endif
       end subroutine

       subroutine export_ELS(ELS,un)
         implicit none
         type(export_lines),intent(in) :: ELS
         integer,intent(in) :: un
         integer :: i
         write(un,*) ELS%N
         if (allocated(ELS%EL)) then
         do i=1,size(ELS%EL); call export(ELS%EL(i),un); enddo
         endif
       end subroutine

       subroutine import_ELS(ELS,un)
         implicit none
         type(export_lines),intent(inout) :: ELS
         integer,intent(in) :: un
         integer :: i,N
         call delete(ELS)
         read(un,*) N
         if (N.gt.0) then
           allocate(ELS%EL(N))
           do i=1,N; call import(ELS%EL(i),un); enddo
         endif
       end subroutine

       subroutine display_ELS(ELS,un)
         implicit none
         type(export_lines),intent(in) :: ELS
         integer,intent(in) :: un
         integer :: i
         if (allocated(ELS%EL)) then
         do i=1,size(ELS%EL); call display(ELS%EL(i),un); enddo
         endif
       end subroutine

       subroutine print_ELS(ELS)
         implicit none
         type(export_lines),intent(inout) :: ELS
         call display(ELS,6)
       end subroutine

       end module