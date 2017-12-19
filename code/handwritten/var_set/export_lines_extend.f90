       module export_lines_extend_mod
       use export_lines_mod
       use export_line_mod
       use export_line_extend_mod
       implicit none

       private
       public :: init
       public :: add

       interface init;      module procedure init_ELS;      end interface
       interface add;       module procedure add_ELS;       end interface
       interface add;       module procedure add_ELS_EL;    end interface

       contains

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

       end module