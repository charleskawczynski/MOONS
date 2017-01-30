       module export_now_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: export_now
       public :: init,delete,export,import
       public :: update

       interface init;     module procedure init_EN;     end interface
       interface delete;   module procedure delete_EN;   end interface
       interface export;   module procedure export_EN;   end interface
       interface import;   module procedure import_EN;   end interface
       interface update;   module procedure update_EN;   end interface

       type step
         logical :: this = .false.
         logical :: next = .false.
       end type

       type export_now
         type(step) :: X,all
         type(string) :: dir,name
         logical :: any_next = .false.
         integer :: un
       end type

       contains

       subroutine update_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = s%next
         s%next = .false.
       end subroutine

       subroutine delete_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = .false.
         s%next = .false.
       end subroutine

       subroutine init_EN(EN,dir,name)
         implicit none
         type(export_now),intent(inout) :: EN
         character(len=*),intent(in) :: dir,name
         call delete_step(EN%X)
         call delete_step(EN%all)
         EN%any_next = .false.

         call init(EN%dir,dir)
         call init(EN%name,name)
       end subroutine

       subroutine delete_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         call delete_step(EN%X)
         call delete_step(EN%all)
         EN%any_next = .false.

         call delete(EN%dir)
         call delete(EN%name)
       end subroutine

       subroutine export_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         integer :: un
         un = new_and_open(str(EN%dir),str(EN%name))
         write(un,*) 'X_next = ';   write(un,*) EN%X%next
         write(un,*) 'all_next = '; write(un,*) EN%all%next
         close(un)
       end subroutine

       subroutine import_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         integer :: un
         un = open_to_read(str(EN%dir),str(EN%name))
         read(un,*) ; read(un,*) EN%X%next
         read(un,*) ; read(un,*) EN%all%next
         close(un)
       end subroutine

       subroutine update_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         EN%any_next = any((/EN%X%next,EN%all%next/))
         call update_step(EN%X)
         call update_step(EN%all)
       end subroutine

       end module