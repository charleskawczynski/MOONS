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
         logical :: this,next
       end type

       type export_now
         type(step) :: U,B,T,all
         type(string) :: dir,name
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
         call delete_step(EN%T)
         call delete_step(EN%U)
         call delete_step(EN%B)
         call delete_step(EN%all)

         call init(EN%dir,dir)
         call init(EN%name,name)
       end subroutine

       subroutine delete_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         call delete_step(EN%T)
         call delete_step(EN%U)
         call delete_step(EN%B)
         call delete_step(EN%all)

         call delete(EN%dir)
         call delete(EN%name)
         EN%un = 0
       end subroutine

       subroutine export_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         integer :: un
         un = newAndOpen(str(EN%dir),str(EN%name))
         write(un,*) 'T_next = ';   write(un,*) EN%T%next
         write(un,*) 'U_next = ';   write(un,*) EN%U%next
         write(un,*) 'B_next = ';   write(un,*) EN%B%next
         write(un,*) 'all_next = '; write(un,*) EN%all%next
         close(un)
       end subroutine

       subroutine import_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         integer :: un
         un = openToRead(str(EN%dir),str(EN%name))
         read(un,*) ; read(un,*) EN%T%next
         read(un,*) ; read(un,*) EN%U%next
         read(un,*) ; read(un,*) EN%B%next
         read(un,*) ; read(un,*) EN%all%next
         close(un)
       end subroutine

       subroutine update_EN(EN)
         implicit none
         type(export_now),intent(inout) :: EN
         call update_step(EN%T)
         call update_step(EN%U)
         call update_step(EN%B)
         call update_step(EN%all)
       end subroutine

       end module