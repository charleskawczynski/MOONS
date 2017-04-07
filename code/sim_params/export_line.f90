       module export_line_mod
       implicit none

       private
       public :: export_line
       public :: init,delete,export,import,display,print

       type export_line
         logical :: export_ever = .false.
         integer :: dir = 0
         integer,dimension(2) :: line = 0
         character(len=1) :: suffix = '1'
       end type

       interface init;      module procedure init_EL;      end interface
       interface init;      module procedure init_copy_EL; end interface
       interface delete;    module procedure delete_EL;    end interface
       interface export;    module procedure export_EL;    end interface
       interface import;    module procedure import_EL;    end interface
       interface display;   module procedure display_EL;   end interface
       interface print;     module procedure print_EL;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_EL(EL,export_ever,dir,line,suffix)
         implicit none
         type(export_line),intent(inout) :: EL
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,dimension(2),intent(in) :: line
         character(len=1),intent(in) :: suffix
         EL%export_ever = export_ever
         EL%dir         = dir
         EL%line        = line
         EL%suffix      = suffix
       end subroutine

       subroutine init_copy_EL(EL,EL_in)
         implicit none
         type(export_line),intent(inout) :: EL
         type(export_line),intent(in) :: EL_in
         EL%export_ever = EL_in%export_ever
         EL%dir         = EL_in%dir
         EL%line        = EL_in%line
         EL%suffix      = EL_in%suffix
       end subroutine

       subroutine delete_EL(EL)
         implicit none
         type(export_line),intent(inout) :: EL
         EL%export_ever = .false.
         EL%dir         = 0
         EL%line        = 0
         EL%suffix      = '1'
       end subroutine

       subroutine export_EL(EL,un)
         implicit none
         type(export_line),intent(in) :: EL
         integer,intent(in) :: un
         write(un,*) 'export_ever = '; write(un,*) EL%export_ever
         write(un,*) 'dir         = '; write(un,*) EL%dir
         write(un,*) 'line        = '; write(un,*) EL%line
         write(un,*) 'suffix      = '; write(un,*) EL%suffix
       end subroutine

       subroutine import_EL(EL,un)
         implicit none
         type(export_line),intent(inout) :: EL
         integer,intent(in) :: un
         read(un,*); read(un,*) EL%export_ever
         read(un,*); read(un,*) EL%dir
         read(un,*); read(un,*) EL%line
         read(un,*); read(un,*) EL%suffix
       end subroutine

       subroutine display_EL(EL,un)
         implicit none
         type(export_line),intent(in) :: EL
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',EL%export_ever
         write(un,*) 'dir         = ',EL%dir
         write(un,*) 'line        = ',EL%line
         write(un,*) 'suffix      = ',EL%suffix
       end subroutine

       subroutine print_EL(EL)
         implicit none
         type(export_line),intent(inout) :: EL
         call display(EL,6)
       end subroutine

       end module