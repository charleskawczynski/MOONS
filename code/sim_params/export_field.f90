       module export_field_mod
       implicit none

       private
       public :: export_field
       public :: init,delete,export,import,display,print

       type export_field
         logical :: export_ever = .false.
       end type

       interface init;      module procedure init_EP;      end interface
       interface init;      module procedure init_copy_EP; end interface
       interface delete;    module procedure delete_EP;    end interface
       interface export;    module procedure export_EP;    end interface
       interface import;    module procedure import_EP;    end interface
       interface display;   module procedure display_EP;   end interface
       interface print;     module procedure print_EP;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_EP(EP,export_ever)
         implicit none
         type(export_field),intent(inout) :: EP
         logical,intent(in) :: export_ever
         EP%export_ever = export_ever
       end subroutine

       subroutine init_copy_EP(EP,EP_in)
         implicit none
         type(export_field),intent(inout) :: EP
         type(export_field),intent(in) :: EP_in
         EP%export_ever = EP_in%export_ever
       end subroutine

       subroutine delete_EP(EP)
         implicit none
         type(export_field),intent(inout) :: EP
         EP%export_ever = .false.
       end subroutine

       subroutine export_EP(EP,un)
         implicit none
         type(export_field),intent(in) :: EP
         integer,intent(in) :: un
         write(un,*) 'export_ever = '; write(un,*) EP%export_ever
       end subroutine

       subroutine import_EP(EP,un)
         implicit none
         type(export_field),intent(inout) :: EP
         integer,intent(in) :: un
         read(un,*); read(un,*) EP%export_ever
       end subroutine

       subroutine display_EP(EP,un)
         implicit none
         type(export_field),intent(in) :: EP
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',EP%export_ever
       end subroutine

       subroutine print_EP(EP)
         implicit none
         type(export_field),intent(inout) :: EP
         call display(EP,6)
       end subroutine

       end module