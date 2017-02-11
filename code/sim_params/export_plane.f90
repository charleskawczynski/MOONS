       module export_plane_mod
       implicit none

       private
       public :: export_plane
       public :: init,delete,export,import,display,print

       type export_plane
         logical :: export_ever = .false.
         integer :: dir = 0
         integer :: plane = 0
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

       subroutine init_EP(EP,export_ever,dir,plane)
         implicit none
         type(export_plane),intent(inout) :: EP
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir,plane
         EP%export_ever = export_ever
         EP%dir         = dir
         EP%plane       = plane
       end subroutine

       subroutine init_copy_EP(EP,EP_in)
         implicit none
         type(export_plane),intent(inout) :: EP
         type(export_plane),intent(in) :: EP_in
         EP%export_ever = EP_in%export_ever
         EP%dir         = EP_in%dir
         EP%plane       = EP_in%plane
       end subroutine

       subroutine delete_EP(EP)
         implicit none
         type(export_plane),intent(inout) :: EP
         EP%export_ever = .false.
         EP%dir         = 0
         EP%plane       = 0
       end subroutine

       subroutine export_EP(EP,un)
         implicit none
         type(export_plane),intent(in) :: EP
         integer,intent(in) :: un
         write(un,*) EP%export_ever
         write(un,*) EP%dir
         write(un,*) EP%plane
       end subroutine

       subroutine import_EP(EP,un)
         implicit none
         type(export_plane),intent(inout) :: EP
         integer,intent(in) :: un
         read(un,*) EP%export_ever
         read(un,*) EP%dir
         read(un,*) EP%plane
       end subroutine

       subroutine display_EP(EP,un)
         implicit none
         type(export_plane),intent(in) :: EP
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',EP%export_ever
         write(un,*) 'dir         = ',EP%dir
         write(un,*) 'plane       = ',EP%plane
       end subroutine

       subroutine print_EP(EP)
         implicit none
         type(export_plane),intent(inout) :: EP
         call display(EP,6)
       end subroutine

       end module