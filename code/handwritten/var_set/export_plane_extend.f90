       module export_plane_extend_mod
       use export_plane_mod
       implicit none

       private
       public :: init
       interface init;      module procedure init_EP;      end interface

       contains

       subroutine init_EP(EP,export_ever,dir,plane,suffix)
         implicit none
         type(export_plane),intent(inout) :: EP
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,intent(in) :: plane
         character(len=1),intent(in) :: suffix
         EP%export_ever = export_ever
         EP%dir         = dir
         EP%plane        = plane
         EP%suffix      = suffix
       end subroutine

       end module