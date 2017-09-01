       module export_field_extend_mod
       use export_field_mod
       implicit none

       private
       public :: init
       interface init;      module procedure init_EP;      end interface

       contains

       subroutine init_EP(EP,export_ever)
         implicit none
         type(export_field),intent(inout) :: EP
         logical,intent(in) :: export_ever
         EP%export_ever = export_ever
       end subroutine

       end module