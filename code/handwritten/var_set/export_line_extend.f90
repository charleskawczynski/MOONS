       module export_line_extend_mod
       use export_line_mod
       implicit none

       private
       public :: init

       interface init;      module procedure init_EL;      end interface

       contains

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

       end module