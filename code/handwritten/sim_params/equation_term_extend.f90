     module equation_term_extend_mod
     use equation_term_mod
     use current_precision_mod
     use IO_tools_mod
     implicit none

     private
     public :: init

     interface init;    module procedure init_ET;           end interface

     contains

     subroutine init_ET(ET,add,scale)
       implicit none
       type(equation_term),intent(inout) :: ET
       logical,intent(in) :: add
       real(cp),intent(in) :: scale
       ET%add   = add
       if (add) ET%scale = scale
      end subroutine

     end module