       module single_procedure_extend_mod
       use single_procedure_mod
       use IO_tools_mod
       use GF_mod
       use apply_BCs_faces_bridge_mod
       use apply_face_BC_op_mod

       implicit none
       private
       public :: init
       public :: insist_defined

       interface init;           module procedure init_SP;           end interface
       interface insist_defined; module procedure insist_defined_SP; end interface

       contains

       subroutine init_SP(SP,P,ID)
         implicit none
         type(single_procedure),intent(inout) :: SP
         procedure(apply_face_BC_op) :: P
         integer,intent(in) :: ID
         call delete(SP)
         SP%P => P
         SP%ID = ID
         SP%defined = .true.
       end subroutine

       subroutine insist_defined_SP(SP,caller)
         implicit none
         type(single_procedure),intent(in) :: SP
         character(len=*),intent(in) :: caller
         if (.not.SP%defined) then
         write(*,*) 'Error: SP is not defined in ',caller,' in single_procedure.f90'
         stop 'Done'
         endif
       end subroutine

       end module