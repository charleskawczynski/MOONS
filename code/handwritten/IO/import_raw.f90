       module import_raw_mod
       use current_precision_mod
       use mesh_extend_mod
       use string_mod
       use SF_extend_mod
       use VF_extend_mod
       use data_location_extend_mod
       use IO_tools_mod
       use IO_import_mod

       implicit none
       private

       public :: import_raw
       interface import_raw; module procedure ir_steady_SF; end interface
       interface import_raw; module procedure ir_steady_VF; end interface

       contains

       ! **********************************************************************
       ! ***************************** IMPORT RAW *****************************
       ! **********************************************************************

       subroutine ir_steady_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         call import_3D_1C(m,x,dir,name,pad)
       end subroutine

       subroutine ir_steady_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (is_collocated(x)) then; call import_3D_3C(m,x,dir,name,pad)
         else;                       call import_3D_1C(m,x%x,dir,name,pad)
                                     call import_3D_1C(m,x%y,dir,name,pad)
                                     call import_3D_1C(m,x%z,dir,name,pad)
         endif
       end subroutine

       end module