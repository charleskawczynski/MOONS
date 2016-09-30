       module init_field_mod
       use current_precision_mod
       use IO_SF_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use BCs_mod
       implicit none

       private
       public :: init_field_zero

       interface init_field_zero;      module procedure init_field_zero_SF;      end interface
       interface init_field_zero;      module procedure init_field_zero_SF;      end interface

       contains

       subroutine init_field_zero_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         logical,intent(in) :: restartP
         character(len=*),intent(in) :: dir
         if (restartP) then
               call initRestartPfield(p,m,dir)
         elseif (preDefinedP_ICs.ne.0) then
               call initPreDefinedPfield(p)
         else; call initUserPfield(p)
         endif
       end subroutine
       
       subroutine initRestartPfield(p,m,dir)
         implicit none
         type(SF),intent(inout) :: p
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,p,dir,'pc',0)
         call delete(temp)
       end subroutine
       
       subroutine initPreDefinedPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       subroutine initUserPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       end module