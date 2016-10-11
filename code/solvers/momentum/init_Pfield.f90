       module init_Pfield_mod
       use current_precision_mod
       use IO_SF_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use boundary_conditions_mod
       implicit none

       private
       public :: init_Pfield
       integer :: preDefinedP_ICs = 1

       contains

       subroutine init_Pfield(p,m,restartP,dir)
         implicit none
         type(SF),intent(inout) :: p
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