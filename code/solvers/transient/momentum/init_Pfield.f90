       module init_Pfield_mod
       use IO_SF_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use BCs_mod
       use init_Ufield_mod ! for restartU
       implicit none

       private
       public :: init_Pfield,restartP

       integer,parameter :: preDefinedP_ICs = 1
       logical,parameter :: restartP = restartU

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine init_Pfield(p,m,dir)
         implicit none
         type(SF),intent(inout) :: p
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
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
         call import_1C_SF(temp,p,dir//'Ufield/','pci',0)
         ! call export_1C_SF(temp,p,dir//'Ufield/','pci_imported',0)
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