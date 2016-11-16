       module init_rhofield_mod
       use current_precision_mod
       use SF_mod
       use IO_SF_mod
       use mesh_mod
       implicit none

       private
       public :: init_rho_field
       integer :: preDefinedrho_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                              0 : User-defined case (no override)
       !                              1 : Uniform

       contains

       subroutine init_rho_field(rho,m,restart_rho,dir)
         implicit none
         type(SF),intent(inout) :: rho
         type(mesh),intent(in) :: m
         logical,intent(in) :: restart_rho
         character(len=*),intent(in) :: dir
         if (restart_rho) then
           call initrestart_rho(rho,m,dir)
         elseif (preDefinedrho_ICs.ne.0) then
           call init_preDefined_rho(rho)
         endif
       end subroutine

       subroutine initrestart_rho(rho,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: rho
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,rho,dir,'Tc',1)
         call delete(temp)
       end subroutine

       subroutine init_preDefined_rho(rho)
         implicit none
         type(SF),intent(inout) :: rho
         select case (preDefinedrho_ICs)
         case (1); call uniform_rho_field(rho)
         case default
         stop 'Incorrect preDefinedrho_ICs case in init_rhoField.f90.'
         end select
       end subroutine

       subroutine uniform_rho_field(rho)
         implicit none
         type(SF),intent(inout) :: rho
         call assign(rho,0.0_cp)
       end subroutine

       end module
