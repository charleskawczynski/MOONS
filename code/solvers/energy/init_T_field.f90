       module init_T_field_mod
       use current_precision_mod
       use SF_mod
       use IO_import_mod
       use mesh_mod
       use sim_params_mod
       implicit none

       private
       public :: init_T_field

       contains

       subroutine init_T_field(T,m,SP,dir)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call assign(T,0.0_cp)

         preset_ID = SP%VS%T%IC
         if (SP%VS%T%SS%restart) then
               call restart_T(T,m,dir)
         else
           select case(preset_ID)
           case (0); call uniformTfield(T)
           case (1); call initUserTfield(T)
           case default; stop 'Error: bad preset_ID in init_T_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_T(T,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: T
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,T,dir,'Tc',1)
         call delete(temp)
       end subroutine

       subroutine uniformTfield(T)
         implicit none
         type(SF),intent(inout) :: T
         call assign(T,0.0_cp)
       end subroutine

       subroutine initUserTfield(T)
         implicit none
         type(SF),intent(inout) :: T
         call uniformTfield(T)
       end subroutine

       end module
