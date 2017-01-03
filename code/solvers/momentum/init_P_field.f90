       module init_P_field_mod
       use current_precision_mod
       use IO_import_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use boundary_conditions_mod
       use benchmark_case_mod
       implicit none

       private
       public :: init_P_field

       contains

       subroutine init_P_field(p,m,BMC,dir)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(benchmark_case),intent(in) :: BMC
         integer :: preset_ID

         call assign(P,0.0_cp)

         preset_ID = BMC%VS%P%IC
         if (BMC%VS%P%SS%restart) then
               call restart_P(P,m,dir)
         else
           select case(preset_ID)
           case (0)
           case (1); call initUserPfield(p)
           case default; stop 'Error: bad preset_ID in init_P_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_P(p,m,dir)
         implicit none
         type(SF),intent(inout) :: p
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         call import_3D_1C(m,p,dir,'pc',0)
       end subroutine

       subroutine initUserPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       end module