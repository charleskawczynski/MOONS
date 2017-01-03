       module init_B_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_import_mod
       use benchmark_case_mod
       implicit none

       private
       public :: init_B_field

       contains

       subroutine init_B_field(B,m,BMC,dir)
         implicit none
         type(VF),intent(inout) :: B
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(benchmark_case),intent(in) :: BMC
         integer :: preset_ID

         call initZeroField(B)

         preset_ID = BMC%VS%B%IC
         if (BMC%VS%B%SS%restart) then
               call restart_B(B,m,dir)
         else
           select case(preset_ID)
           case (0)
           case default; stop 'Error: bad preset_ID in init_B_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_B(B,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: B
         call import_3D_1C(m,B%x,dir,'Bf_x',0)
         call import_3D_1C(m,B%y,dir,'Bf_y',0)
         call import_3D_1C(m,B%z,dir,'Bf_z',0)
       end subroutine

       subroutine initZeroField(B)
         implicit none
         type(VF),intent(inout) :: B
         call assign(B,0.0_cp)
       end subroutine

       end module