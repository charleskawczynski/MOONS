       module init_U_field_mod
       use current_precision_mod
       use IO_import_mod
       use IO_export_mod
       use grid_mod
       use mesh_mod
       use data_location_mod
       use array_mod
       use GF_mod
       use SF_mod
       use VF_mod
       use boundary_conditions_mod
       use ops_aux_mod
       use export_raw_processed_mod
       use constants_mod
       use benchmark_case_mod
       implicit none

       private
       public :: init_U_field

       contains

       subroutine init_U_field(U,m,BMC,dir)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(benchmark_case),intent(in) :: BMC
         integer :: preset_ID
         call assign(U,0.0_cp)
         preset_ID = BMC%VS%U%IC
         if (BMC%VS%U%SS%restart) then
               call restart_U(U,m,dir)
         else
           select case(preset_ID)
           case (0)
           case (1); call FD_duct(U,m,1,1)
           case (2); call isolated_eddy_2D(U,m,3,1) ! Isolated Eddy (Weiss)
           case (3); call single_eddy_2D(U,m,3,1)   ! Single Eddy (Weiss)
           case (4); call cylinder2D(U,m,3,1)     ! Cylinder
           case (5); call parabolic1D(U,m,1,2,1)  ! Bandaru (SS of Ha=0)
           case default; stop 'Error: bad preset_ID in init_P_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_U(U,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: U
         call import_3D_1C(m,U%x,dir,'Uf_x',0)
         call import_3D_1C(m,U%y,dir,'Uf_y',0)
         call import_3D_1C(m,U%z,dir,'Uf_z',0)
       end subroutine

       ! **************************************************************************
       ! ************************ REAL FIELD FUNCTIONS ****************************
       ! **************************************************************************

       subroutine FD_duct(U,m,dir,posNeg)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,posNeg
         integer :: i
         do i=1,m%s
           select case(dir)
           case (1); call fully_developed_duct_velocity(U%x%BF(i)%GF,m%B(i)%g,U%x%DL,dir,posNeg)
           case (2); call fully_developed_duct_velocity(U%y%BF(i)%GF,m%B(i)%g,U%y%DL,dir,posNeg)
           case (3); call fully_developed_duct_velocity(U%z%BF(i)%GF,m%B(i)%g,U%z%DL,dir,posNeg)
           case default; stop 'Error: dir must = 1:3 in FD_duct in distribution_funcs.f90'
           end select
         enddo
       end subroutine

       subroutine parabolic1D(U,m,flow_dir,vary_dir,posNeg)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: flow_dir,vary_dir,posNeg
         integer :: i
         real(cp) :: Re
         Re = 200.0_cp
         if (flow_dir.eq.vary_dir) then
           stop 'Error: flow_dir=vary_dir in parabolic1D in init_U_field.f90'
         endif
         do i=1,m%s
           select case (flow_dir)
           case (1); call parabolic_1D(U%x%BF(i)%GF,m%B(i)%g,U%x%DL,vary_dir)
           case (2); call parabolic_1D(U%y%BF(i)%GF,m%B(i)%g,U%y%DL,vary_dir)
           case (3); call parabolic_1D(U%z%BF(i)%GF,m%B(i)%g,U%z%DL,vary_dir)
           case default; stop 'Error: dir must = 1:3 in parabolic1D in distribution_funcs.f90'
           end select
         enddo
         call multiply(U,Re*real(posNeg,cp))
       end subroutine

       subroutine isolated_eddy_2D(U,m,dir,posNeg)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,posNeg
         integer :: i
         do i=1,m%s
           select case(dir)
           case (1); call isolated_2D_eddy(U%y%BF(i)%GF,U%z%BF(i)%GF,U%y%DL,U%z%DL,m%B(i)%g,dir,posNeg)
           case (2); call isolated_2D_eddy(U%x%BF(i)%GF,U%z%BF(i)%GF,U%x%DL,U%z%DL,m%B(i)%g,dir,posNeg)
           case (3); call isolated_2D_eddy(U%x%BF(i)%GF,U%y%BF(i)%GF,U%x%DL,U%y%DL,m%B(i)%g,dir,posNeg)
           case default; stop 'Error: dir must = 1:3 in isolated_eddy_2D in distribution_funcs.f90'
           end select
         enddo
       end subroutine

       subroutine single_eddy_2D(U,m,dir,posNeg)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,posNeg
         integer :: i
         do i=1,m%s
           select case(dir)
           case (1); call single_2D_eddy(U%y%BF(i)%GF,U%z%BF(i)%GF,m%B(i)%g,U%y%DL,U%z%DL,dir,posNeg)
           case (2); call single_2D_eddy(U%x%BF(i)%GF,U%z%BF(i)%GF,m%B(i)%g,U%x%DL,U%z%DL,dir,posNeg)
           case (3); call single_2D_eddy(U%x%BF(i)%GF,U%y%BF(i)%GF,m%B(i)%g,U%x%DL,U%y%DL,dir,posNeg)
           case default; stop 'Error: dir must = 1:3 in single_eddy_2D in distribution_funcs.f90'
           end select
         enddo
       end subroutine

       subroutine cylinder2D(U,m,dir,posNeg)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,posNeg
         integer :: i
         do i=1,m%s
           select case(dir)
           case (1); call cylinder_2D_velocity(U%y%BF(i)%GF,U%z%BF(i)%GF,m%B(i)%g,U%y%DL,U%z%DL,dir,posNeg)
           case (2); call cylinder_2D_velocity(U%x%BF(i)%GF,U%z%BF(i)%GF,m%B(i)%g,U%x%DL,U%z%DL,dir,posNeg)
           case (3); call cylinder_2D_velocity(U%x%BF(i)%GF,U%y%BF(i)%GF,m%B(i)%g,U%x%DL,U%y%DL,dir,posNeg)
           case default; stop 'Error: dir must = 1:3 in cylinder2D in distribution_funcs.f90'
           end select
         enddo
       end subroutine

       end module
