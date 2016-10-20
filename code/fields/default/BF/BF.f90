      module block_field_mod
        ! Compiler flags: (_PARALLELIZE_BF_PLANE_)
        use current_precision_mod
        use grid_mod
        use bctype_mod
        use data_location_mod
        use GF_mod
        use block_mod
        use data_location_mod
        use procedure_array_mod
        use procedure_array_plane_op_mod
        use boundary_conditions_mod
        implicit none
        private

        public :: block_field
        public :: init,delete,display,print,export,import ! Essentials

        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: init_BCs,init_BC_props

        public :: volume
        public :: cosine_waves
        public :: sine_waves
        public :: random_noise

        public :: square,abs
        public :: insist_amax_lt_tol

        public :: assign_ghost_XPeriodic
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann

        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z

        ! GF_symmetry_error_mod
        public :: symmetry_error_x
        public :: symmetry_error_y
        public :: symmetry_error_z

        public :: symmetry_local_x
        public :: symmetry_local_y
        public :: symmetry_local_z

        ! public :: assign_plane_x
        ! public :: assign_plane_y
        ! public :: assign_plane_z

        public :: cross_product_x
        public :: cross_product_y
        public :: cross_product_z

        type block_field
          type(grid_field) :: GF ! bulk
          type(boundary_conditions) :: BCs
          type(data_location) :: DL
          ! type(stitches) :: st
          type(procedure_array_plane_op) :: PA_assign_ghost_XPeriodic
          type(procedure_array_plane_op) :: PA_assign_wall_Dirichlet
          type(procedure_array_plane_op) :: PA_multiply_wall_Neumann
        end type

       interface init_CC;                module procedure init_CC_BF;                  end interface
       interface init_Face;              module procedure init_Face_BF;                end interface
       interface init_Edge;              module procedure init_Edge_BF;                end interface
       interface init_Node;              module procedure init_Node_BF;                end interface
       interface init;                   module procedure init_copy_BF;                end interface
       interface delete;                 module procedure delete_BF;                   end interface
       interface display;                module procedure display_BF;                  end interface
       interface print;                  module procedure print_BF;                    end interface
       interface export;                 module procedure export_BF;                   end interface
       interface import;                 module procedure import_BF;                   end interface

       interface init_BCs;               module procedure init_BC_val;                 end interface
       interface init_BCs;               module procedure init_BC_block_DL;            end interface
       interface init_BC_props;          module procedure init_BC_props_BF;            end interface

       interface volume;                 module procedure volume_BF;                   end interface
       interface cosine_waves;           module procedure cosine_waves_BF;             end interface
       interface sine_waves;             module procedure sine_waves_BF;               end interface
       interface random_noise;           module procedure random_noise_BF;             end interface
       interface cross_product_x;        module procedure cross_product_x_BF;          end interface
       interface cross_product_y;        module procedure cross_product_y_BF;          end interface
       interface cross_product_z;        module procedure cross_product_z_BF;          end interface

       interface square;                 module procedure square_BF;                   end interface
       interface abs;                    module procedure abs_BF;                      end interface
       interface insist_amax_lt_tol;     module procedure insist_amax_lt_tol_BF;       end interface

       interface assign_ghost_XPeriodic; module procedure assign_ghost_XPeriodic_BF;   end interface
       interface assign_ghost_XPeriodic; module procedure assign_ghost_XPeriodic_BF2;  end interface
       interface assign_wall_Dirichlet;  module procedure assign_wall_Dirichlet_BF;    end interface
       interface assign_wall_Dirichlet;  module procedure assign_wall_Dirichlet_BF2;   end interface
       interface multiply_wall_Neumann;  module procedure multiply_wall_Neumann_BF;    end interface
       interface multiply_wall_Neumann;  module procedure multiply_wall_Neumann_BF2;   end interface

       interface plane_sum_x;            module procedure plane_sum_x_BF;              end interface
       interface plane_sum_y;            module procedure plane_sum_y_BF;              end interface
       interface plane_sum_z;            module procedure plane_sum_z_BF;              end interface

       interface symmetry_error_x;       module procedure symmetry_error_x_BF;         end interface
       interface symmetry_error_y;       module procedure symmetry_error_y_BF;         end interface
       interface symmetry_error_z;       module procedure symmetry_error_z_BF;         end interface

       interface symmetry_local_x;       module procedure symmetry_local_x_BF;         end interface
       interface symmetry_local_y;       module procedure symmetry_local_y_BF;         end interface
       interface symmetry_local_z;       module procedure symmetry_local_z_BF;         end interface

       ! interface assign_plane_x;         module procedure assign_plane_x_BF;           end interface
       ! interface assign_plane_y;         module procedure assign_plane_y_BF;           end interface
       ! interface assign_plane_z;         module procedure assign_plane_z_BF;           end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_CC_BF(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         call init_CC(BF%GF,B%g)
         call init_CC(BF%DL)
         call set_assign_ghost_all_faces(BF)
         call set_assign_wall_Dirichlet(BF)
         call set_multiply_wall_Neumann(BF)
       end subroutine

       subroutine init_Face_BF(BF,B,dir)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         call init_Face(BF%GF,B%g,dir)
         call init_Face(BF%DL,dir)
         call set_assign_ghost_all_faces(BF)
         call set_assign_wall_Dirichlet(BF)
         call set_multiply_wall_Neumann(BF)
       end subroutine

       subroutine init_Edge_BF(BF,B,dir)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         call init_Edge(BF%GF,B%g,dir)
         call init_Edge(BF%DL,dir)
         call set_assign_ghost_all_faces(BF)
         call set_assign_wall_Dirichlet(BF)
         call set_multiply_wall_Neumann(BF)
       end subroutine

       subroutine init_Node_BF(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         call init_Node(BF%GF,B%g)
         call init_Node(BF%DL)
         call set_assign_ghost_all_faces(BF)
         call set_assign_wall_Dirichlet(BF)
         call set_multiply_wall_Neumann(BF)
       end subroutine

       subroutine set_assign_ghost_all_faces(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_assign_ghost_XPeriodic)
         if (BF%BCs%BCL%defined) then
         if(.not.is_Periodic(BF%BCs%face%bct(1)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmin,1);endif
         if(.not.is_Periodic(BF%BCs%face%bct(2)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmax,2);endif
         if(.not.is_Periodic(BF%BCs%face%bct(3)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymin,3);endif
         if(.not.is_Periodic(BF%BCs%face%bct(4)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymax,4);endif
         if(.not.is_Periodic(BF%BCs%face%bct(5)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmin,5);endif
         if(.not.is_Periodic(BF%BCs%face%bct(6)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmax,6);endif
         else
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmin,1)
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmax,2)
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymin,3)
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymax,4)
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmin,5)
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmax,6)
         endif
       end subroutine

       subroutine set_assign_wall_Dirichlet(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_assign_wall_Dirichlet)
         if (BF%BCs%BCL%defined) then
           if (N_along(BF%DL,1).and.(is_Dirichlet(BF%BCs%face%bct(1)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmin,1); endif
           if (N_along(BF%DL,2).and.(is_Dirichlet(BF%BCs%face%bct(2)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmax,2); endif
           if (N_along(BF%DL,3).and.(is_Dirichlet(BF%BCs%face%bct(3)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymin,3); endif
           if (N_along(BF%DL,4).and.(is_Dirichlet(BF%BCs%face%bct(4)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymax,4); endif
           if (N_along(BF%DL,5).and.(is_Dirichlet(BF%BCs%face%bct(5)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmin,5); endif
           if (N_along(BF%DL,6).and.(is_Dirichlet(BF%BCs%face%bct(6)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmax,6); endif
         else
           if (N_along(BF%DL,1)) then; call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmin,1)
                                       call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmax,2)
           endif
           if (N_along(BF%DL,2)) then; call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymin,3)
                                       call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymax,4)
           endif
           if (N_along(BF%DL,3)) then; call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmin,5)
                                       call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmax,6)
           endif
         endif
       end subroutine

       subroutine set_multiply_wall_Neumann(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_multiply_wall_Neumann)
         if (BF%BCs%BCL%defined) then
           if (N_along(BF%DL,1).and.(is_Neumann(BF%BCs%face%bct(1)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmin,1); endif
           if (N_along(BF%DL,2).and.(is_Neumann(BF%BCs%face%bct(2)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmax,2); endif
           if (N_along(BF%DL,3).and.(is_Neumann(BF%BCs%face%bct(3)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymin,3); endif
           if (N_along(BF%DL,4).and.(is_Neumann(BF%BCs%face%bct(4)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymax,4); endif
           if (N_along(BF%DL,5).and.(is_Neumann(BF%BCs%face%bct(5)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmin,5); endif
           if (N_along(BF%DL,6).and.(is_Neumann(BF%BCs%face%bct(6)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmax,6); endif
         else
           if (N_along(BF%DL,1)) then; call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmin,1)
                                       call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmax,2)
           endif
           if (N_along(BF%DL,2)) then; call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymin,3)
                                       call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymax,4)
           endif
           if (N_along(BF%DL,3)) then; call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmin,5)
                                       call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmax,6)
           endif
         endif
       end subroutine

       subroutine init_copy_BF(BF,BF_in)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block_field),intent(in) :: BF_in
         call init(BF%GF,BF_in%GF)
         call init(BF%DL,BF_in%DL)
         if (BF_in%BCs%BCL%defined) call init(BF%BCs,BF_in%BCs)

         call init(BF%PA_assign_ghost_XPeriodic,BF_in%PA_assign_ghost_XPeriodic)
         if (BF%PA_assign_wall_Dirichlet%defined) call init(BF%PA_assign_wall_Dirichlet,BF_in%PA_assign_wall_Dirichlet)
         if (BF%PA_multiply_wall_Neumann%defined) call init(BF%PA_multiply_wall_Neumann,BF_in%PA_multiply_wall_Neumann)
       end subroutine

       subroutine delete_BF(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%GF)
         call delete(BF%DL)
         call delete(BF%BCs)

         call delete(BF%PA_assign_ghost_XPeriodic)
         call delete(BF%PA_assign_wall_Dirichlet)
         call delete(BF%PA_multiply_wall_Neumann)
       end subroutine

       subroutine display_BF(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call display(BF%GF,un)
         call display(BF%DL,un)
         call display(BF%BCs,un)
       end subroutine

       subroutine print_BF(BF)
         implicit none
         type(block_field),intent(in) :: BF
         call print(BF%GF)
         call print(BF%DL)
         call print(BF%BCs)
       end subroutine

       subroutine export_BF(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call export(BF%GF,un)
         call export(BF%DL,un)
         call export(BF%BCs,un)
       end subroutine

       subroutine import_BF(BF,un)
         implicit none
         type(block_field),intent(inout) :: BF
         integer,intent(in) :: un
         call import(BF%GF,un)
         call import(BF%DL,un)
         call import(BF%BCs,un)
       end subroutine

       subroutine init_BC_val(BF,val)
         implicit none
         type(block_field),intent(inout) :: BF
         real(cp),intent(in) :: val
         call init(BF%BCs,val)
       end subroutine

       subroutine init_BC_block_DL(BF,B,DL)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         call init(BF%BCs,B,DL)
       end subroutine

       subroutine init_BC_props_BF(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call init_props(BF%BCs)
       end subroutine

       subroutine volume_BF(u,B,DL) ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         call volume(u%GF,B%g,DL)
       end subroutine

       subroutine sine_waves_BF(u,B,wavenum,phi,DL)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         real(cp),dimension(3),intent(in) :: wavenum,phi
         call sine_waves(u%GF,B%g,wavenum,phi,DL)
       end subroutine

       subroutine cosine_waves_BF(u,B,wavenum,phi,DL)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         real(cp),dimension(3),intent(in) :: wavenum,phi
         call cosine_waves(u%GF,B%g,wavenum,phi,DL)
       end subroutine

       subroutine random_noise_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call random_noise(u%GF)
       end subroutine

       subroutine cross_product_x_BF(AcrossB,Ay,Az,By,Bz)
         implicit none
         type(block_field),intent(inout) :: AcrossB
         type(block_field),intent(in) :: Ay,Az,By,Bz
         call cross_product_x(AcrossB%GF,Ay%GF,Az%GF,By%GF,Bz%GF)
       end subroutine
       subroutine cross_product_y_BF(AcrossB,Ax,Az,Bx,Bz)
         implicit none
         type(block_field),intent(inout) :: AcrossB
         type(block_field),intent(in) :: Ax,Az,Bx,Bz
         call cross_product_y(AcrossB%GF,Ax%GF,Az%GF,Bx%GF,Bz%GF)
       end subroutine
       subroutine cross_product_z_BF(AcrossB,Ax,Ay,Bx,By)
         implicit none
         type(block_field),intent(inout) :: AcrossB
         type(block_field),intent(in) :: Ax,Ay,Bx,By
         call cross_product_z(AcrossB%GF,Ax%GF,Ay%GF,Bx%GF,By%GF)
       end subroutine

       subroutine square_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call square(u%GF)
       end subroutine

       subroutine abs_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call abs(u%GF)
       end subroutine

       subroutine insist_amax_lt_tol_BF(u,caller)
         implicit none
         type(block_field),intent(in) :: u
         character(len=*),intent(in) :: caller
         call insist_amax_lt_tol(u%GF,caller)
       end subroutine

       subroutine assign_ghost_XPeriodic_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_ghost_XPeriodic%N
         call u%PA_assign_ghost_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine assign_ghost_XPeriodic_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         type(block_field),intent(in) :: u_with_BCs
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_ghost_XPeriodic%N
         call u_with_BCs%PA_assign_ghost_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine assign_wall_Dirichlet_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_wall_Dirichlet%N
         call u%PA_assign_wall_Dirichlet%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine assign_wall_Dirichlet_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: u_with_BCs
         real(cp),intent(in) :: val
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_wall_Dirichlet%N
         call u_with_BCs%PA_assign_wall_Dirichlet%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine multiply_wall_Neumann_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_multiply_wall_Neumann%N
         call u%PA_multiply_wall_Neumann%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine multiply_wall_Neumann_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: u_with_BCs
         real(cp),intent(in) :: val
         integer :: i
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_multiply_wall_Neumann%N
         call u_with_BCs%PA_multiply_wall_Neumann%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
       end subroutine

       function plane_sum_x_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_x(u%GF,B%g,p)
       end function

       function plane_sum_y_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_y(u%GF,B%g,p)
       end function

       function plane_sum_z_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_z(u%GF,B%g,p)
       end function

       function symmetry_error_x_BF(u) result(SE)
         implicit none
         type(block_field),intent(in) :: u
         real(cp) :: SE
         SE = symmetry_error_x(u%GF)
       end function

       function symmetry_error_y_BF(u) result(SE)
         implicit none
         type(block_field),intent(in) :: u
         real(cp) :: SE
         SE = symmetry_error_y(u%GF)
       end function

       function symmetry_error_z_BF(u) result(SE)
         implicit none
         type(block_field),intent(in) :: u
         real(cp) :: SE
         SE = symmetry_error_z(u%GF)
       end function

       subroutine symmetry_local_x_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call symmetry_local_x(u%GF)
       end subroutine

       subroutine symmetry_local_y_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call symmetry_local_y(u%GF)
       end subroutine

       subroutine symmetry_local_z_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call symmetry_local_z(u%GF)
       end subroutine

      end module