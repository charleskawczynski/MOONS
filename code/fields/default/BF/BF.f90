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
        use face_edge_corner_indexing_mod
        use boundary_conditions_mod
        use GF_curl_curl_mod
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

        public :: square,square_root,abs
        public :: insist_amax_lt_tol

        public :: assign_BCs
        public :: assign_BC_vals
        public :: assign_Neumann_BCs
        public :: assign_Dirichlet_BCs
        public :: assign_Periodic_BCs
        public :: multiply_Neumann_BCs
        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Periodic_single
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann
        public :: set_prescribed_BCs

        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z
        public :: boundary_flux

        public :: mirror_about_hmin,mirror_about_hmax

        public :: assign_ghost_xmin_xmax
        public :: assign_ghost_ymin_ymax
        public :: assign_ghost_zmin_zmax

        public :: symmetry_error_x
        public :: symmetry_error_y
        public :: symmetry_error_z

        public :: symmetry_local_x
        public :: symmetry_local_y
        public :: symmetry_local_z

        public :: cross_product_x
        public :: cross_product_y
        public :: cross_product_z

        public :: CFL_number

        public :: restrict
        public :: prolongate

        public :: laplacian_matrix_based
        public :: curl_curl_matrix_based

        type block_field
          type(grid_field) :: GF ! bulk
          type(boundary_conditions) :: BCs
          type(data_location) :: DL
          ! type(stitches) :: st
          type(procedure_array_plane_op) :: PA_assign_ghost_XPeriodic
          type(procedure_array_plane_op) :: PA_assign_ghost_N_XPeriodic
          type(procedure_array_plane_op) :: PA_assign_wall_Dirichlet
          type(procedure_array_plane_op) :: PA_assign_wall_Periodic_single
          type(procedure_array_plane_op) :: PA_multiply_wall_Neumann
        end type

       interface init_CC;                     module procedure init_CC_BF;                      end interface
       interface init_Face;                   module procedure init_Face_BF;                    end interface
       interface init_Edge;                   module procedure init_Edge_BF;                    end interface
       interface init_Node;                   module procedure init_Node_BF;                    end interface
       interface init;                        module procedure init_copy_BF;                    end interface
       interface delete;                      module procedure delete_BF;                       end interface
       interface display;                     module procedure display_BF;                      end interface
       interface print;                       module procedure print_BF;                        end interface
       interface export;                      module procedure export_BF;                       end interface
       interface import;                      module procedure import_BF;                       end interface

       interface init_BCs;                    module procedure init_BC_val;                     end interface
       interface init_BCs;                    module procedure init_BC_block_DL;                end interface
       interface init_BC_props;               module procedure init_BC_props_BF;                end interface

       interface volume;                      module procedure volume_DL_BF;                    end interface
       interface volume;                      module procedure volume_BF;                       end interface
       interface cosine_waves;                module procedure cosine_waves_BF;                 end interface
       interface sine_waves;                  module procedure sine_waves_BF;                   end interface
       interface random_noise;                module procedure random_noise_BF;                 end interface
       interface random_noise;                module procedure random_noise_BF_dir;             end interface
       interface cross_product_x;             module procedure cross_product_x_BF;              end interface
       interface cross_product_y;             module procedure cross_product_y_BF;              end interface
       interface cross_product_z;             module procedure cross_product_z_BF;              end interface

       interface CFL_number;                  module procedure CFL_number_BF;                   end interface

       interface square;                      module procedure square_BF;                       end interface
       interface square_root;                 module procedure square_root_BF;                  end interface
       interface abs;                         module procedure abs_BF;                          end interface
       interface insist_amax_lt_tol;          module procedure insist_amax_lt_tol_BF;           end interface

       interface assign_BCs;                  module procedure assign_BCs_BF;                   end interface
       interface assign_BC_vals;              module procedure assign_BC_vals_BF;               end interface
       interface assign_Neumann_BCs;          module procedure assign_Neumann_BCs_BF;           end interface
       interface assign_Dirichlet_BCs;        module procedure assign_Dirichlet_BCs_BF;         end interface
       interface assign_Periodic_BCs;         module procedure assign_Periodic_BCs_BF;          end interface
       interface multiply_Neumann_BCs;        module procedure multiply_Neumann_BCs_BF;         end interface
       interface assign_ghost_XPeriodic;      module procedure assign_ghost_XPeriodic_BF;       end interface
       interface assign_ghost_XPeriodic;      module procedure assign_ghost_XPeriodic_BF2;      end interface
       interface assign_ghost_N_XPeriodic;    module procedure assign_ghost_N_XPeriodic_BF;     end interface
       interface assign_ghost_N_XPeriodic;    module procedure assign_ghost_N_XPeriodic_BF2;    end interface
       interface assign_wall_Periodic_single; module procedure assign_wall_Periodic_single_BF;  end interface
       interface assign_wall_Periodic_single; module procedure assign_wall_Periodic_single_BF2; end interface
       interface assign_wall_Dirichlet;       module procedure assign_wall_Dirichlet_BF;        end interface
       interface assign_wall_Dirichlet;       module procedure assign_wall_Dirichlet_BF2;       end interface
       interface multiply_wall_Neumann;       module procedure multiply_wall_Neumann_BF;        end interface
       interface multiply_wall_Neumann;       module procedure multiply_wall_Neumann_BF2;       end interface
       interface set_prescribed_BCs;          module procedure set_prescribed_BCs_BF;           end interface

       interface plane_sum_x;                 module procedure plane_sum_x_BF;                  end interface
       interface plane_sum_y;                 module procedure plane_sum_y_BF;                  end interface
       interface plane_sum_z;                 module procedure plane_sum_z_BF;                  end interface
       interface boundary_flux;               module procedure boundary_flux_BF;                end interface

       interface assign_ghost_xmin_xmax;      module procedure assign_ghost_xmin_xmax_BF;       end interface
       interface assign_ghost_ymin_ymax;      module procedure assign_ghost_ymin_ymax_BF;       end interface
       interface assign_ghost_zmin_zmax;      module procedure assign_ghost_zmin_zmax_BF;       end interface

       interface mirror_about_hmin;           module procedure mirror_about_hmin_BF;            end interface
       interface mirror_about_hmax;           module procedure mirror_about_hmax_BF;            end interface

       interface symmetry_error_x;            module procedure symmetry_error_x_BF;             end interface
       interface symmetry_error_y;            module procedure symmetry_error_y_BF;             end interface
       interface symmetry_error_z;            module procedure symmetry_error_z_BF;             end interface

       interface symmetry_local_x;            module procedure symmetry_local_x_BF;             end interface
       interface symmetry_local_y;            module procedure symmetry_local_y_BF;             end interface
       interface symmetry_local_z;            module procedure symmetry_local_z_BF;             end interface

       interface restrict;                    module procedure restrict_BF;                     end interface
       interface restrict;                    module procedure restrict_reset_BF;               end interface
       interface prolongate;                  module procedure prolongate_BF;                   end interface
       interface prolongate;                  module procedure prolongate_reset_BF;             end interface

       interface laplacian_matrix_based;      module procedure laplacian_matrix_based_VF_BF;    end interface
       interface laplacian_matrix_based;      module procedure laplacian_matrix_based_SF_BF;    end interface
       interface curl_curl_matrix_based;      module procedure curl_curl_matrix_based_BF;       end interface

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
         call set_assign_wall_Periodic_single_BF(BF)
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
         call set_assign_wall_Periodic_single_BF(BF)
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
         call set_assign_wall_Periodic_single_BF(BF)
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
         call set_assign_wall_Periodic_single_BF(BF)
       end subroutine

       subroutine set_assign_ghost_all_faces(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_assign_ghost_XPeriodic)
         call delete(BF%PA_assign_ghost_N_XPeriodic)
         if (defined(BF%BCs)) then
         if(.not.is_Periodic(BF%BCs%face%bct(1)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmin,1)
         if (N_along(BF%DL,1)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_xmin,1)
         endif
         if(.not.is_Periodic(BF%BCs%face%bct(2)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmax,2)
         if (N_along(BF%DL,1)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_xmax,2)
         endif
         if(.not.is_Periodic(BF%BCs%face%bct(3)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymin,3)
         if (N_along(BF%DL,2)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_ymin,3)
         endif
         if(.not.is_Periodic(BF%BCs%face%bct(4)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymax,4)
         if (N_along(BF%DL,2)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_ymax,4)
         endif
         if(.not.is_Periodic(BF%BCs%face%bct(5)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmin,5)
         if (N_along(BF%DL,3)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_zmin,5)
         endif
         if(.not.is_Periodic(BF%BCs%face%bct(6)))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmax,6)
         if (N_along(BF%DL,3)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_zmax,6)
         endif
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
         if (defined(BF%BCs)) then
           if (N_along(BF%DL,1).and.(is_Dirichlet(BF%BCs%face%bct(1)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmin,1)
           endif
           if (N_along(BF%DL,1).and.(is_Dirichlet(BF%BCs%face%bct(2)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmax,2)
           endif
           if (N_along(BF%DL,2).and.(is_Dirichlet(BF%BCs%face%bct(3)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymin,3)
           endif
           if (N_along(BF%DL,2).and.(is_Dirichlet(BF%BCs%face%bct(4)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymax,4)
           endif
           if (N_along(BF%DL,3).and.(is_Dirichlet(BF%BCs%face%bct(5)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmin,5)
           endif
           if (N_along(BF%DL,3).and.(is_Dirichlet(BF%BCs%face%bct(6)))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmax,6)
           endif
         else
           if (N_along(BF%DL,1)) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmin,1)
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmax,2)
           endif
           if (N_along(BF%DL,2)) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymin,3)
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymax,4)
           endif
           if (N_along(BF%DL,3)) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmin,5)
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmax,6)
           endif
         endif
       end subroutine

       subroutine set_assign_wall_Periodic_single_BF(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         logical,dimension(4) :: L
         call delete(BF%PA_assign_wall_Periodic_single)
         if (defined(BF%BCs)) then
           L(1) = N_along(BF%DL,1)
           L(2) = is_Periodic(BF%BCs%face%bct(1))
           L(3) = is_Periodic(BF%BCs%face%bct(2))
           L(4) = BF%GF%s(1).gt.4
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_xmax,2)

           L(1) = N_along(BF%DL,2)
           L(2) = is_Periodic(BF%BCs%face%bct(3))
           L(3) = is_Periodic(BF%BCs%face%bct(4))
           L(4) = BF%GF%s(2).gt.4
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_ymax,4)

           L(1) = N_along(BF%DL,3)
           L(2) = is_Periodic(BF%BCs%face%bct(5))
           L(3) = is_Periodic(BF%BCs%face%bct(6))
           L(4) = BF%GF%s(3).gt.4
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_zmax,6)
         endif
       end subroutine

       subroutine set_multiply_wall_Neumann(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_multiply_wall_Neumann)
         if (defined(BF%BCs)) then
           if (N_along(BF%DL,1).and.(is_Neumann(BF%BCs%face%bct(1)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmin,1); endif
           if (N_along(BF%DL,1).and.(is_Neumann(BF%BCs%face%bct(2)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmax,2); endif
           if (N_along(BF%DL,2).and.(is_Neumann(BF%BCs%face%bct(3)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymin,3); endif
           if (N_along(BF%DL,2).and.(is_Neumann(BF%BCs%face%bct(4)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymax,4); endif
           if (N_along(BF%DL,3).and.(is_Neumann(BF%BCs%face%bct(5)))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmin,5); endif
           if (N_along(BF%DL,3).and.(is_Neumann(BF%BCs%face%bct(6)))) then
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
         call init(BF%PA_assign_ghost_N_XPeriodic,BF_in%PA_assign_ghost_N_XPeriodic)
         call init(BF%PA_assign_wall_Dirichlet,BF_in%PA_assign_wall_Dirichlet)
         call init(BF%PA_assign_wall_Periodic_single,BF_in%PA_assign_wall_Periodic_single)
         call init(BF%PA_multiply_wall_Neumann,BF_in%PA_multiply_wall_Neumann)
       end subroutine

       subroutine delete_BF(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%GF)
         call delete(BF%DL)
         call delete(BF%BCs)
         call delete(BF%PA_assign_ghost_XPeriodic)
         call delete(BF%PA_assign_ghost_N_XPeriodic)
         call delete(BF%PA_assign_wall_Dirichlet)
         call delete(BF%PA_assign_wall_Periodic_single)
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
         call set_assign_ghost_all_faces(BF)
         call set_assign_wall_Dirichlet(BF)
         call set_multiply_wall_Neumann(BF)
         call set_assign_wall_Periodic_single_BF(BF)
       end subroutine

       subroutine volume_DL_BF(u,B,DL) ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         call volume(u%GF,B%g,DL)
       end subroutine

       subroutine volume_BF(u,B) ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         call volume(u%GF,B%g)
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

       subroutine random_noise_BF_dir(u,dir)
         implicit none
         type(block_field),intent(inout) :: u
         integer,intent(in) :: dir
         call random_noise(u%GF,dir)
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

       function CFL_number_BF(U_CC,V_CC,W_CC,B,dt) result(CFL)
         implicit none
         type(block_field),intent(in) :: U_CC,V_CC,W_CC
         type(block),intent(in) :: B
         real(cp),intent(in) :: dt
         real(cp) :: CFL
         CFL = CFL_number(U_CC%GF,V_CC%GF,W_CC%GF,B%g,dt)
       end function

       subroutine square_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call square(u%GF)
       end subroutine

       subroutine square_root_BF(u)
         implicit none
         type(block_field),intent(inout) :: u
         call square_root(u%GF)
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

       subroutine assign_BCs_BF(u,f)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: f
         if (defined(u%BCs)) then
           call assign_plane(u%BCs%face%b(1),f%GF,1,     2     ,1)
           call assign_plane(u%BCs%face%b(2),f%GF,1,f%GF%s(1)-1,1)
           call assign_plane(u%BCs%face%b(3),f%GF,1,     2     ,2)
           call assign_plane(u%BCs%face%b(4),f%GF,1,f%GF%s(2)-1,2)
           call assign_plane(u%BCs%face%b(5),f%GF,1,     2     ,3)
           call assign_plane(u%BCs%face%b(6),f%GF,1,f%GF%s(3)-1,3)
         endif
       end subroutine

       subroutine assign_BC_vals_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
#ifdef __DEBUG_BF__
         if (.not.(defined(A%BCs).and.defined(B%BCs))) stop 'Error: BCs not defined in BF.f90'
#endif
         call assign(A%BCs%face%b(1),B%BCs%face%b(1))
         call assign(A%BCs%face%b(2),B%BCs%face%b(2))
         call assign(A%BCs%face%b(3),B%BCs%face%b(3))
         call assign(A%BCs%face%b(4),B%BCs%face%b(4))
         call assign(A%BCs%face%b(5),B%BCs%face%b(5))
         call assign(A%BCs%face%b(6),B%BCs%face%b(6))
       end subroutine

       subroutine assign_Neumann_BCs_BF(u,f)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: f
         if (defined(u%BCs)) then
           if (is_Neumann(u%BCs%face%bct(1))) call assign_plane(u%BCs%face%b(1),f%GF,1,     2     ,1)
           if (is_Neumann(u%BCs%face%bct(2))) call assign_plane(u%BCs%face%b(2),f%GF,1,f%GF%s(1)-1,1)
           if (is_Neumann(u%BCs%face%bct(3))) call assign_plane(u%BCs%face%b(3),f%GF,1,     2     ,2)
           if (is_Neumann(u%BCs%face%bct(4))) call assign_plane(u%BCs%face%b(4),f%GF,1,f%GF%s(2)-1,2)
           if (is_Neumann(u%BCs%face%bct(5))) call assign_plane(u%BCs%face%b(5),f%GF,1,     2     ,3)
           if (is_Neumann(u%BCs%face%bct(6))) call assign_plane(u%BCs%face%b(6),f%GF,1,f%GF%s(3)-1,3)
         endif
       end subroutine

       subroutine assign_Dirichlet_BCs_BF(u,f)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: f
         if (defined(u%BCs)) then
           if (is_Dirichlet(u%BCs%face%bct(1))) call assign_plane(u%BCs%face%b(1),f%GF,1,     2     ,1)
           if (is_Dirichlet(u%BCs%face%bct(2))) call assign_plane(u%BCs%face%b(2),f%GF,1,f%GF%s(1)-1,1)
           if (is_Dirichlet(u%BCs%face%bct(3))) call assign_plane(u%BCs%face%b(3),f%GF,1,     2     ,2)
           if (is_Dirichlet(u%BCs%face%bct(4))) call assign_plane(u%BCs%face%b(4),f%GF,1,f%GF%s(2)-1,2)
           if (is_Dirichlet(u%BCs%face%bct(5))) call assign_plane(u%BCs%face%b(5),f%GF,1,     2     ,3)
           if (is_Dirichlet(u%BCs%face%bct(6))) call assign_plane(u%BCs%face%b(6),f%GF,1,f%GF%s(3)-1,3)
         endif
       end subroutine

       subroutine assign_Periodic_BCs_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer,dimension(3) :: p,i_opp_e,i_opp_s
         if (defined(A%BCs)) then
           p = N_eye(A%DL)
           i_opp_e = B%GF%s-1-p
           i_opp_s = 2+p
           if (is_Periodic(A%BCs%face%bct(1))) call assign_plane(A%BCs%face%b(1),B%GF,1,i_opp_e(1),1)
           if (is_Periodic(A%BCs%face%bct(2))) call assign_plane(A%BCs%face%b(2),B%GF,1,i_opp_s(1),1)
           if (is_Periodic(A%BCs%face%bct(3))) call assign_plane(A%BCs%face%b(3),B%GF,1,i_opp_e(2),2)
           if (is_Periodic(A%BCs%face%bct(4))) call assign_plane(A%BCs%face%b(4),B%GF,1,i_opp_s(2),2)
           if (is_Periodic(A%BCs%face%bct(5))) call assign_plane(A%BCs%face%b(5),B%GF,1,i_opp_e(3),3)
           if (is_Periodic(A%BCs%face%bct(6))) call assign_plane(A%BCs%face%b(6),B%GF,1,i_opp_s(3),3)
         endif
       end subroutine

       subroutine multiply_Neumann_BCs_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         if (defined(u%BCs)) then
           if (is_Neumann(u%BCs%face%bct(1))) call multiply(u%BCs%face%b(1),val)
           if (is_Neumann(u%BCs%face%bct(2))) call multiply(u%BCs%face%b(2),val)
           if (is_Neumann(u%BCs%face%bct(3))) call multiply(u%BCs%face%b(3),val)
           if (is_Neumann(u%BCs%face%bct(4))) call multiply(u%BCs%face%b(4),val)
           if (is_Neumann(u%BCs%face%bct(5))) call multiply(u%BCs%face%b(5),val)
           if (is_Neumann(u%BCs%face%bct(6))) call multiply(u%BCs%face%b(6),val)
         endif
       end subroutine

       subroutine set_prescribed_BCs_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         if (defined(A%BCs)) then
           if (is_Periodic(A%BCs%face%bct(1))) call set_prescribed(A%BCs%face%bct(1))
           if (is_Periodic(A%BCs%face%bct(2))) call set_prescribed(A%BCs%face%bct(2))
           if (is_Periodic(A%BCs%face%bct(3))) call set_prescribed(A%BCs%face%bct(3))
           if (is_Periodic(A%BCs%face%bct(4))) call set_prescribed(A%BCs%face%bct(4))
           if (is_Periodic(A%BCs%face%bct(5))) call set_prescribed(A%BCs%face%bct(5))
           if (is_Periodic(A%BCs%face%bct(6))) call set_prescribed(A%BCs%face%bct(6))

           if (is_Periodic(A%BCs%face%bct(1))) call init_Periodic(A%BCs,1)
           if (is_Periodic(A%BCs%face%bct(2))) call init_Periodic(A%BCs,2)
           if (is_Periodic(A%BCs%face%bct(3))) call init_Periodic(A%BCs,3)
           if (is_Periodic(A%BCs%face%bct(4))) call init_Periodic(A%BCs,4)
           if (is_Periodic(A%BCs%face%bct(5))) call init_Periodic(A%BCs,5)
           if (is_Periodic(A%BCs%face%bct(6))) call init_Periodic(A%BCs,6)
         endif
       end subroutine

       subroutine assign_ghost_XPeriodic_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
         if (u%PA_assign_ghost_XPeriodic%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_ghost_XPeriodic%N
         call u%PA_assign_ghost_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine
       subroutine assign_ghost_XPeriodic_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         type(block_field),intent(in) :: u_with_BCs
         integer :: i
         if (u_with_BCs%PA_assign_ghost_XPeriodic%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_ghost_XPeriodic%N
         call u_with_BCs%PA_assign_ghost_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_ghost_N_XPeriodic_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
         if (u%PA_assign_ghost_N_XPeriodic%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_ghost_N_XPeriodic%N
         call u%PA_assign_ghost_N_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine
       subroutine assign_ghost_N_XPeriodic_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         type(block_field),intent(in) :: u_with_BCs
         integer :: i
         if (u_with_BCs%PA_assign_ghost_N_XPeriodic%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_ghost_N_XPeriodic%N
         call u_with_BCs%PA_assign_ghost_N_XPeriodic%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_wall_Dirichlet_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
         if (u%PA_assign_wall_Dirichlet%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_wall_Dirichlet%N
         call u%PA_assign_wall_Dirichlet%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_wall_Dirichlet_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: u_with_BCs
         real(cp),intent(in) :: val
         integer :: i
         if (u_with_BCs%PA_assign_wall_Dirichlet%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_wall_Dirichlet%N
         call u_with_BCs%PA_assign_wall_Dirichlet%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_wall_Periodic_single_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
         if (u%PA_assign_wall_Periodic_single%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_assign_wall_Periodic_single%N
         call u%PA_assign_wall_Periodic_single%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_wall_Periodic_single_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: u_with_BCs
         real(cp),intent(in) :: val
         integer :: i
         if (u_with_BCs%PA_assign_wall_Periodic_single%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_assign_wall_Periodic_single%N
         call u_with_BCs%PA_assign_wall_Periodic_single%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine multiply_wall_Neumann_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         integer :: i
         if (u%PA_multiply_wall_Neumann%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u%PA_multiply_wall_Neumann%N
         call u%PA_multiply_wall_Neumann%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine multiply_wall_Neumann_BF2(u,val,u_with_BCs)
         implicit none
         type(block_field),intent(inout) :: u
         type(block_field),intent(in) :: u_with_BCs
         real(cp),intent(in) :: val
         integer :: i
         if (u_with_BCs%PA_multiply_wall_Neumann%defined) then
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP PARALLEL DO

#endif
         do i=1,u_with_BCs%PA_multiply_wall_Neumann%N
         call u_with_BCs%PA_multiply_wall_Neumann%SP(i)%P(u%GF,val)
         enddo
#ifdef _PARALLELIZE_BF_PLANE_
         !$OMP END PARALLEL DO

#endif
         endif
       end subroutine

       subroutine assign_ghost_xmin_xmax_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         call assign_ghost_xmin_xmax(u%GF,val)
       end subroutine
       subroutine assign_ghost_ymin_ymax_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         call assign_ghost_ymin_ymax(u%GF,val)
       end subroutine
       subroutine assign_ghost_zmin_zmax_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         call assign_ghost_zmin_zmax(u%GF,val)
       end subroutine

       subroutine mirror_about_hmin_BF(u,dir,mirror_sign)
         implicit none
         type(block_field),intent(inout) :: u
         integer,intent(in) :: dir
         real(cp),intent(in) :: mirror_sign
         integer,dimension(3) :: N_along_dir
         N_along_dir = N_eye(u%DL)
         call mirror_about_hmin(u%GF,dir,mirror_sign,N_along_dir(dir))
       end subroutine
       subroutine mirror_about_hmax_BF(u,dir,mirror_sign)
         implicit none
         type(block_field),intent(inout) :: u
         integer,intent(in) :: dir
         real(cp),intent(in) :: mirror_sign
         integer,dimension(3) :: N_along_dir
         N_along_dir = N_eye(u%DL)
         call mirror_about_hmax(u%GF,dir,mirror_sign,N_along_dir(dir))
       end subroutine

       function plane_sum_x_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_x(u%GF,B%g,p,1.0_cp)
       end function

       function plane_sum_y_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_y(u%GF,B%g,p,1.0_cp)
       end function

       function plane_sum_z_BF(u,B,p) result(PS)
         implicit none
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: p
         real(cp) :: PS
         PS = plane_sum_z(u%GF,B%g,p,1.0_cp)
       end function

       function boundary_flux_BF(x,y,z,B) result(BF)
         implicit none
         type(block_field),intent(in) :: x,y,z
         type(block),intent(in) :: B
         type(block_field) :: temp_x,temp_y,temp_z
         real(cp) :: BF
         logical,dimension(3) :: L
         L(1) = is_Face(x%DL).and.(get_Face(x%DL).eq.1)
         L(2) = is_Face(y%DL).and.(get_Face(y%DL).eq.2)
         L(3) = is_Face(z%DL).and.(get_Face(z%DL).eq.3)
         if (all(L)) then
         BF = 0.0_cp
         call init(temp_x,x); call assign(temp_x%GF,x%GF)
         call init(temp_y,y); call assign(temp_y%GF,y%GF)
         call init(temp_z,z); call assign(temp_z%GF,z%GF)
         call assign_ghost_XPeriodic(temp_x,0.0_cp)
         call assign_ghost_XPeriodic(temp_y,0.0_cp)
         call assign_ghost_XPeriodic(temp_z,0.0_cp)
         BF = BF + plane_sum_x(temp_x%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_x(temp_x%GF,B%g,temp_x%GF%s(1)-1,1.0_cp)
         BF = BF + plane_sum_y(temp_y%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_y(temp_y%GF,B%g,temp_y%GF%s(2)-1,1.0_cp)
         BF = BF + plane_sum_z(temp_z%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_z(temp_z%GF,B%g,temp_z%GF%s(3)-1,1.0_cp)
         call delete(temp_x)
         call delete(temp_y)
         call delete(temp_z)
         else; stop 'Error: boundary flux only offered for face data in BF.f90'
         endif
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

       subroutine restrict_BF(r,u,B,dir)
         implicit none
         type(block_field),intent(inout) :: r
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (CC_along(u%DL,dir))    then; call restrict_C(r%GF,u%GF,B%g,dir,x,y,z)
         elseif (N_along(u%DL,dir)) then; call restrict_N(r%GF,u%GF,B%g,dir,x,y,z)
         else; stop 'Error: bad DL in restrict_BF in BF.f90'
         endif
       end subroutine

       subroutine restrict_reset_BF(u,B,dir)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: i,x,y,z
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (CC_along(u%DL,dir))    then; call restrict_C(u%GF,B%g,dir,x,y,z)
         elseif (N_along(u%DL,dir)) then; call restrict_N(u%GF,B%g,dir,x,y,z)
         else; stop 'Error: bad DL in restrict_BF in BF.f90'
         endif

         if (defined(u%BCs)) then
         do i=1,6
         if (dir_given_face(i).ne.dir) then ! only restrict BCs along surface tangent directions
           if (CC_along(u%DL,dir))    then; call restrict_C(u%BCs%face%b(i),B%fb(i),dir,x,y,z)
           elseif (N_along(u%DL,dir)) then; call restrict_N(u%BCs%face%b(i),B%fb(i),dir,x,y,z)
           else; stop 'Error: bad DL in restrict_BF in BF.f90'
           endif
         endif
         enddo
         call restrict(u%BCs,B,dir)
         call init_BC_props(u)
         endif
       end subroutine

       subroutine prolongate_BF(r,u,B,dir)
         implicit none
         type(block_field),intent(inout) :: r
         type(block_field),intent(in) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (CC_along(u%DL,dir))    then; call prolongate_C(r%GF,u%GF,B%g,dir,x,y,z)
         elseif (N_along(u%DL,dir)) then; call prolongate_N(r%GF,u%GF,x,y,z)
         else; stop 'Error: bad DL in prolongate_BF in BF.f90'
         endif
       end subroutine

       subroutine prolongate_reset_BF(u,B,dir)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         integer :: x,y,z
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
             if (CC_along(u%DL,dir)) then; call prolongate_C(u%GF,B%g,dir,x,y,z)
         elseif ( N_along(u%DL,dir)) then; call prolongate_N(u%GF,dir,x,y,z)
         else; stop 'Error: bad DL in prolongate_BF in BF.f90'
         endif
         if (defined(u%BCs)) then
           call prolongate(u%BCs,B,dir)
           call init_BC_props(u)
         endif
       end subroutine

       subroutine laplacian_matrix_based_VF_BF(lapX,lapY,lapZ,X,Y,Z,B)
         implicit none
         type(block_field),intent(inout) :: lapX,lapY,lapZ
         type(block_field),intent(in) :: X,Y,Z
         type(block),intent(in) :: B
         call laplacian(lapX%GF,X%GF,B%lap_VF(1)%S(1:3)%SF%L,B%lap_VF(1)%D_3D,B%lap_VF(1)%S(1:3)%SF%U)
         call laplacian(lapY%GF,Y%GF,B%lap_VF(2)%S(1:3)%SF%L,B%lap_VF(2)%D_3D,B%lap_VF(2)%S(1:3)%SF%U)
         call laplacian(lapZ%GF,Z%GF,B%lap_VF(3)%S(1:3)%SF%L,B%lap_VF(3)%D_3D,B%lap_VF(3)%S(1:3)%SF%U)
       end subroutine

       subroutine laplacian_matrix_based_SF_BF(lap,U,B)
         implicit none
         type(block_field),intent(inout) :: lap
         type(block_field),intent(in) :: U
         type(block),intent(in) :: B
         call laplacian(lap%GF,U%GF,B%lap_SF%S(1:3)%SF%L,B%lap_SF%D_3D,B%lap_SF%S(1:3)%SF%U)
       end subroutine

       subroutine curl_curl_matrix_based_BF(CX,CY,CZ,X,Y,Z,B)
         implicit none
         type(block_field),intent(inout) :: CX,CY,CZ
         type(block_field),intent(in) :: X,Y,Z
         type(block),intent(in) :: B
         call curl_curl_x(CX%GF,X%GF,Y%GF,Z%GF,&
         B%curl_curlX(1)%D_3D,&
         B%curl_curlX(1)%S(1:3)%SF%L,&
         B%curl_curlX(1)%S(1:3)%SF%U,&
         B%curl_curlY(1)%D1_D2,B%curl_curlZ(1)%D1_D2,&
         B%curl_curlY(1)%D1_U2,B%curl_curlZ(1)%D1_U2,&
         B%curl_curlY(1)%U1_D2,B%curl_curlZ(1)%U1_D2,&
         B%curl_curlY(1)%U1_U2,B%curl_curlZ(1)%U1_U2)
         call curl_curl_y(CY%GF,X%GF,Y%GF,Z%GF,&
         B%curl_curlY(2)%D_3D,&
         B%curl_curlY(2)%S(1:3)%SF%L,&
         B%curl_curlY(2)%S(1:3)%SF%U,&
         B%curl_curlX(2)%D1_D2,B%curl_curlZ(2)%D1_D2,&
         B%curl_curlX(2)%D1_U2,B%curl_curlZ(2)%D1_U2,&
         B%curl_curlX(2)%U1_D2,B%curl_curlZ(2)%U1_D2,&
         B%curl_curlX(2)%U1_U2,B%curl_curlZ(2)%U1_U2)
         call curl_curl_z(CZ%GF,X%GF,Y%GF,Z%GF,&
         B%curl_curlZ(3)%D_3D,&
         B%curl_curlZ(3)%S(1:3)%SF%L,&
         B%curl_curlZ(3)%S(1:3)%SF%U,&
         B%curl_curlX(3)%D1_D2,B%curl_curlY(3)%D1_D2,&
         B%curl_curlX(3)%D1_U2,B%curl_curlY(3)%D1_U2,&
         B%curl_curlX(3)%U1_D2,B%curl_curlY(3)%U1_D2,&
         B%curl_curlX(3)%U1_U2,B%curl_curlY(3)%U1_U2)
       end subroutine

      end module