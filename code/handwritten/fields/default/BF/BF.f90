      module block_field_mod
        ! Compiler flags: (_PARALLELIZE_BF_PLANE_)
        use current_precision_mod
        use grid_mod
        use bctype_mod
        use data_location_mod
        use GF_mod
        use block_extend_mod
        use data_location_mod
        use procedure_array_mod
        use procedure_array_extend_mod
        use procedure_array_plane_op_mod
        use procedure_array_plane_op_extend_mod
        use face_edge_corner_indexing_mod
        use boundary_conditions_mod
        use boundary_conditions_extend_mod
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
        public :: update_BC_vals
        public :: assign_Neumann_BCs
        public :: assign_Neumann_BCs_wall_normal
        public :: multiply_Neumann_BCs
        public :: multiply_BCs_by_nhat
        public :: assign_Dirichlet_BCs
        public :: assign_Periodic_BCs
        public :: assign_Robin_BCs
        public :: multiply_Robin_coeff
        public :: multiply_nhat
        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Periodic_single
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann
        public :: set_prescribed_BCs
        public :: set_BCs_homogeneous

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
        public :: dt_given_CFL_number
        public :: Fourier_number
        public :: Robin_BC_coeff

        public :: get_any_Dirichlet
        public :: get_any_Neumann
        public :: get_any_Robin
        public :: get_any_Prescribed

        public :: restrict
        public :: prolongate

        type block_field
          type(grid_field) :: GF ! bulk
          type(boundary_conditions) :: BCs
          type(data_location) :: DL
          logical,dimension(3) :: many_cell_N_periodic = .false.
          logical,dimension(3) :: many_cell = .false.
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
       interface dt_given_CFL_number;         module procedure dt_given_CFL_number_BF;          end interface
       interface Fourier_number;              module procedure Fourier_number_BF;               end interface
       interface Robin_BC_coeff;              module procedure Robin_BC_coeff_BF;               end interface

       interface get_any_Dirichlet;           module procedure get_any_Dirichlet_BF;            end interface
       interface get_any_Neumann;             module procedure get_any_Neumann_BF;              end interface
       interface get_any_Robin;               module procedure get_any_Robin_BF;                end interface
       interface get_any_Prescribed;          module procedure get_any_Prescribed_BF;           end interface

       interface square;                      module procedure square_BF;                       end interface
       interface square_root;                 module procedure square_root_BF;                  end interface
       interface abs;                         module procedure abs_BF;                          end interface
       interface insist_amax_lt_tol;          module procedure insist_amax_lt_tol_BF;           end interface

       interface assign_BCs;                  module procedure assign_BCs_BF;                   end interface
       interface assign_BC_vals;              module procedure assign_BC_vals_BF;               end interface
       interface update_BC_vals;              module procedure update_BC_vals_BF;               end interface
       interface assign_Dirichlet_BCs;        module procedure assign_Dirichlet_BCs_BF;         end interface
       interface assign_Periodic_BCs;         module procedure assign_Periodic_BCs_BF;          end interface
       interface assign_Neumann_BCs;          module procedure assign_Neumann_BCs_faces_BF;     end interface
       interface assign_Neumann_BCs_wall_normal; module procedure assign_Neumann_BCs_wall_normal_BF;     end interface
       interface multiply_Neumann_BCs;        module procedure multiply_Neumann_BCs_BF;         end interface
       interface multiply_BCs_by_nhat;        module procedure multiply_BCs_by_nhat_BF;         end interface
       interface assign_Robin_BCs;            module procedure assign_Robin_BCs_faces_BF;       end interface
       interface assign_Robin_BCs;            module procedure assign_Robin_BCs_dir_BF;         end interface
       interface multiply_Robin_coeff;        module procedure multiply_Robin_coeff_BF;         end interface
       interface multiply_nhat;               module procedure multiply_nhat_BF;                end interface
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
       interface set_BCs_homogeneous;         module procedure set_BCs_homogeneous_BF;          end interface

       interface plane_sum_x;                 module procedure plane_sum_x_BF;                  end interface
       interface plane_sum_y;                 module procedure plane_sum_y_BF;                  end interface
       interface plane_sum_z;                 module procedure plane_sum_z_BF;                  end interface
       interface boundary_flux;               module procedure boundary_flux_BF;                end interface
       interface boundary_flux;               module procedure boundary_flux_SF_BF;             end interface

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
         if(.not.is_Periodic(BF%BCs%face%SB(1)%bct))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmin,1)
         if (N_along(BF%DL,1)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_xmin,1)
         endif
         if(.not.is_Periodic(BF%BCs%face%SB(2)%bct))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_xmax,2)
         if (N_along(BF%DL,1)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_xmax,2)
         endif
         if(.not.is_Periodic(BF%BCs%face%SB(3)%bct))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymin,3)
         if (N_along(BF%DL,2)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_ymin,3)
         endif
         if(.not.is_Periodic(BF%BCs%face%SB(4)%bct))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_ymax,4)
         if (N_along(BF%DL,2)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_ymax,4)
         endif
         if(.not.is_Periodic(BF%BCs%face%SB(5)%bct))then
         call add(BF%PA_assign_ghost_XPeriodic,assign_ghost_zmin,5)
         if (N_along(BF%DL,3)) call add(BF%PA_assign_ghost_N_XPeriodic,assign_ghost_zmin,5)
         endif
         if(.not.is_Periodic(BF%BCs%face%SB(6)%bct))then
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
           if (N_along(BF%DL,1).and.(is_Dirichlet(BF%BCs%face%SB(1)%bct))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmin,1)
           endif
           if (N_along(BF%DL,1).and.(is_Dirichlet(BF%BCs%face%SB(2)%bct))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_xmax,2)
           endif
           if (N_along(BF%DL,2).and.(is_Dirichlet(BF%BCs%face%SB(3)%bct))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymin,3)
           endif
           if (N_along(BF%DL,2).and.(is_Dirichlet(BF%BCs%face%SB(4)%bct))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_ymax,4)
           endif
           if (N_along(BF%DL,3).and.(is_Dirichlet(BF%BCs%face%SB(5)%bct))) then
           call add(BF%PA_assign_wall_Dirichlet,assign_wall_zmin,5)
           endif
           if (N_along(BF%DL,3).and.(is_Dirichlet(BF%BCs%face%SB(6)%bct))) then
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
         integer :: i
         call delete(BF%PA_assign_wall_Periodic_single)
         if (defined(BF%BCs)) then
           L(1) = N_along(BF%DL,1)
           L(2) = is_Periodic(BF%BCs%face%SB(1)%bct)
           L(3) = is_Periodic(BF%BCs%face%SB(2)%bct)
           L(4) = BF%GF%s(1).gt.4
           BF%many_cell_N_periodic(1) = all(L)
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_xmax,2)

           L(1) = N_along(BF%DL,2)
           L(2) = is_Periodic(BF%BCs%face%SB(3)%bct)
           L(3) = is_Periodic(BF%BCs%face%SB(4)%bct)
           L(4) = BF%GF%s(2).gt.4
           BF%many_cell_N_periodic(2) = all(L)
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_ymax,4)

           L(1) = N_along(BF%DL,3)
           L(2) = is_Periodic(BF%BCs%face%SB(5)%bct)
           L(3) = is_Periodic(BF%BCs%face%SB(6)%bct)
           L(4) = BF%GF%s(3).gt.4
           BF%many_cell_N_periodic(3) = all(L)
           if (all(L)) call add(BF%PA_assign_wall_Periodic_single,assign_wall_zmax,6)

           do i=1,3
             L(1) = N_along(BF%DL,i)
             L(2) = CC_along(BF%DL,i)
             L(3) = BF%GF%s(i).gt.4
             L(4) = BF%GF%s(i).gt.3
             BF%many_cell(i) = (L(1).and.L(3)).or.(L(2).and.L(4))
           enddo
         endif
       end subroutine

       subroutine set_multiply_wall_Neumann(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%PA_multiply_wall_Neumann)
         if (defined(BF%BCs)) then
           if (N_along(BF%DL,1).and.(is_Neumann(BF%BCs%face%SB(1)%bct))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmin,1); endif
           if (N_along(BF%DL,1).and.(is_Neumann(BF%BCs%face%SB(2)%bct))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_xmax,2); endif
           if (N_along(BF%DL,2).and.(is_Neumann(BF%BCs%face%SB(3)%bct))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymin,3); endif
           if (N_along(BF%DL,2).and.(is_Neumann(BF%BCs%face%SB(4)%bct))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_ymax,4); endif
           if (N_along(BF%DL,3).and.(is_Neumann(BF%BCs%face%SB(5)%bct))) then
           call add(BF%PA_multiply_wall_Neumann,multiply_wall_zmin,5); endif
           if (N_along(BF%DL,3).and.(is_Neumann(BF%BCs%face%SB(6)%bct))) then
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
         BF%many_cell_N_periodic = BF_in%many_cell_N_periodic
         BF%many_cell = BF_in%many_cell
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
         BF%many_cell_N_periodic = .false.
         BF%many_cell = .false.
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

       subroutine init_BC_props_BF(BF,c_w,Robin_coeff)
         implicit none
         type(block_field),intent(inout) :: BF
         real(cp),dimension(6),intent(in) :: c_w,Robin_coeff
         call init_props(BF%BCs,c_w,Robin_coeff)
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

       function dt_given_CFL_number_BF(U_CC,V_CC,W_CC,B,CFL) result(dt)
         implicit none
         type(block_field),intent(in) :: U_CC,V_CC,W_CC
         type(block),intent(in) :: B
         real(cp),intent(in) :: CFL
         real(cp) :: dt
         dt = dt_given_CFL_number(U_CC%GF,V_CC%GF,W_CC%GF,B%g,CFL)
       end function

       function Fourier_number_BF(alpha,B,dt) result(Fourier)
         implicit none
         real(cp),intent(in) :: alpha
         type(block),intent(in) :: B
         real(cp),intent(in) :: dt
         real(cp) :: Fourier
         Fourier = Fourier_number(alpha,B%g,dt)
       end function

       function Robin_BC_coeff_BF(c_w,B) result(coeff)
         implicit none
         real(cp),dimension(6),intent(in) :: c_w
         type(block),intent(in) :: B
         real(cp),dimension(6) :: coeff
         coeff = Robin_BC_coeff(c_w,B%g)
       end function

       function get_any_Dirichlet_BF(BF) result(L)
         implicit none
         type(block_field),intent(in) :: BF
         logical :: L
         L = get_any_Dirichlet(BF%BCs)
       end function

       function get_any_Neumann_BF(BF) result(L)
         implicit none
         type(block_field),intent(in) :: BF
         logical :: L
         L = get_any_Neumann(BF%BCs)
       end function

       function get_any_Robin_BF(BF) result(L)
         implicit none
         type(block_field),intent(in) :: BF
         logical :: L
         L = get_any_Robin(BF%BCs)
       end function

       function get_any_Prescribed_BF(BF) result(L)
         implicit none
         type(block_field),intent(in) :: BF
         logical :: L
         L = get_any_Prescribed(BF%BCs)
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
           call assign_plane_x(u%BCs%face%SB(1)%b,f%GF,1,     2     )
           call assign_plane_x(u%BCs%face%SB(2)%b,f%GF,1,f%GF%s(1)-1)
           call assign_plane_y(u%BCs%face%SB(3)%b,f%GF,1,     2     )
           call assign_plane_y(u%BCs%face%SB(4)%b,f%GF,1,f%GF%s(2)-1)
           call assign_plane_z(u%BCs%face%SB(5)%b,f%GF,1,     2     )
           call assign_plane_z(u%BCs%face%SB(6)%b,f%GF,1,f%GF%s(3)-1)
         endif
       end subroutine

       subroutine assign_BC_vals_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer :: i
#ifdef _DEBUG_BF_
         if (.not.(defined(A%BCs).and.defined(B%BCs))) stop 'Error: BCs not defined in BF.f90'
#endif
         do i=1,6; call assign(A%BCs%face%SB(i)%b         ,B%BCs%face%SB(i)%b         ); enddo
         do i=1,6; call assign(A%BCs%face%SB(i)%b_total   ,B%BCs%face%SB(i)%b_total   ); enddo
         do i=1,6; call assign(A%BCs%face%SB(i)%b_modified,B%BCs%face%SB(i)%b_modified); enddo
       end subroutine

       subroutine update_BC_vals_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         integer :: i
#ifdef _DEBUG_BF_
         if (.not.(defined(A%BCs))) stop 'Error: BCs not defined in update_BC_vals_BF in BF.f90'
#endif
         do i=1,6
         call add(A%BCs%face%SB(i)%b_total,A%BCs%face%SB(i)%b,A%BCs%face%SB(i)%b_modified)
         enddo
       end subroutine

       subroutine assign_Dirichlet_BCs_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer :: i,dir
         if (defined(A%BCs)) then
           do i=1,6
             dir = dir_given_face(i)
             if (is_Dirichlet(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,       2     ,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir)-1,dir)
             else
               if (min_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,     1     ,      2      ,dir)
               if (max_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir),B%GF%s(dir)-1,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine assign_Periodic_BCs_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer,dimension(3) :: p
         integer :: i,dir,i_opp_e,i_opp_s
         if (defined(A%BCs)) then
           do i=1,6
             dir = dir_given_face(i)
             p = N_eye(A%DL)
             i_opp_e = B%GF%s(dir)-1-p(dir)
             i_opp_s = 2+p(dir)
             if (is_Periodic(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,i_opp_e,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,i_opp_s,dir)
             else
               if (min_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,i_opp_e+1,i_opp_e,dir)
               if (max_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,i_opp_s-1,i_opp_s,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine assign_Neumann_BCs_faces_BF(A,B,dir)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer,intent(in) :: dir
         integer :: i,j
         integer,dimension(2) :: faces
         if (defined(A%BCs)) then
           faces = normal_faces_given_dir(dir)
           do j=1,2
             i = faces(j)
             if (is_Neumann(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,       2     ,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir)-1,dir)
             else
               if (min_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,     1     ,      2      ,dir)
               if (max_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir),B%GF%s(dir)-1,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine assign_Neumann_BCs_wall_normal_BF(A,B,dir)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer,intent(in) :: dir
         integer :: i,j
         integer,dimension(2) :: faces
         if (defined(A%BCs)) then
           faces = normal_faces_given_dir(dir)
           do j=1,2
             i = faces(j)
             if (is_Neumann(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,       2     ,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir)-1,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine multiply_Neumann_BCs_BF(A,scale)
         implicit none
         type(block_field),intent(inout) :: A
         real(cp),intent(in) :: scale
         integer :: i
         if (defined(A%BCs)) then
           do i=1,6
             if (is_Neumann(A%BCs%face%SB(i)%bct)) then
             call multiply(A%BCs%face%SB(i)%b_modified,scale)
             endif
           enddo
         endif
       end subroutine

       subroutine multiply_BCs_by_nhat_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         integer :: i
         if (defined(A%BCs)) then
           do i=1,6
             call multiply(A%BCs%face%SB(i)%b_modified,A%BCs%f_BCs%nhat(i))
           enddo
         endif
       end subroutine

       subroutine assign_Robin_BCs_dir_BF(A,B,dir)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer,intent(in) :: dir
         integer :: i,j
         integer,dimension(2) :: faces
         if (defined(A%BCs)) then
           faces = normal_faces_given_dir(dir)
           do j=1,2
             i = faces(j)
             if (is_Robin(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,       2     ,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir)-1,dir)
             else
               if (min_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,     1     ,      2      ,dir)
               if (max_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir),B%GF%s(dir)-1,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine assign_Robin_BCs_faces_BF(A,B)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: B
         integer :: i,dir
         if (defined(A%BCs)) then
           do i=1,6
             dir = dir_given_face(i)
             if (is_Robin(A%BCs%face%SB(i)%bct)) then
             if (N_along(B%DL,dir)) then
               if (min_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,       2     ,dir)
               if (max_face(i)) call assign_plane(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir)-1,dir)
             else
               if (min_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,     1     ,      2      ,dir)
               if (max_face(i)) call assign_plane_ave(A%BCs%face%SB(i)%b_modified,B%GF,1,B%GF%s(dir),B%GF%s(dir)-1,dir)
             endif
             endif
           enddo
         endif
       end subroutine

       subroutine multiply_Robin_coeff_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         integer :: i,dir
         if (defined(A%BCs)) then
           do i=1,6
             dir = dir_given_face(i)
             if (is_Robin(A%BCs%face%SB(i)%bct)) then
               call multiply(A%BCs%face%SB(i)%b_modified,A%BCs%f_BCs%Robin_coeff(i))
             endif
           enddo
         endif
       end subroutine

       subroutine multiply_nhat_BF(A,A_with_BCs)
         implicit none
         type(block_field),intent(inout) :: A
         type(block_field),intent(in) :: A_with_BCs
         integer :: i,dir
         if (defined(A_with_BCs%BCs)) then
           do i=1,6
             dir = dir_given_face(i)
             if (N_along(A%DL,dir)) then
               if (min_face(i)) call multiply_plane(A%GF,A_with_BCs%BCs%f_BCs%nhat(i),       2     ,dir)
               if (max_face(i)) call multiply_plane(A%GF,A_with_BCs%BCs%f_BCs%nhat(i),A%GF%s(dir)-1,dir)
             endif
           enddo
         endif
       end subroutine

       subroutine set_prescribed_BCs_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         integer :: i,dir
         if (defined(A%BCs)) then
           do i=1,6
            dir = dir_given_face(i)
            if (A%many_cell(dir)) call set_prescribed(A%BCs,i)
            call init_PA_face(A%BCs,i)
           enddo
         endif
       end subroutine

       subroutine set_BCs_homogeneous_BF(A)
         implicit none
         type(block_field),intent(inout) :: A
         integer :: i
         if (defined(A%BCs)) then
           do i=1,6; call init(A%BCs,0.0_cp,i); enddo
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

       subroutine boundary_flux_BF(BF,x,y,z,B,temp_x,temp_y,temp_z)
         implicit none
         real(cp),intent(inout) :: BF
         type(block_field),intent(in) :: x,y,z
         type(block_field),intent(inout) :: temp_x,temp_y,temp_z
         type(block),intent(in) :: B
         logical,dimension(3) :: L
         L(1) = is_Face(x%DL).and.(get_Face(x%DL).eq.1)
         L(2) = is_Face(y%DL).and.(get_Face(y%DL).eq.2)
         L(3) = is_Face(z%DL).and.(get_Face(z%DL).eq.3)
         if (all(L)) then
         BF = 0.0_cp
         call assign(temp_x%GF,x%GF); call assign_ghost_XPeriodic(temp_x,0.0_cp)
         call assign(temp_y%GF,y%GF); call assign_ghost_XPeriodic(temp_y,0.0_cp)
         call assign(temp_z%GF,z%GF); call assign_ghost_XPeriodic(temp_z,0.0_cp)
         BF = BF + plane_sum_x(temp_x%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_x(temp_x%GF,B%g,temp_x%GF%s(1)-1,1.0_cp)
         BF = BF + plane_sum_y(temp_y%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_y(temp_y%GF,B%g,temp_y%GF%s(2)-1,1.0_cp)
         BF = BF + plane_sum_z(temp_z%GF,B%g,2,-1.0_cp)
         BF = BF + plane_sum_z(temp_z%GF,B%g,temp_z%GF%s(3)-1,1.0_cp)
         else; stop 'Error: boundary flux only offered for face data in BF.f90'
         endif
       end subroutine

       subroutine boundary_flux_SF_BF(BF,phi,B,temp_phi)
         implicit none
         real(cp),intent(inout) :: BF
         type(block_field),intent(in) :: phi
         type(block),intent(in) :: B
         type(block_field),intent(inout) :: temp_phi
         logical,dimension(3) :: L
         L(1) = is_Face(phi%DL).and.(get_Face(phi%DL).eq.1)
         L(2) = is_Face(phi%DL).and.(get_Face(phi%DL).eq.2)
         L(3) = is_Face(phi%DL).and.(get_Face(phi%DL).eq.3)
         BF = 0.0_cp
         if (any(L)) then
           call assign(temp_phi%GF,phi%GF)
           call assign_ghost_XPeriodic(temp_phi,0.0_cp)
              if (L(1)) then
           BF = BF + plane_sum_x(temp_phi%GF,B%g,2,-1.0_cp)
           BF = BF + plane_sum_x(temp_phi%GF,B%g,temp_phi%GF%s(1)-1,1.0_cp)
           elseif(L(2)) then
           BF = BF + plane_sum_y(temp_phi%GF,B%g,2,-1.0_cp)
           BF = BF + plane_sum_y(temp_phi%GF,B%g,temp_phi%GF%s(2)-1,1.0_cp)
           elseif(L(3)) then
           BF = BF + plane_sum_z(temp_phi%GF,B%g,2,-1.0_cp)
           BF = BF + plane_sum_z(temp_phi%GF,B%g,temp_phi%GF%s(3)-1,1.0_cp)
           endif
         endif
       end subroutine

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
         real(cp),dimension(6) :: c_w,Robin_coeff
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
         if (CC_along(u%DL,dir))    then; call restrict_C(u%GF,B%g,dir,x,y,z)
         elseif (N_along(u%DL,dir)) then; call restrict_N(u%GF,B%g,dir,x,y,z)
         else; stop 'Error: bad DL in restrict_BF in BF.f90'
         endif

         if (defined(u%BCs)) then
         do i=1,6
         if (dir_given_face(i).ne.dir) then ! only restrict BCs along surface tangent directions
           if (CC_along(u%DL,dir))    then; call restrict_C(u%BCs%face%SB(i)%b,B%fb(i),dir,x,y,z)
           elseif (N_along(u%DL,dir)) then; call restrict_N(u%BCs%face%SB(i)%b,B%fb(i),dir,x,y,z)
           else; stop 'Error: bad DL in restrict_BF in BF.f90'
           endif
         endif
         enddo
         Robin_coeff = u%BCs%f_BCs%Robin_coeff
         c_w = u%BCs%f_BCs%c_w
         call restrict(u%BCs,B,dir)
         call init_BC_props(u,c_w,Robin_coeff)
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
         real(cp),dimension(6) :: c_w,Robin_coeff
         eye = eye_given_dir(dir)
         x = eye(1); y = eye(2); z = eye(3)
             if (CC_along(u%DL,dir)) then; call prolongate_C(u%GF,B%g,dir,x,y,z)
         elseif ( N_along(u%DL,dir)) then; call prolongate_N(u%GF,dir,x,y,z)
         else; stop 'Error: bad DL in prolongate_BF in BF.f90'
         endif
         if (defined(u%BCs)) then
           Robin_coeff = u%BCs%f_BCs%Robin_coeff
           c_w = u%BCs%f_BCs%c_w
           call prolongate(u%BCs,B,dir)
           call init_BC_props(u,c_w,Robin_coeff)
         endif
       end subroutine

      end module