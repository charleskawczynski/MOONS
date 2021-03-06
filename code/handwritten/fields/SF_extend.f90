      module SF_extend_mod
      use SF_mod
        use current_precision_mod
        use IO_tools_mod
        use array_mod
        use data_location_extend_mod
        use mesh_extend_mod
        use mesh_domain_extend_mod
        use boundary_conditions_extend_mod
        use GF_mod
        use block_field_mod
        use block_field_extend_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: SF
        public :: init,delete,display,print,export,import ! Essentials
        ! Grid initialization
        public :: init_CC
        public :: init_Node
        public :: init_Face
        public :: init_Edge

        public :: is_CC
        public :: is_Node
        public :: is_Face
        public :: is_Edge

        public :: get_DL

        public :: inverse_area
        public :: volume
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z
        public :: boundary_flux

        public :: symmetry_error_x,symmetry_local_x
        public :: symmetry_error_y,symmetry_local_y
        public :: symmetry_error_z,symmetry_local_z

        public :: mirror_about_hmin,mirror_about_hmax

        public :: multiply_volume
        public :: mean_along_dir
        public :: amax_diff
        public :: subtract_mean_along_dir

        public :: assign_BCs
        public :: assign_BC_vals
        public :: update_BC_vals
        public :: assign_Dirichlet_BCs
        public :: assign_Periodic_BCs
        public :: assign_Neumann_BCs
        public :: assign_Neumann_BCs_wall_normal
        public :: multiply_Neumann_BCs
        public :: multiply_BCs_by_nhat
        public :: assign_Robin_BCs
        public :: multiply_Robin_coeff
        public :: multiply_nhat
        public :: assign_wall_Periodic_single
        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann
        public :: set_prescribed_BCs
        public :: set_BCs_homogeneous

        public :: get_any_Dirichlet
        public :: get_any_Neumann
        public :: get_any_Robin
        public :: get_any_Prescribed

        public :: init_BCs,init_BC_Dirichlet,init_BC_mesh
        public :: init_BC_props
        public :: set_procedures
        public :: set_necessary_for_restart
        public :: dot_product
        public :: cross_product_x
        public :: cross_product_y
        public :: cross_product_z

        public :: CFL_number
        public :: dt_given_CFL_number
        public :: Fourier_number
        public :: Robin_BC_coeff

        public :: restrict
        public :: prolongate

        ! Monitoring
        public :: print_BCs
        public :: export_BCs

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: invert
        public :: add_product,product_add,swap
        ! Auxiliary
        public :: square,square_root,min,max,amin,amax
        public :: mean,sum,abs,insist_amax_lt_tol
        public :: magnitude

        public :: assign_ghost_xmin_xmax
        public :: assign_ghost_ymin_ymax
        public :: assign_ghost_zmin_zmax

        interface init;                     module procedure init_DL_SF;                   end interface
        interface init;                     module procedure init_SF_copy_mesh;            end interface

        interface init_CC;                  module procedure init_SF_CC;                   end interface
        interface init_Node;                module procedure init_SF_Node;                 end interface
        interface init_Face;                module procedure init_SF_Face;                 end interface
        interface init_Edge;                module procedure init_SF_Edge;                 end interface

        interface init_CC;                  module procedure init_SF_CC_D;                 end interface
        interface init_Node;                module procedure init_SF_Node_D;               end interface
        interface init_Face;                module procedure init_SF_Face_D;               end interface
        interface init_Edge;                module procedure init_SF_Edge_D;               end interface

        interface init_CC;                  module procedure init_SF_CC_assign;            end interface
        interface init_Node;                module procedure init_SF_Node_assign;          end interface
        interface init_Face;                module procedure init_SF_Face_assign;          end interface
        interface init_Edge;                module procedure init_SF_Edge_assign;          end interface

        interface init_BCs;                 module procedure init_BCs_SF_SF;               end interface
        interface init_BC_Dirichlet;        module procedure init_BC_Dirichlet_SF;         end interface
        interface init_BCs;                 module procedure init_BC_val_SF;               end interface
        interface init_BC_mesh;             module procedure init_BC_mesh_SF;              end interface
        interface init_BC_props;            module procedure init_BC_props_SF;             end interface
        interface set_procedures;           module procedure set_procedures_SF;            end interface
        interface set_necessary_for_restart;module procedure set_necessary_for_restart_SF; end interface

        interface print_BCs;                module procedure print_BCs_SF;                 end interface
        interface export_BCs;               module procedure export_BCs_SF;                end interface

        interface multiply_volume;          module procedure multiply_volume_SF;           end interface
        interface mean_along_dir;           module procedure mean_along_dir_SF;            end interface
        interface amax_diff;                module procedure amax_diff_SF;                 end interface
        interface amax_diff;                module procedure amax_diff_dir_SF;             end interface
        interface subtract_mean_along_dir;  module procedure subtract_mean_along_dir_SF;   end interface

        interface is_CC;                    module procedure is_CC_SF;                     end interface
        interface is_Node;                  module procedure is_Node_SF;                   end interface
        interface is_Face;                  module procedure is_Face_SF;                   end interface
        interface is_Edge;                  module procedure is_Edge_SF;                   end interface

        interface get_DL;                   module procedure get_DL_SF;                    end interface

        interface inverse_area;             module procedure inverse_area_SF;              end interface
        interface volume;                   module procedure volume_SF;                    end interface
        interface sine_waves;               module procedure sine_waves_SF;                end interface
        interface cosine_waves;             module procedure cosine_waves_SF;              end interface
        interface random_noise;             module procedure random_noise_SF;              end interface
        interface random_noise;             module procedure random_noise_SF_dir;          end interface

        interface assign_BCs;               module procedure assign_BCs_SF;                end interface
        interface assign_BC_vals;           module procedure assign_BC_vals_SF;            end interface
        interface update_BC_vals;           module procedure update_BC_vals_SF;            end interface
        interface assign_Dirichlet_BCs;     module procedure assign_Dirichlet_BCs_SF;      end interface
        interface assign_Periodic_BCs;      module procedure assign_Periodic_BCs_SF;       end interface
        interface assign_Neumann_BCs;       module procedure assign_Neumann_BCs_faces_SF;  end interface
        interface assign_Neumann_BCs_wall_normal; module procedure assign_Neumann_BCs_wall_normal_SF;  end interface
        interface multiply_Neumann_BCs;     module procedure multiply_Neumann_BCs_SF;      end interface
        interface multiply_BCs_by_nhat;     module procedure multiply_BCs_by_nhat_SF;      end interface
        interface assign_Robin_BCs;         module procedure assign_Robin_BCs_dir_SF;      end interface
        interface assign_Robin_BCs;         module procedure assign_Robin_BCs_faces_SF;    end interface
        interface multiply_Robin_coeff;     module procedure multiply_Robin_coeff_SF;      end interface
        interface multiply_nhat;            module procedure multiply_nhat_SF;             end interface
        interface assign_ghost_XPeriodic;   module procedure assign_ghost_XPeriodic_SF;    end interface
        interface assign_ghost_XPeriodic;   module procedure assign_ghost_XPeriodic_SF2;   end interface
        interface assign_ghost_N_XPeriodic; module procedure assign_ghost_N_XPeriodic_SF;  end interface
        interface assign_ghost_N_XPeriodic; module procedure assign_ghost_N_XPeriodic_SF2; end interface
        interface assign_wall_Dirichlet;    module procedure assign_wall_Dirichlet_SF;     end interface
        interface assign_wall_Dirichlet;    module procedure assign_wall_Dirichlet_SF2;    end interface
        interface assign_wall_Periodic_single;     module procedure assign_wall_Periodic_single_SF;      end interface
        interface assign_wall_Periodic_single;     module procedure assign_wall_Periodic_single_SF2;      end interface
        interface multiply_wall_Neumann;    module procedure multiply_wall_Neumann_SF;     end interface
        interface multiply_wall_Neumann;    module procedure multiply_wall_Neumann_SF2;    end interface
        interface set_prescribed_BCs;       module procedure set_prescribed_BCs_SF;        end interface
        interface set_BCs_homogeneous;      module procedure set_BCs_homogeneous_SF;       end interface

        interface get_any_Dirichlet;        module procedure get_any_Dirichlet_SF;         end interface
        interface get_any_Neumann;          module procedure get_any_Neumann_SF;           end interface
        interface get_any_Robin;            module procedure get_any_Robin_SF;             end interface
        interface get_any_Prescribed;       module procedure get_any_Prescribed_SF;        end interface

        interface plane_sum_x;              module procedure plane_sum_x_SF;               end interface
        interface plane_sum_y;              module procedure plane_sum_y_SF;               end interface
        interface plane_sum_z;              module procedure plane_sum_z_SF;               end interface
        interface boundary_flux;            module procedure boundary_flux_SF;             end interface
        interface boundary_flux;            module procedure boundary_flux_SF_SF;          end interface

        interface symmetry_error_x;         module procedure symmetry_error_x_SF;          end interface
        interface symmetry_error_y;         module procedure symmetry_error_y_SF;          end interface
        interface symmetry_error_z;         module procedure symmetry_error_z_SF;          end interface

        interface symmetry_local_x;         module procedure symmetry_local_x_SF;          end interface
        interface symmetry_local_y;         module procedure symmetry_local_y_SF;          end interface
        interface symmetry_local_z;         module procedure symmetry_local_z_SF;          end interface

        interface mirror_about_hmin;        module procedure mirror_about_hmin_SF;         end interface
        interface mirror_about_hmax;        module procedure mirror_about_hmax_SF;         end interface

        interface assign_ghost_xmin_xmax;   module procedure assign_ghost_xmin_xmax_SF;    end interface
        interface assign_ghost_ymin_ymax;   module procedure assign_ghost_ymin_ymax_SF;    end interface
        interface assign_ghost_zmin_zmax;   module procedure assign_ghost_zmin_zmax_SF;    end interface

        interface cross_product_x;          module procedure cross_product_x_SF;           end interface
        interface cross_product_y;          module procedure cross_product_y_SF;           end interface
        interface cross_product_z;          module procedure cross_product_z_SF;           end interface

        interface CFL_number;               module procedure CFL_number_SF;                end interface
        interface dt_given_CFL_number;      module procedure dt_given_CFL_number_SF;       end interface
        interface Fourier_number;           module procedure Fourier_number_SF;            end interface
        interface Robin_BC_coeff;           module procedure Robin_BC_coeff_SF;            end interface

        interface restrict;                 module procedure restrict_SF;                  end interface
        interface restrict;                 module procedure restrict_reset_SF;            end interface
        interface restrict;                 module procedure restrict_all_reset_SF;        end interface
        interface prolongate;               module procedure prolongate_SF;                end interface
        interface prolongate;               module procedure prolongate_reset_SF;          end interface
        interface prolongate;               module procedure prolongate_all_reset_SF;      end interface

        ! COMPUTATION ROUTINES:

        interface assign;                   module procedure assign_SF_S;                  end interface
        interface assign;                   module procedure assign_SF_SF;                 end interface
        interface assign_negative;          module procedure assign_negative_SF_SF;        end interface

        interface add;                      module procedure add_SF_SF;                    end interface
        interface add;                      module procedure add_SF_SF_SF;                 end interface
        interface add;                      module procedure add_SF_SF_SF_SF;              end interface
        interface add;                      module procedure add_SF_S;                     end interface
        interface add;                      module procedure add_S_SF;                     end interface
        interface add;                      module procedure add_SF_SF9;                   end interface

        interface add_product;              module procedure add_product_SF_SF_S;          end interface
        interface add_product;              module procedure add_product_SF_SF_SF;         end interface
        interface product_add;              module procedure product_add_SF_SF_S;          end interface
        interface product_add;              module procedure product_add_SF_SF_SF;         end interface

        interface multiply;                 module procedure multiply_SF_SF;               end interface
        interface multiply;                 module procedure multiply_SF_SF_SF;            end interface
        interface multiply;                 module procedure multiply_SF_SF_S;             end interface
        interface multiply;                 module procedure multiply_SF_S;                end interface
        interface multiply;                 module procedure multiply_S_SF;                end interface

        interface subtract;                 module procedure subtract_SF_SF;               end interface
        interface subtract;                 module procedure subtract_SF_SF_SF;            end interface
        interface subtract;                 module procedure subtract_SF_S;                end interface
        interface subtract;                 module procedure subtract_S_SF;                end interface

        interface divide;                   module procedure divide_SF_SF;                 end interface
        interface divide;                   module procedure divide_SF_SF_SF;              end interface
        interface divide;                   module procedure divide_SF_S_SF;               end interface
        interface divide;                   module procedure divide_SF_S;                  end interface
        interface divide;                   module procedure divide_S_SF;                  end interface

        interface invert;                   module procedure invert_SF;                    end interface
        interface square;                   module procedure square_SF;                    end interface
        interface square_root;              module procedure square_root_SF;               end interface
        interface abs;                      module procedure abs_SF;                       end interface
        interface magnitude;                module procedure magnitude_SF_SF_SF;           end interface
        interface insist_amax_lt_tol;       module procedure insist_amax_lt_tol_SF;        end interface

        interface swap;                     module procedure swap_SF;                      end interface
        interface min;                      module procedure min_SF;                       end interface
        interface max;                      module procedure max_SF;                       end interface
        interface min;                      module procedure min_pad_SF;                   end interface
        interface max;                      module procedure max_pad_SF;                   end interface
        interface amin;                     module procedure amin_SF;                      end interface
        interface amax;                     module procedure amax_SF;                      end interface
        interface mean;                     module procedure mean_SF;                      end interface
        interface sum;                      module procedure sum_SF;                       end interface
        interface sum;                      module procedure sum_SF_pad;                   end interface
        interface dot_product;              module procedure dot_product_SF;               end interface

      contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

        subroutine init_DL_SF(f,m,DL)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(data_location),intent(in) :: DL
                if (is_CC(DL)) then; call init_CC(f,m)
          elseif (is_Node(DL)) then; call init_Node(f,m)
          elseif (is_Face(DL)) then; call init_Face(f,m,get_Face(DL))
          elseif (is_Edge(DL)) then; call init_Edge(f,m,get_Edge(DL))
          else; stop 'Error: Bad DL in SF.f90'
          endif
        end subroutine

        subroutine init_SF_copy_mesh(f,f_in,m)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: f_in
          type(mesh),intent(in) :: m
          type(SF) :: temp
                if (is_CC(f_in%DL)) then; call init_CC(temp,m)
          elseif (is_Node(f_in%DL)) then; call init_Node(temp,m)
          elseif (is_Face(f_in%DL)) then; call init_Face(temp,m,get_Face(f_in%DL))
          elseif (is_Edge(f_in%DL)) then; call init_Edge(temp,m,get_Edge(f_in%DL))
          else; stop 'Error: bad datatype in init_SF_copy_mesh in SF.f90'
          endif
          call init(f,temp)
          call delete(temp)
        end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

        subroutine computeNumEl(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          f%numEl = 0
          f%numPhysEl = 0
          do i=1,f%s
          f%numEl = f%numEl + f%BF(i)%GF%s(1)*f%BF(i)%GF%s(2)*f%BF(i)%GF%s(3)
          f%numPhysEl = f%numPhysEl + (f%BF(i)%GF%s(1)-2)*(f%BF(i)%GF%s(2)-2)*(f%BF(i)%GF%s(3)-2)
          enddo
        end subroutine

        subroutine init_SF_CC(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          call delete(f)
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_CC(f%BF(i),m%B(i)); enddo
          call computeNumEl(f)
          call init_CC(f%DL)
        end subroutine

        subroutine init_SF_CC_D(f,m,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
              if (compare(m,MD%m_R1)) then; call init_CC(f,MD%m_R2)
          elseif (compare(m,MD%m_R2)) then; call init_CC(f,MD%m_R1)
          else; stop 'Error: case not found in init_SF_CC_D in SF.f90'
          endif
        end subroutine

        subroutine init_SF_CC_assign(f,m,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f,m); call assign(f,val)
        end subroutine

        subroutine init_SF_Face(f,m,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          call delete(f)
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Face(f%BF(i),m%B(i),dir); enddo
          call computeNumEl(f)
          call init_Face(f%DL,dir)
        end subroutine

        subroutine init_SF_Face_D(f,m,dir,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          type(mesh_domain),intent(in) :: MD
              if (compare(m,MD%m_R1)) then; call init_Face(f,MD%m_R2,dir)
          elseif (compare(m,MD%m_R2)) then; call init_Face(f,MD%m_R1,dir)
          else; stop 'Error: case not found in init_SF_Face_D in SF.f90'
          endif
        end subroutine

        subroutine init_SF_Face_assign(f,m,dir,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          real(cp),intent(in) :: val
          call init_Face(f,m,dir); call assign(f,val)
        end subroutine

        subroutine init_SF_Edge(f,m,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          call delete(f)
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Edge(f%BF(i),m%B(i),dir); enddo
          call computeNumEl(f)
          call init_Edge(f%DL,dir)
        end subroutine

        subroutine init_SF_Edge_D(f,m,dir,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          type(mesh_domain),intent(in) :: MD
              if (compare(m,MD%m_R1)) then; call init_Edge(f,MD%m_R2,dir)
          elseif (compare(m,MD%m_R2)) then; call init_Edge(f,MD%m_R1,dir)
          else; stop 'Error: case not found in init_SF_Edge_D in SF.f90'
          endif
        end subroutine

        subroutine init_SF_Edge_assign(f,m,dir,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          real(cp),intent(in) :: val
          call init_Edge(f,m,dir); call assign(f,val)
        end subroutine

        subroutine init_SF_Node(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          call delete(f)
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Node(f%BF(i),m%B(i)); enddo
          call computeNumEl(f)
          call init_Node(f%DL)
        end subroutine

        subroutine init_SF_Node_D(f,m,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
              if (compare(m,MD%m_R1)) then; call init_Node(f,MD%m_R2)
          elseif (compare(m,MD%m_R2)) then; call init_Node(f,MD%m_R1)
          else; stop 'Error: case not found in init_SF_Node_D in SF.f90'
          endif
        end subroutine

        subroutine init_SF_Node_assign(f,m,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f,m); call assign(f,val)
        end subroutine

        ! ***********************************************************
        ! *************************** BCS ***************************
        ! ***********************************************************

        subroutine init_BCs_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call init(f%BF(i)%BCs,g%BF(i)%BCs); enddo
        end subroutine

        subroutine init_BC_Dirichlet_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call init_Dirichlet(f%BF(i)%BCs); enddo
        end subroutine

        subroutine init_BC_val_SF(f,val,c_w,Robin_coeff)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          real(cp),dimension(6),intent(in) :: c_w,Robin_coeff
          integer :: i
          do i=1,f%s; call init_BCs(f%BF(i),val); enddo
          f%all_Neumann = all((/(f%BF(i)%BCs%BCL%all_Neumann,i=1,f%s)/))
          call init_BC_props(f,c_w,Robin_coeff)
        end subroutine

        subroutine init_BC_mesh_SF(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,f%s; call init_BCs(f%BF(i),m%B(i),f%DL); enddo
        end subroutine

        subroutine init_BC_props_SF(f,c_w,Robin_coeff)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),dimension(6),intent(in) :: c_w,Robin_coeff
          integer :: i
          do i=1,f%s
            call init_BC_props(f%BF(i),c_w,Robin_coeff)
          enddo
        end subroutine

        subroutine set_procedures_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s
            call set_procedures(f%BF(i))
          enddo
        end subroutine

        subroutine set_necessary_for_restart_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call set_necessary_for_restart(f%BF(i)); enddo
        end subroutine


        ! ***********************************************************
        ! ***********************************************************
        ! ***********************************************************

        function is_CC_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          L = is_CC(u%DL)
        end function

        function is_Node_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          L = is_Node(u%DL)
        end function

        function is_Face_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          L = is_Face(u%DL)
        end function

        function is_Edge_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          L = is_Edge(u%DL)
        end function

        function get_DL_SF(u) result(DL)
          implicit none
          type(SF),intent(in) :: u
          type(data_location) :: DL
          DL = u%DL
        end function

        subroutine volume_SF(u,m) ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          call assign(u,0.0_cp)
          do i=1,m%s
            call volume(u%BF(i),m%B(i),u%DL)
          enddo
          u%vol = sum(u)
        end subroutine

        subroutine inverse_area_SF(u,m,dir)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          call assign(u,0.0_cp)
          do i=1,m%s
            call inverse_area(u%BF(i),m%B(i),u%DL,dir)
          enddo
        end subroutine

        subroutine sine_waves_SF(u,m,wavenum,phi)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          integer :: i
          do i=1,m%s; call sine_waves(u%BF(i),m%B(i),wavenum,phi,u%DL); enddo
        end subroutine

        subroutine cosine_waves_SF(u,m,wavenum,phi)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          integer :: i
          do i=1,m%s; call cosine_waves(u%BF(i),m%B(i),wavenum,phi,u%DL); enddo
        end subroutine

        subroutine random_noise_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call random_noise(u%BF(i)); enddo
        end subroutine

        subroutine random_noise_SF_dir(u,dir)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          integer :: i
          do i=1,u%s; call random_noise(u%BF(i),dir); enddo
        end subroutine

        subroutine assign_BC_vals_SF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer :: i
          do i=1,A%s; call assign_BC_vals(A%BF(i),B%BF(i)); enddo
        end subroutine

        subroutine update_BC_vals_SF(A)
          implicit none
          type(SF),intent(inout) :: A
          integer :: i
          do i=1,A%s; call update_BC_vals(A%BF(i)); enddo
        end subroutine

        subroutine assign_BCs_SF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer :: i
          do i=1,A%s; call assign_BCs(A%BF(i),B%BF(i)); enddo
        end subroutine
        subroutine assign_Dirichlet_BCs_SF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer :: i
          do i=1,A%s; call assign_Dirichlet_BCs(A%BF(i),B%BF(i)); enddo
        end subroutine
        subroutine assign_Periodic_BCs_SF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer :: i
          do i=1,A%s; call assign_Periodic_BCs(A%BF(i),B%BF(i)); enddo
        end subroutine
        subroutine assign_Neumann_BCs_faces_SF(A,B,dir)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer,intent(in) :: dir
          integer :: i
          do i=1,A%s; call assign_Neumann_BCs(A%BF(i),B%BF(i),dir); enddo
        end subroutine
        subroutine assign_Neumann_BCs_wall_normal_SF(A,B,dir)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer,intent(in) :: dir
          integer :: i
          do i=1,A%s; call assign_Neumann_BCs_wall_normal(A%BF(i),B%BF(i),dir); enddo
        end subroutine
        subroutine multiply_Neumann_BCs_SF(A,scale)
          implicit none
          type(SF),intent(inout) :: A
          real(cp),intent(in) :: scale
          integer :: i
          do i=1,A%s; call multiply_Neumann_BCs(A%BF(i),scale); enddo
        end subroutine
        subroutine multiply_BCs_by_nhat_SF(A)
          implicit none
          type(SF),intent(inout) :: A
          integer :: i
          do i=1,A%s; call multiply_BCs_by_nhat(A%BF(i)); enddo
        end subroutine
        subroutine assign_Robin_BCs_dir_SF(A,B,dir)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer,intent(in) :: dir
          integer :: i
          do i=1,A%s; call assign_Robin_BCs(A%BF(i),B%BF(i),dir); enddo
        end subroutine
        subroutine assign_Robin_BCs_faces_SF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B
          integer :: i
          do i=1,A%s; call assign_Robin_BCs(A%BF(i),B%BF(i)); enddo
        end subroutine

        subroutine multiply_Robin_coeff_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call multiply_Robin_coeff(u%BF(i)); enddo
        end subroutine
        subroutine multiply_nhat_SF(u,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call multiply_nhat(u%BF(i),u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_ghost_XPeriodic_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_ghost_XPeriodic(u%BF(i),val); enddo
        end subroutine
        subroutine assign_ghost_XPeriodic_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_ghost_XPeriodic(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_ghost_N_XPeriodic_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_ghost_N_XPeriodic(u%BF(i),val); enddo
        end subroutine
        subroutine assign_ghost_N_XPeriodic_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_ghost_N_XPeriodic(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_wall_Dirichlet_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_wall_Dirichlet(u%BF(i),val); enddo
        end subroutine
        subroutine assign_wall_Dirichlet_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_wall_Dirichlet(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_wall_Periodic_single_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_wall_Periodic_single(u%BF(i),val); enddo
        end subroutine
        subroutine assign_wall_Periodic_single_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_wall_Periodic_single(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine multiply_wall_Neumann_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call multiply_wall_Neumann(u%BF(i),val); enddo
        end subroutine
        subroutine multiply_wall_Neumann_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call multiply_wall_Neumann(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine set_prescribed_BCs_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call set_prescribed_BCs(u%BF(i)); enddo
        end subroutine

        subroutine set_BCs_homogeneous_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call set_BCs_homogeneous(u%BF(i)); enddo
        end subroutine

        function get_any_Dirichlet_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          integer :: i
          L = any((/(get_any_Dirichlet(u%BF(i)),i=1,u%s)/))
        end function

        function get_any_Neumann_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          integer :: i
          L = any((/(get_any_Neumann(u%BF(i)),i=1,u%s)/))
        end function

        function get_any_Robin_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          integer :: i
          L = any((/(get_any_Robin(u%BF(i)),i=1,u%s)/))
        end function

        function get_any_Prescribed_SF(u) result(L)
          implicit none
          type(SF),intent(in) :: u
          logical :: L
          integer :: i
          L = any((/(get_any_Prescribed(u%BF(i)),i=1,u%s)/))
        end function

        function plane_sum_x_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_x(u%BF(i),m%B(i),p); enddo
        end function

        function plane_sum_y_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_y(u%BF(i),m%B(i),p); enddo
        end function

        function plane_sum_z_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_z(u%BF(i),m%B(i),p); enddo
        end function

        subroutine boundary_flux_SF(BF,x,y,z,m,x_temp,y_temp,z_temp)
          implicit none
          real(cp),intent(inout) :: BF
          type(SF),intent(in) :: x,y,z
          type(mesh),intent(in) :: m
          type(SF),intent(inout) :: x_temp,y_temp,z_temp
          real(cp) :: temp
          integer :: i
          BF = 0.0_cp; temp = 0.0_cp
          do i=1,m%s
            call boundary_flux(temp,x%BF(i),y%BF(i),z%BF(i),m%B(i),&
                x_temp%BF(i),y_temp%BF(i),z_temp%BF(i)); BF = BF+temp
          enddo
        end subroutine

        subroutine boundary_flux_SF_SF(BF,phi,m,temp_phi)
          implicit none
          real(cp),intent(inout) :: BF
          type(SF),intent(in) :: phi
          type(mesh),intent(in) :: m
          type(SF),intent(inout) :: temp_phi
          real(cp) :: temp
          integer :: i
          BF = 0.0_cp; temp = 0.0_cp
          do i=1,m%s;
            call boundary_flux(temp,phi%BF(i),m%B(i),temp_phi%BF(i)); BF = BF+temp
          enddo
        end subroutine

        function symmetry_error_x_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_x(u%BF(i)); enddo
        end function

        function symmetry_error_y_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_y(u%BF(i)); enddo
        end function

        function symmetry_error_z_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_z(u%BF(i)); enddo
        end function

        subroutine symmetry_local_x_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_x(u%BF(i)); enddo
        end subroutine

        subroutine symmetry_local_y_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_y(u%BF(i)); enddo
        end subroutine

        subroutine symmetry_local_z_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_z(u%BF(i)); enddo
        end subroutine

        subroutine mirror_about_hmin_SF(u,dir,mirror_sign)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          real(cp),intent(in) :: mirror_sign
          integer :: i
          do i=1,u%s; call mirror_about_hmin(u%BF(i),dir,mirror_sign); enddo
        end subroutine
        subroutine mirror_about_hmax_SF(u,dir,mirror_sign)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          real(cp),intent(in) :: mirror_sign
          integer :: i
          do i=1,u%s; call mirror_about_hmax(u%BF(i),dir,mirror_sign); enddo
        end subroutine

        subroutine multiply_volume_SF(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(SF) :: vol
          call init(vol,f)
          call volume(vol,m)
          call multiply(f,vol)
          call delete(vol)
        end subroutine

       subroutine mean_along_dir_SF(x_mean,x,m,dir)
         ! Computes  L⁻¹ ∫∫ x_CC dA in plane p along direction dir in grid t
         implicit none
         type(SF),intent(inout) :: x_mean
         type(SF),intent(in) :: x
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: t
         select case (dir)
         case (1); do t=1,m%s; call mean_along_x(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
         case (2); do t=1,m%s; call mean_along_y(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
         case (3); do t=1,m%s; call mean_along_z(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
         case default; stop 'Error: dir must = 1,2,3 in mean_along_dir in SF.f90'
         end select
       end subroutine

       function amax_diff_SF(x) result(x_amax_diff)
         implicit none
         type(SF),intent(in) :: x
         real(cp),dimension(3) :: x_amax_diff
         real(cp),dimension(3) :: temp
         integer :: t
         x_amax_diff = 0.0_cp
         do t=1,x%s
         temp = amax_diff(x%BF(t)%GF)
         x_amax_diff(1) = maxval((/x_amax_diff(1),temp(1)/))
         x_amax_diff(2) = maxval((/x_amax_diff(2),temp(2)/))
         x_amax_diff(3) = maxval((/x_amax_diff(3),temp(3)/))
         enddo
       end function

       function amax_diff_dir_SF(x,dir) result(x_amax_diff)
         implicit none
         type(SF),intent(in) :: x
         integer,intent(in) :: dir
         real(cp) :: x_amax_diff
         integer :: t
         x_amax_diff = 0.0_cp
         do t=1,x%s
         x_amax_diff = maxval((/x_amax_diff,amax_diff(x%BF(t)%GF,dir)/))
         enddo
       end function

       subroutine subtract_mean_along_dir_SF(x,m,dir,x_temp)
         implicit none
         type(SF),intent(inout) :: x,x_temp
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         call mean_along_dir(x_temp,x,m,dir)
         call subtract(x,x_temp)
       end subroutine

        subroutine print_BCs_SF(f,name)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: name
          call display_BCs_SF(f,name,6)
        end subroutine

        subroutine export_BCs_SF(f,dir,name)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = new_and_open(dir,name//'_BoundaryConditions')
          call display_BCs_SF(f,name,un)
          call close_and_message(un,dir,name//'_BoundaryConditions')
        end subroutine

        subroutine display_BCs_SF(f,name,un)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: name
          integer,intent(in) :: un
          integer :: i
          write(un,*) ' ------ BCs for ' // name // ' ------ '
          do i=1,f%s
            call display(f%BF(i)%BCs,un)
          enddo
          write(un,*) ' ------------------------------------ '
        end subroutine

        ! ****************************************************************
        ! ****************************************************************
        ! ********************* COMPUTATION ROUTINES *********************
        ! ****************************************************************
        ! ****************************************************************

        subroutine assign_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine assign_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine assign_negative_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign_negative(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF_SF_SF(f,g,r,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r,q
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine add_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call add(g2,f%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
          integer :: i
          do i=1,A%s; call add(A%BF(i)%GF,B1%BF(i)%GF,B2%BF(i)%GF,B3%BF(i)%GF,&
                                       B4%BF(i)%GF,B5%BF(i)%GF,B6%BF(i)%GF,&
                                       B7%BF(i)%GF,B8%BF(i)%GF,B9%BF(i)%GF); enddo
        end subroutine

       ! ------------------- ADD PRODUCT ------------------------

        subroutine add_product_SF_SF_S(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%BF(i)%GF,g%BF(i)%GF,r); enddo
        end subroutine

        subroutine add_product_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          type(SF),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF); enddo
        end subroutine

        subroutine product_add_SF_SF_S(f,r,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: r
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call product_add(f%BF(i)%GF,r,g%BF(i)%GF); enddo
        end subroutine

        subroutine product_add_SF_SF_SF(f,r,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: r
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call product_add(f%BF(i)%GF,r%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine subtract_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine subtract_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine subtract_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call subtract(g2,f%BF(i)%GF); enddo
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine multiply_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine multiply_SF_SF_S(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: q
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF,q); enddo
        end subroutine

        subroutine multiply_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine multiply_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g2); enddo
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_S_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: q
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g,q%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine divide_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call divide(g2,f%BF(i)%GF); enddo
        end subroutine

      ! ------------------- Magnitude ------------------------

        subroutine magnitude_SF_SF_SF(R,x,y,z)
          implicit none
          type(SF),intent(inout) :: R
          type(SF),intent(in) :: x,y,z
          integer :: i
          do i=1,R%s; call magnitude(R%BF(i)%GF,&
                                     x%BF(i)%GF,&
                                     y%BF(i)%GF,&
                                     z%BF(i)%GF); enddo
        end subroutine

      ! ------------------- OTHER ------------------------

        subroutine invert_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call divide(1.0_cp,f%BF(i)%GF); enddo
        end subroutine

        subroutine square_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square(f%BF(i)%GF); enddo
        end subroutine

        subroutine square_root_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square_root(f%BF(i)%GF); enddo
        end subroutine

        subroutine abs_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call abs(u%BF(i)); enddo
        end subroutine

        subroutine insist_amax_lt_tol_SF(u,caller)
          implicit none
          type(SF),intent(in) :: u
          character(len=*),intent(in) :: caller
          integer :: i
          do i=1,u%s; call insist_amax_lt_tol(u%BF(i),caller); enddo
        end subroutine

        subroutine swap_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f,g,q
          integer :: i
          do i=1,f%s; call swap(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        function min_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval((/m,min(f%BF(i)%GF)/))
          enddo
        end function

        function max_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval((/m,max(f%BF(i)%GF)/))
          enddo
        end function

        function min_pad_SF(f,pad) result(m)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval((/m,min(f%BF(i)%GF,pad)/))
          enddo
        end function

        function max_pad_SF(f,pad) result(m)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval((/m,max(f%BF(i)%GF,pad)/))
          enddo
        end function

        function amin_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval(abs((/m,amin(f%BF(i)%GF)/)))
          enddo
        end function

        function amax_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval(abs((/m,amax(f%BF(i)%GF)/)))
          enddo
        end function

        function mean_SF(a) result(m)
          implicit none
          type(SF),intent(in) :: a
          real(cp) :: m
          integer :: i,s
          m = sum(a)
          s = 0
          do i=1,a%s
            s = s + size(a%BF(i)%GF)
          enddo
          m = m/real(s,cp)
        end function

        function sum_SF(a) result(m)
          implicit none
          type(SF),intent(in) :: a
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,a%s
            m = m + sum(a%BF(i)%GF)
          enddo
        end function

        function sum_SF_pad(a,pad) result(m)
          implicit none
          type(SF),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,a%s
            m = m + sum(a%BF(i)%GF,pad)
          enddo
        end function

        function dot_product_SF(A,B,temp) result(dot)
          implicit none
          type(SF),intent(in) :: A,B
          type(SF),intent(inout) :: temp
          real(cp) :: dot
          call multiply(temp,A,B)
          dot = sum(temp)
        end function

        subroutine assign_ghost_xmin_xmax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_xmin_xmax(f%BF(t),val); enddo
        end subroutine
        subroutine assign_ghost_ymin_ymax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_ymin_ymax(f%BF(t),val); enddo
        end subroutine
        subroutine assign_ghost_zmin_zmax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_zmin_zmax(f%BF(t),val); enddo
        end subroutine

        subroutine cross_product_x_SF(ACrossB,Ay,Az,By,Bz)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ay,Az,By,Bz
          integer :: t
          do t=1,ACrossB%s
            call cross_product_x(ACrossB%BF(t),Ay%BF(t),Az%BF(t),By%BF(t),Bz%BF(t))
          enddo
        end subroutine
        subroutine cross_product_y_SF(ACrossB,Ax,Az,Bx,Bz)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ax,Az,Bx,Bz
          integer :: t
          do t=1,ACrossB%s
            call cross_product_y(ACrossB%BF(t),Ax%BF(t),Az%BF(t),Bx%BF(t),Bz%BF(t))
          enddo
        end subroutine
        subroutine cross_product_z_SF(ACrossB,Ax,Ay,Bx,By)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ax,Ay,Bx,By
          integer :: t
          do t=1,ACrossB%s
            call cross_product_z(ACrossB%BF(t),Ax%BF(t),Ay%BF(t),Bx%BF(t),By%BF(t))
          enddo
        end subroutine

        function CFL_number_SF(U_CC,V_CC,W_CC,m,dt) result(CFL)
          implicit none
          type(SF),intent(in) :: U_CC,V_CC,W_CC
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: dt
          type(array) :: a
          real(cp) :: CFL
          integer :: t
          call init(a,m%s)
          do t=1,m%s
            call init(a,CFL_number(U_CC%BF(t),V_CC%BF(t),W_CC%BF(t),m%B(t),dt),t)
          enddo
          CFL = maxval(a%f)
          call delete(a)
        end function

        function dt_given_CFL_number_SF(U_CC,V_CC,W_CC,m,CFL) result(dt)
          implicit none
          type(SF),intent(in) :: U_CC,V_CC,W_CC
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: CFL
          type(array) :: a
          real(cp) :: dt
          integer :: t
          call init(a,m%s)
          do t=1,m%s
            call init(a,dt_given_CFL_number(U_CC%BF(t),V_CC%BF(t),W_CC%BF(t),m%B(t),CFL),t)
          enddo
          dt = minval(a%f)
          call delete(a)
        end function

        function Fourier_number_SF(alpha,m,dt) result(Fourier)
          implicit none
          real(cp),intent(in) :: alpha
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: dt
          type(array) :: a
          real(cp) :: Fourier
          integer :: t
          call init(a,m%s)
          do t=1,m%s
            call init(a,Fourier_number(alpha,m%B(t),dt),t)
          enddo
          Fourier = maxval(a%f)
          call delete(a)
        end function

        function Robin_BC_coeff_SF(c_w,m) result(coeff)
          implicit none
          real(cp),dimension(6),intent(in) :: c_w
          type(mesh),intent(in) :: m
          real(cp),dimension(6) :: coeff
          real(cp),dimension(6) :: temp
          integer :: t
          coeff = 0.0_cp
          temp = coeff
          do t=1,m%s
            temp = Robin_BC_coeff(c_w,m%B(t))
            coeff = temp ! BAD DESIGN, BUT NOT SURE WHAT TO DO HERE...
          enddo
        end function

        subroutine restrict_SF(r,u,m,dir)
          implicit none
          type(SF),intent(inout) :: r
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call restrict(r%BF(t),u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine restrict_reset_SF(u,m,dir)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call restrict(u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine restrict_all_reset_SF(u,m)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,3; call restrict(u,m,i); enddo
        end subroutine

        subroutine prolongate_SF(r,u,m,dir)
          implicit none
          type(SF),intent(inout) :: r
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call prolongate(r%BF(t),u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine prolongate_reset_SF(u,m,dir)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call prolongate(u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine prolongate_all_reset_SF(u,m)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,3; call prolongate(u,m,i); enddo
        end subroutine

      end module