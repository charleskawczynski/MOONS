      module block_field_mod
        use current_precision_mod
        use grid_mod
        use data_location_mod
        use GF_mod
        use block_mod
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
        public :: assign_ghost

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

        type block_field
          type(grid_field) :: GF ! bulk
          type(boundary_conditions) :: BCs
          ! type(stitches) :: st
        end type

       interface init_CC;            module procedure init_CC_BF;                     end interface
       interface init_Face;          module procedure init_Face_BF;                   end interface
       interface init_Edge;          module procedure init_Edge_BF;                   end interface
       interface init_Node;          module procedure init_Node_BF;                   end interface
       interface init;               module procedure init_block_field_copy;          end interface
       interface delete;             module procedure delete_block_field;             end interface
       interface display;            module procedure display_block_field;            end interface
       interface print;              module procedure print_block_field;              end interface
       interface export;             module procedure export_block_field;             end interface
       interface import;             module procedure import_block_field;             end interface

       interface init_BCs;           module procedure init_BC_val;                    end interface
       interface init_BCs;           module procedure init_BC_block_DL;               end interface
       interface init_BC_props;      module procedure init_BC_props_BF;               end interface

       interface volume;             module procedure volume_BF;                      end interface
       interface cosine_waves;       module procedure cosine_waves_BF;                end interface
       interface sine_waves;         module procedure sine_waves_BF;                  end interface
       interface random_noise;       module procedure random_noise_BF;                end interface

       interface square;             module procedure square_BF;                      end interface
       interface abs;                module procedure abs_BF;                         end interface
       interface insist_amax_lt_tol; module procedure insist_amax_lt_tol_BF;          end interface

       interface assign_ghost;       module procedure assign_ghost_BF;                end interface

       interface plane_sum_x;        module procedure plane_sum_x_BF;                 end interface
       interface plane_sum_y;        module procedure plane_sum_y_BF;                 end interface
       interface plane_sum_z;        module procedure plane_sum_z_BF;                 end interface

       interface symmetry_error_x;   module procedure symmetry_error_x_BF;            end interface
       interface symmetry_error_y;   module procedure symmetry_error_y_BF;            end interface
       interface symmetry_error_z;   module procedure symmetry_error_z_BF;            end interface

       interface symmetry_local_x;   module procedure symmetry_local_x_BF;            end interface
       interface symmetry_local_y;   module procedure symmetry_local_y_BF;            end interface
       interface symmetry_local_z;   module procedure symmetry_local_z_BF;            end interface

       ! interface assign_plane_x;     module procedure assign_plane_x_BF;              end interface
       ! interface assign_plane_y;     module procedure assign_plane_y_BF;              end interface
       ! interface assign_plane_z;     module procedure assign_plane_z_BF;              end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_CC_BF(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         call init_CC(BF%GF,B%g)
       end subroutine

       subroutine init_Face_BF(BF,B,dir)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         call init_Face(BF%GF,B%g,dir)
       end subroutine

       subroutine init_Edge_BF(BF,B,dir)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         integer,intent(in) :: dir
         call init_Edge(BF%GF,B%g,dir)
       end subroutine

       subroutine init_Node_BF(BF,B)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         call init_Node(BF%GF,B%g)
       end subroutine

       subroutine init_block_field_copy(BF,BF_in)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block_field),intent(in) :: BF_in
         call init(BF%GF,BF_in%GF)
         if (BF_in%BCs%BCL%defined) call init(BF%BCs,BF_in%BCs)
       end subroutine

       subroutine delete_block_field(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%GF)
         call delete(BF%BCs)
       end subroutine

       subroutine display_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call display(BF%GF,un)
         call display(BF%BCs,un)
       end subroutine

       subroutine print_block_field(BF)
         implicit none
         type(block_field),intent(in) :: BF
         call print(BF%GF)
         call print(BF%BCs)
       end subroutine

       subroutine export_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call export(BF%GF,un)
         call export(BF%BCs,un)
       end subroutine

       subroutine import_block_field(BF,un)
         implicit none
         type(block_field),intent(inout) :: BF
         integer,intent(in) :: un
         call import(BF%GF,un)
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

       subroutine assign_ghost_BF(u,val)
         implicit none
         type(block_field),intent(inout) :: u
         real(cp),intent(in) :: val
         call assign_ghost(u%GF,val)
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