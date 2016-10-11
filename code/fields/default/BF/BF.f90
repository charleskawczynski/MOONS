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

       subroutine init_block_field_copy(b_out,b_in)
         implicit none
         type(block_field),intent(inout) :: b_out
         type(block_field),intent(in) :: b_in
         call init(b_out%GF,b_in%GF)
         if (b_in%BCs%BCL%defined) call init(b_out%BCs,b_in%BCs)
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

       subroutine volume_BF(u,B,DL)
         ! Computes
         ! 
         !   volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
         implicit none
         type(block_field),intent(inout) :: u
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         integer :: i,j,k
         call assign(u%GF,0.0_cp)
         if (is_CC(DL)) then
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhn(i)*&
                             B%g%c(2)%dhn(j)*&
                             B%g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (is_Node(DL)) then
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhc(i-1)*&
                             B%g%c(2)%dhc(j-1)*&
                             B%g%c(3)%dhc(k-1)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (is_Face(DL)) then
         select case (DL%face)
         case (1);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhc(i-1)*&
                             B%g%c(2)%dhn(j)*&
                             B%g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (2);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhn(i)*&
                             B%g%c(2)%dhc(j-1)*&
                             B%g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (3);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhn(i)*&
                             B%g%c(2)%dhn(j)*&
                             B%g%c(3)%dhc(k-1)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case default; stop 'Error: bad face location in volume_SF in ops_aux.f90'
         end select
         elseif (is_Edge(DL)) then
         select case (DL%edge)
         case (1);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhn(i)*&
                             B%g%c(2)%dhc(j-1)*&
                             B%g%c(3)%dhc(k-1)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (2);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhc(i-1)*&
                             B%g%c(2)%dhn(j)*&
                             B%g%c(3)%dhc(k-1)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (3);
         !$OMP PARALLEL DO SHARED(B)
         do k=2,u%GF%s(3)-1; do j=2,u%GF%s(2)-1; do i=2,u%GF%s(1)-1
             u%GF%f(i,j,k) = B%g%c(1)%dhc(i-1)*&
                             B%g%c(2)%dhc(j-1)*&
                             B%g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case default; stop 'Error: bad edge location in volume_BF in BF.f90'
         end select
         else; stop 'Error: bad location in volume_BF in BF.f90'
         endif
       end subroutine

      end module