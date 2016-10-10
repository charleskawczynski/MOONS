      module block_field_mod
        use current_precision_mod
        use grid_mod
        use data_location_mod
        use GF_mod
        use block_mod
        use BCs_mod
        use boundary_conditions_mod
        implicit none
        private

        public :: block_field
        public :: init,delete,display,print,export,import ! Essentials

        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: init_BCs

        type block_field
          type(grid_field) :: GF ! bulk
          type(BCs) :: b
          type(boundary_conditions) :: BCs_
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
       interface init_BCs;           module procedure init_BC_vals;                   end interface
       interface init_BCs;           module procedure init_BC_block_DL;               end interface

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
         if (b_in%b%defined) call init(b_out%b,b_in%b)
       end subroutine

       subroutine delete_block_field(BF)
         implicit none
         type(block_field),intent(inout) :: BF
         call delete(BF%GF)
         call delete(BF%b)
       end subroutine

       subroutine display_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call display(BF%GF,un)
         call display(BF%b,un)
       end subroutine

       subroutine print_block_field(BF)
         implicit none
         type(block_field),intent(in) :: BF
         call print(BF%GF)
         call print(BF%b)
       end subroutine

       subroutine export_block_field(BF,un)
         implicit none
         type(block_field),intent(in) :: BF
         integer,intent(in) :: un
         call export(BF%GF,un)
         call export(BF%b,un)
       end subroutine

       subroutine import_block_field(BF,un)
         implicit none
         type(block_field),intent(inout) :: BF
         integer,intent(in) :: un
         call import(BF%GF,un)
         call import(BF%b,un)
       end subroutine

       subroutine init_BC_val(BF,val)
         implicit none
         type(block_field),intent(inout) :: BF
         real(cp),intent(in) :: val
         call init(BF%b,val)
       end subroutine

       subroutine init_BC_block_DL(BF,B,DL)
         implicit none
         type(block_field),intent(inout) :: BF
         type(block),intent(in) :: B
         type(data_location),intent(in) :: DL
         call init(BF%BCs_,B,DL)
       end subroutine

       subroutine init_BC_vals(BF,is_CC,is_Node)
         implicit none
         type(block_field),intent(inout) :: BF
         logical,intent(in) :: is_CC,is_Node
         logical,dimension(2) :: L
         L = (/is_CC,is_Node/)
         if (count(L).gt.1) then
           stop 'Error: more than one datatype in init_BC_vals in GF.f90'
         endif
         if (is_Node) then
           call init(BF%b,BF%GF%f(2,:,:),1)
           call init(BF%b,BF%GF%f(:,2,:),3)
           call init(BF%b,BF%GF%f(:,:,2),5)
           call init(BF%b,BF%GF%f(BF%GF%s(1)-1,:,:),2)
           call init(BF%b,BF%GF%f(:,BF%GF%s(2)-1,:),4)
           call init(BF%b,BF%GF%f(:,:,BF%GF%s(3)-1),6)
         elseif (is_CC) then
           call init(BF%b,0.5_cp*(BF%GF%f(1,:,:)+BF%GF%f(2,:,:)),1)
           call init(BF%b,0.5_cp*(BF%GF%f(:,1,:)+BF%GF%f(:,2,:)),3)
           call init(BF%b,0.5_cp*(BF%GF%f(:,:,1)+BF%GF%f(:,:,2)),5)
           call init(BF%b,0.5_cp*(BF%GF%f(BF%GF%s(1),:,:)+BF%GF%f(BF%GF%s(1)-1,:,:)),2)
           call init(BF%b,0.5_cp*(BF%GF%f(:,BF%GF%s(2),:)+BF%GF%f(:,BF%GF%s(2)-1,:)),4)
           call init(BF%b,0.5_cp*(BF%GF%f(:,:,BF%GF%s(3))+BF%GF%f(:,:,BF%GF%s(3)-1)),6)
         else
           stop 'Error: field-based BC init is only available for N / CC data.'
         endif
       end subroutine


      end module