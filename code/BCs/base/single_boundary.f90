       module single_boundary_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use grid_mod
       use block_mod
       use GF_mod
       use bctype_mod
       use IO_tools_mod
       implicit none

       private
       public :: single_boundary
       public :: init,delete,display,print,export,import ! Essentials

       public :: prolongate
       public :: restrict

       type single_boundary
         type(grid_field) :: b
         type(grid_field) :: b_modified
         type(grid_field) :: b_total
         type(bctype) :: bct
       end type

       interface init;        module procedure init_GFs_SB_DL;  end interface
       interface init;        module procedure init_SB_copy;    end interface

       interface delete;      module procedure delete_SB;       end interface
       interface display;     module procedure display_SB;      end interface
       interface print;       module procedure print_SB;        end interface
       interface export;      module procedure export_SB;       end interface
       interface import;      module procedure import_SB;       end interface

       interface prolongate;  module procedure prolongate_B;    end interface
       interface restrict;    module procedure restrict_B;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_GFs_SB_DL(SB,g,DL)
         implicit none
         type(single_boundary),intent(inout) :: SB
         type(grid),intent(in) :: g
         type(data_location),intent(in) :: DL
         call delete(SB)
               if (is_CC(DL)) then; call init_CC(  SB%b,g)
         elseif (is_Node(DL)) then; call init_Node(SB%b,g)
         elseif (is_Face(DL)) then; call init_Face(SB%b,g,get_Face(DL))
         elseif (is_Edge(DL)) then; call init_Edge(SB%b,g,get_Edge(DL))
         else; stop 'Error: bad DL f in init_GFs_SB_DL in single_boundary.f90'
         endif
               if (is_CC(DL)) then; call init_CC(  SB%b_modified,g)
         elseif (is_Node(DL)) then; call init_Node(SB%b_modified,g)
         elseif (is_Face(DL)) then; call init_Face(SB%b_modified,g,get_Face(DL))
         elseif (is_Edge(DL)) then; call init_Edge(SB%b_modified,g,get_Edge(DL))
         else; stop 'Error: bad DL f in init_GFs_SB_DL in single_boundary.f90'
         endif
               if (is_CC(DL)) then; call init_CC(  SB%b_total,g)
         elseif (is_Node(DL)) then; call init_Node(SB%b_total,g)
         elseif (is_Face(DL)) then; call init_Face(SB%b_total,g,get_Face(DL))
         elseif (is_Edge(DL)) then; call init_Edge(SB%b_total,g,get_Edge(DL))
         else; stop 'Error: bad DL f in init_GFs_SB_DL in single_boundary.f90'
         endif
         call assign(SB%b,0.0_cp)
         call assign(SB%b_modified,0.0_cp)
         call assign(SB%b_total,0.0_cp)
       end subroutine

       subroutine init_SB_copy(SB,SB_in)
         implicit none
         type(single_boundary),intent(inout) :: SB
         type(single_boundary),intent(in) :: SB_in
         call delete(SB)
         call init(SB%b,SB_in%b)
         call assign(SB%b,SB_in%b)

         call init(SB%b_modified,SB_in%b_modified)
         call assign(SB%b_modified,SB_in%b_modified)

         call init(SB%b_total,SB_in%b_total)
         call assign(SB%b_total,SB_in%b_total)

         call init(SB%bct,SB_in%bct)
       end subroutine

       subroutine delete_SB(SB)
         implicit none
         type(single_boundary),intent(inout) :: SB
         call delete(SB%b)
         call delete(SB%b_modified)
         call delete(SB%b_total)
         call delete(SB%bct)
       end subroutine

       subroutine display_SB(SB,un)
         implicit none
         type(single_boundary),intent(in) :: SB
         integer,intent(in) :: un
         write(un,*) 'amax(b         ) = ',amax(SB%b)
         write(un,*) 'amax(b_modified) = ',amax(SB%b_modified)
         write(un,*) 'amax(b_total   ) = ',amax(SB%b_total)
         call display(SB%bct,un)
       end subroutine

       subroutine print_SB(SB)
         implicit none
         type(single_boundary),intent(in) :: SB
         call display(SB,6)
       end subroutine

       subroutine export_SB(SB,un)
         implicit none
         type(single_boundary),intent(in) :: SB
         integer,intent(in) :: un
         call export(SB%b,un)
         call export(SB%b_modified,un)
         call export(SB%b_total,un)
         call export(SB%bct,un)
       end subroutine

       subroutine import_SB(SB,un)
         implicit none
         type(single_boundary),intent(inout) :: SB
         integer,intent(in) :: un
         call import(SB%b,un)
         call import(SB%b_modified,un)
         call import(SB%b_total,un)
         call import(SB%bct,un)
       end subroutine

       subroutine restrict_B(SB,g,DL,dir,x,y,z)
         implicit none
         integer,intent(in) :: dir,x,y,z
         type(single_boundary),intent(inout) :: SB
         type(grid),intent(in) :: g
         type(data_location),intent(in) :: DL
             if (CC_along(DL,dir)) then; call restrict_C(SB%b,g,dir,x,y,z)
                                         call restrict_C(SB%b_total,g,dir,x,y,z)
                                         call restrict_C(SB%b_modified,g,dir,x,y,z)
         elseif ( N_along(DL,dir)) then; call restrict_N(SB%b,g,dir,x,y,z)
                                         call restrict_N(SB%b_total,g,dir,x,y,z)
                                         call restrict_N(SB%b_modified,g,dir,x,y,z)
         else; stop 'Error: bad DL in restrict_B in single_boundary.f90'
         endif
       end subroutine

       subroutine prolongate_B(SB,g,DL,dir,x,y,z)
         implicit none
         type(single_boundary),intent(inout) :: SB
         integer,intent(in) :: dir,x,y,z
         type(grid),intent(in) :: g
         type(data_location),intent(in) :: DL
             if (CC_along(DL,dir)) then; call prolongate_C(SB%b,g,dir,x,y,z)
                                         call prolongate_C(SB%b_total,g,dir,x,y,z)
                                         call prolongate_C(SB%b_modified,g,dir,x,y,z)
         elseif ( N_along(DL,dir)) then; call prolongate_N(SB%b,dir,x,y,z)
                                         call prolongate_N(SB%b_total,dir,x,y,z)
                                         call prolongate_N(SB%b_modified,dir,x,y,z)
         else; stop 'Error: bad DL in prolongate_B in single_boundary.f90'
         endif
       end subroutine

       end module