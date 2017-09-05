       module single_boundary_extend_mod
       use single_boundary_mod
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

       interface init;        module procedure init_GFs_SB_DL;  end interface
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