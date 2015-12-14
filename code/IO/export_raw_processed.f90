       module export_raw_processed_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use ops_interp_mod
       
       implicit none

       private
       public :: export_raw       ! call export_raw(m,x,dir,name,pad)
       public :: export_processed ! call export_processed(m,x,dir,name,pad)

       interface export_raw;       module procedure export_raw_SF;        end interface
       interface export_raw;       module procedure export_raw_VF;        end interface
       interface export_processed; module procedure export_processed_SF;  end interface
       interface export_processed; module procedure export_processed_VF;  end interface

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine export_raw_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (x%is_CC) then
           call export_3D_1C(m,x,dir,name//'c',pad)
         elseif (x%is_Node) then
           call export_3D_1C(m,x,dir,name//'n',pad)
         elseif (x%is_Face) then
           select case (x%face)
           case (1); call export_3D_1C(m,x,dir,name//'f_x',pad)
           case (2); call export_3D_1C(m,x,dir,name//'f_y',pad)
           case (3); call export_3D_1C(m,x,dir,name//'f_z',pad)
           case default; stop 'Error: face must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
           end select
         elseif (x%is_Edge) then
           select case (x%edge)
           case (1); call export_3D_1C(m,x,dir,name//'e_x',pad)
           case (2); call export_3D_1C(m,x,dir,name//'e_y',pad)
           case (3); call export_3D_1C(m,x,dir,name//'e_z',pad)
           case default; stop 'Error: edge must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
           end select
         endif
       end subroutine

       subroutine export_raw_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (x%x%is_CC) then
           call export_3D_3C(m,x,dir,name//'c',pad)
         elseif (x%x%is_Node) then
           call export_3D_3C(m,x,dir,name//'n',pad)
         elseif (x%x%is_Face) then
           call export_3D_1C(m,x%x,dir,name//'f_x',pad)
           call export_3D_1C(m,x%y,dir,name//'f_y',pad)
           call export_3D_1C(m,x%z,dir,name//'f_z',pad)
         elseif (x%x%is_Edge) then
           call export_3D_1C(m,x%x,dir,name//'e_x',pad)
           call export_3D_1C(m,x%y,dir,name//'e_y',pad)
           call export_3D_1C(m,x%z,dir,name//'e_z',pad)
         endif
       end subroutine

       subroutine export_processed_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(SF) :: temp_1,temp_2,temp_N
         if (x%is_CC) then
           call init_Face(temp_1,m,1); call init_Edge(temp_2,m,3); call init_Node(temp_N,m)
           call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
           call export_3D_1C(m,temp_N,dir,name//'np',pad)
           call delete(temp_1); call delete(temp_2); call delete(temp_N)
         elseif (x%is_Node) then
           call export_3D_1C(m,x,dir,name//'np',pad)
         elseif (x%is_Face) then
           select case (x%face)
           case (1); call init_Edge(temp_1,m,2); call init_Node(temp_N,m); 
                     call face2Node(temp_N,x,m,x%face,temp_1)
                     call export_3D_1C(m,x,dir,name//'np_x',pad)
                     call delete(temp_1); call delete(temp_N)
           case (2); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                     call face2Node(temp_N,x,m,x%face,temp_1)
                     call export_3D_1C(m,x,dir,name//'np_y',pad)
                     call delete(temp_1); call delete(temp_N)
           case (3); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                     call face2Node(temp_N,x,m,x%face,temp_1)
                     call export_3D_1C(m,x,dir,name//'np_z',pad)
                     call delete(temp_1); call delete(temp_N)
           case default; stop 'Error: face must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
           end select
         elseif (x%is_Edge) then
           select case (x%edge)
           case (1); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                     call export_3D_1C(m,x,dir,name//'ep_x',pad)
                     call delete(temp_N)
           case (2); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                     call export_3D_1C(m,x,dir,name//'ep_y',pad)
                     call delete(temp_N)
           case (3); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                     call export_3D_1C(m,x,dir,name//'ep_z',pad)
                     call delete(temp_N)
           case default; stop 'Error: edge must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
           end select
         endif
       end subroutine

       subroutine export_processed_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(VF) :: temp_1,temp_2,temp_N
         if (x%x%is_CC) then
           call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
           call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
           call export_3D_3C(m,temp_N,dir,name//'np',pad)
           call delete(temp_1); call delete(temp_2); call delete(temp_N)
         elseif (x%x%is_Node) then
           call export_3D_3C(m,x,dir,name//'np',pad)
         elseif (x%x%is_Face) then
           call init_Edge(temp_1,m); call init_Node(temp_N,m); 
           call face2Node(temp_N,x,m,temp_1)
           call export_3D_3C(m,x,dir,name//'np',pad)
           call delete(temp_1); call delete(temp_N)
         elseif (x%x%is_Edge) then
           call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
           call export_3D_3C(m,x,dir,name//'np',pad)
           call delete(temp_N)
         endif
       end subroutine

       end module