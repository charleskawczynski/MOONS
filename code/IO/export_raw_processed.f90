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
       public :: export_raw                 ! call export_raw(m,x,dir,name,pad)
       public :: export_processed           ! call export_processed(m,x,dir,name,pad)
       public :: export_processed_transient ! call export_processed_transient(m,x,dir,name,pad,nstep)

       logical :: export_planar = .true.

       interface export_raw;                 module procedure export_raw_SF;                  end interface
       interface export_raw;                 module procedure export_raw_VF;                  end interface
       interface export_processed;           module procedure export_processed_SF;            end interface
       interface export_processed;           module procedure export_processed_VF;            end interface

       interface export_processed_transient; module procedure export_processed_transient_VF;  end interface

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
         if (.not.export_planar) then; call export_raw_SF_func(export_3D_1C,m,x,dir,name,pad,0)
         else; if (m%plane_x) then;    call export_raw_SF_func(export_2D_1C,m,x,dir,name,pad,1)
         elseif   (m%plane_y) then;    call export_raw_SF_func(export_2D_1C,m,x,dir,name,pad,2)
         elseif   (m%plane_z) then;    call export_raw_SF_func(export_2D_1C,m,x,dir,name,pad,3)
         else; stop 'Error: attempted plane export of 3D geometry in export_raw_SF in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_raw_SF_func(func,m,x,dir,name,pad,direction)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         if (.not.export_planar) then
           if (x%is_CC) then;       call func(m,x,dir,name//'c',pad)
           elseif (x%is_Node) then; call func(m,x,dir,name//'n',pad)
           elseif (x%is_Face) then
             select case (x%face)
             case (1); call func(m,x,dir,name//'f_x',pad)
             case (2); call func(m,x,dir,name//'f_y',pad)
             case (3); call func(m,x,dir,name//'f_z',pad)
             case default; stop 'Error: face must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
             end select
           elseif (x%is_Edge) then
             select case (x%edge)
             case (1); call func(m,x,dir,name//'e_x',pad)
             case (2); call func(m,x,dir,name//'e_y',pad)
             case (3); call func(m,x,dir,name//'e_z',pad)
             case default; stop 'Error: edge must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
             end select
           endif
         else
           if (x%is_CC) then;       call func(m,x,dir,name//'c',pad,direction)
           elseif (x%is_Node) then; call func(m,x,dir,name//'n',pad,direction)
           elseif (x%is_Face) then
             select case (x%face)
             case (1); call func(m,x,dir,name//'f_x',pad,direction)
             case (2); call func(m,x,dir,name//'f_y',pad,direction)
             case (3); call func(m,x,dir,name//'f_z',pad,direction)
             case default; stop 'Error: face must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
             end select
           elseif (x%is_Edge) then
             select case (x%edge)
             case (1); call func(m,x,dir,name//'e_x',pad,direction)
             case (2); call func(m,x,dir,name//'e_y',pad,direction)
             case (3); call func(m,x,dir,name//'e_z',pad,direction)
             case default; stop 'Error: edge must = 1,2,3 in export_raw_SF in export_raw_processed.f90'
             end select
           endif
         endif
       end subroutine

       subroutine export_raw_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (.not.export_planar) then;
           if (x%is_CC.or.x%is_Node) then;       call export_raw_VF_func(export_3D_3C,m,x,dir,name,pad,0)
           elseif (x%is_Face.or.x%is_Edge) then; call export_raw_VF_func(export_3D_1C,m,x,dir,name,pad,0)
           else; stop 'Error: bad input to export_raw_VF in export_processed.f90'
           endif
         else; if (m%plane_x) then;    call export_raw_VF_func(export_2D_1C,m,x,dir,name,pad,1)
         elseif   (m%plane_y) then;    call export_raw_VF_func(export_2D_1C,m,x,dir,name,pad,2)
         elseif   (m%plane_z) then;    call export_raw_VF_func(export_2D_1C,m,x,dir,name,pad,3)
         else; stop 'Error: attempted plane export of 3D geometry in export_raw_VF in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_raw_VF_func(func,m,x,dir,name,pad,direction)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         if (.not.export_planar) then
           if (x%is_CC) then;       call func(m,x,dir,name//'c',pad)
           elseif (x%is_Node) then; call func(m,x,dir,name//'n',pad)
           elseif (x%is_Face) then
             call func(m,x%x,dir,name//'f_x',pad)
             call func(m,x%y,dir,name//'f_y',pad)
             call func(m,x%z,dir,name//'f_z',pad)
           elseif (x%is_Edge) then
             call func(m,x%x,dir,name//'e_x',pad)
             call func(m,x%y,dir,name//'e_y',pad)
             call func(m,x%z,dir,name//'e_z',pad)
           else; stop 'Error: bad input to export_raw_VF_func (1) in export_processed.f90'
           endif
         else
           if (x%is_CC) then;       call func(m,x,dir,name//'c',pad,direction)
           elseif (x%is_Node) then; call func(m,x,dir,name//'n',pad,direction)
           elseif (x%is_Face) then
             call func(m,x%x,dir,name//'f_x',pad,direction)
             call func(m,x%y,dir,name//'f_y',pad,direction)
             call func(m,x%z,dir,name//'f_z',pad,direction)
           elseif (x%is_Edge) then
             call func(m,x%x,dir,name//'e_x',pad,direction)
             call func(m,x%y,dir,name//'e_y',pad,direction)
             call func(m,x%z,dir,name//'e_z',pad,direction)
           else; stop 'Error: bad input to export_raw_VF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       subroutine export_processed_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (.not.export_planar) then; call export_processed_SF_func(export_3D_1C,m,x,dir,name,pad,0)
         else; if (m%plane_x) then;    call export_processed_SF_func(export_2D_1C,m,x,dir,name,pad,1)
         elseif   (m%plane_y) then;    call export_processed_SF_func(export_2D_1C,m,x,dir,name,pad,2)
         elseif   (m%plane_z) then;    call export_processed_SF_func(export_2D_1C,m,x,dir,name,pad,3)
         else; stop 'Error: attempted plane export of 3D geometry in export_raw_VF in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_processed_SF_func(func,m,x,dir,name,pad,direction)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         type(SF) :: temp_1,temp_2,temp_N
         if (.not.export_planar) then
           if (x%is_CC) then
             call init_Face(temp_1,m,1); call init_Edge(temp_2,m,3); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call func(m,temp_N,dir,name//'np',pad)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call func(m,x,dir,name//'np',pad)
           elseif (x%is_Face) then
             select case (x%face)
             case (1); call init_Edge(temp_1,m,2); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_x',pad)
                       call delete(temp_1); call delete(temp_N)
             case (2); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_y',pad)
                       call delete(temp_1); call delete(temp_N)
             case (3); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_z',pad)
                       call delete(temp_1); call delete(temp_N)
             case default; stop 'Error: face must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
             end select
           elseif (x%is_Edge) then
             select case (x%edge)
             case (1); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_x',pad)
                       call delete(temp_N)
             case (2); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_y',pad)
                       call delete(temp_N)
             case (3); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_z',pad)
                       call delete(temp_N)
             case default; stop 'Error: edge must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
             end select
           else; stop 'Error: bad input to export_processed_SF_func (1) in export_processed.f90'
           endif
         else
           if (x%is_CC) then
             call init_Face(temp_1,m,1); call init_Edge(temp_2,m,3); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call func(m,temp_N,dir,name//'np',pad,direction)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call func(m,x,dir,name//'np',pad,direction)
           elseif (x%is_Face) then
             select case (x%face)
             case (1); call init_Edge(temp_1,m,2); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_x',pad,direction)
                       call delete(temp_1); call delete(temp_N)
             case (2); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_y',pad,direction)
                       call delete(temp_1); call delete(temp_N)
             case (3); call init_Edge(temp_1,m,1); call init_Node(temp_N,m); 
                       call face2Node(temp_N,x,m,x%face,temp_1)
                       call func(m,temp_N,dir,name//'np_z',pad,direction)
                       call delete(temp_1); call delete(temp_N)
             case default; stop 'Error: face must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
             end select
           elseif (x%is_Edge) then
             select case (x%edge)
             case (1); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_x',pad,direction)
                       call delete(temp_N)
             case (2); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_y',pad,direction)
                       call delete(temp_N)
             case (3); call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
                       call func(m,temp_N,dir,name//'ep_z',pad,direction)
                       call delete(temp_N)
             case default; stop 'Error: edge must = 1,2,3 in export_processed_SF in export_raw_processed.f90'
             end select
           else; stop 'Error: bad input to export_processed_SF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       subroutine export_processed_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (.not.export_planar) then; call export_processed_VF_func(export_3D_3C,m,x,dir,name,pad,0)
         else; if (m%plane_x) then;    call export_processed_VF_func(export_2D_2C,m,x,dir,name,pad,1)
         elseif   (m%plane_y) then;    call export_processed_VF_func(export_2D_2C,m,x,dir,name,pad,2)
         elseif   (m%plane_z) then;    call export_processed_VF_func(export_2D_2C,m,x,dir,name,pad,3)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_VF in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_processed_VF_func(func,m,x,dir,name,pad,direction)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         type(VF) :: temp_1,temp_2,temp_N
         if (.not.export_planar) then
           if (x%is_CC) then
             call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call func(m,temp_N,dir,name//'np',pad)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call func(m,temp_N,dir,name//'np',pad)
           elseif (x%is_Face) then
             call init_Edge(temp_1,m); call init_Node(temp_N,m); 
             call face2Node(temp_N,x,m,temp_1)
             call func(m,temp_N,dir,name//'np',pad)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
             call func(m,temp_N,dir,name//'np',pad)
             call delete(temp_N)
           else; stop 'Error: bad input to export_processed_VF_func (1) in export_processed.f90'
           endif
         else
           if (x%is_CC) then
             call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call func(m,temp_N,dir,name//'np',pad,direction)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call func(m,temp_N,dir,name//'np',pad,direction)
           elseif (x%is_Face) then
             call init_Edge(temp_1,m); call init_Node(temp_N,m); 
             call face2Node(temp_N,x,m,temp_1)
             call func(m,temp_N,dir,name//'np',pad,direction)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
             call func(m,temp_N,dir,name//'np',pad,direction)
             call delete(temp_N)
           else; stop 'Error: bad input to export_processed_VF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       subroutine export_processed_transient_VF(m,x,dir,name,pad,nstep)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,nstep
         if (.not.export_planar) then
         stop 'Error: trying to export 3D transient solution in export_processed_transient_VF in export_processed.f90.'
         else; if (m%plane_x) then; call export_processed_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,1,nstep)
         elseif   (m%plane_y) then; call export_processed_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,2,nstep)
         elseif   (m%plane_z) then; call export_processed_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,3,nstep)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_VF in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_processed_transient_VF_func(func,m,x,dir,name,pad,direction,nstep)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: nstep,pad,direction
         type(VF) :: temp_1,temp_2,temp_N
         if (.not.export_planar) then
           stop 'Error: trying to export 3D transient solution in export_processed_transient_VF_func in export_processed.f90.'
         else
           if (x%is_CC) then
             call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call func(m,temp_N,dir,name//'np',pad,direction,nstep)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call func(m,temp_N,dir,name//'np',pad,direction,nstep)
           elseif (x%is_Face) then
             call init_Edge(temp_1,m); call init_Node(temp_N,m); 
             call face2Node(temp_N,x,m,temp_1)
             call func(m,temp_N,dir,name//'np',pad,direction,nstep)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
             call func(m,temp_N,dir,name//'np',pad,direction,nstep)
             call delete(temp_N)
           else; stop 'Error: bad input to export_processed_VF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       end module