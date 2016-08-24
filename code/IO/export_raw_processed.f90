       module export_raw_processed_mod
       use current_precision_mod
       use simParams_mod
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
       public :: export_raw                    ! call export_raw(m,x,dir,name,pad)
       public :: export_processed              ! call export_processed(m,x,dir,name,pad)
       public :: export_processed_transient_2C ! call export_processed_transient(m,x,dir,name,pad,nstep)
       public :: export_processed_transient_3C

       interface export_raw;                    module procedure export_raw_SF;                     end interface
       interface export_raw;                    module procedure export_raw_VF;                     end interface
       interface export_processed;              module procedure export_processed_SF;               end interface
       interface export_processed;              module procedure export_processed_VF;               end interface

       interface export_processed_transient_2C; module procedure export_processed_transient_VF_2C;  end interface
       interface export_processed_transient_3C; module procedure export_processed_transient_VF_3C;  end interface

       contains

       ! ************************************************************************************
       ! ************************************* RAW ******************************************
       ! ************************************************************************************

       subroutine export_raw_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         integer :: direction
         character(len=1) :: DL,CD ! DL = direction letter, CD = component direction
         ! ------------- collect some info
         DL = get_DL_SF(x,'export_raw_SF0')
         if (x%is_Face) CD = get_CD(x%face,'export_raw_SF1')
         if (x%is_Edge) CD = get_CD(x%edge,'export_raw_SF2')
         ! ------------- begin export
         if (.not.export_planar) then
           if (x%is_CC.or.x%is_Node) then; call export_3D_1C(m,x,dir,name//DL,pad)
           else;                           call export_3D_1C(m,x,dir,name//DL//'_'//CD,pad)
           endif
         else
           direction = get_plane_direction(m,'export_raw_SF3')
           if (x%is_CC.or.x%is_Node) then; call export_2D_1C(m,x,dir,name//DL,pad,direction)
           else;                           call export_2D_1C(m,x,dir,name//DL//'_'//CD,pad,direction)
           endif
         endif
       end subroutine

       subroutine export_raw_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         integer :: direction
         real(cp),dimension(3) :: t
         logical,dimension(3) :: L
         real(cp) :: tol
         integer :: j
         character(len=1) :: DL ! DL = direction letter, CD = component direction
         DL = get_DL(x,'export_raw_VF')

         tol = 10.0_cp**(-15.0_cp)
         t = (/maxabs(x%x),maxabs(x%y),maxabs(x%z)/)
         L = (/(t(j).lt.tol,j=1,3)/)

         if (.not.export_planar) then ! export all components
           if (x%is_CC.or.x%is_Node) then; call export_3D_3C(m,x,dir,name//DL,pad)
           else;                           call export_3D_1C(m,x%x,dir,name//DL//'_x',pad)
                                           call export_3D_1C(m,x%y,dir,name//DL//'_y',pad)
                                           call export_3D_1C(m,x%z,dir,name//DL//'_z',pad)
           endif
         else; direction = get_plane_direction(m,'export_processed_SF3')
         call export_2D_based_on_count(m,x,dir,name,DL,pad,direction,L)
         endif
       end subroutine

       ! ************************************************************************************
       ! *********************************** PROCESSED **************************************
       ! ************************************************************************************

       subroutine export_processed_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(SF) :: temp_1,temp_2,temp_N
         integer :: direction
         character(len=2) :: DL,CD ! DL = direction letter, CD = component direction
         integer,dimension(3) :: i_f
         DL = 'np'
         if (x%is_Face) CD = get_CD(x%face,'export_processed_SF1')
         if (x%is_Edge) CD = get_CD(x%edge,'export_processed_SF2')

         if (.not.export_planar) then
           if (x%is_CC) then
             call init_Face(temp_1,m,1); call init_Edge(temp_2,m,3); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call export_3D_1C(m,temp_N,dir,name//DL,pad)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then; call export_3D_1C(m,x,dir,name//DL,pad)
           elseif (x%is_Face) then
             call init_Node(temp_N,m); i_f = (/2,1,1/)
             call init_Edge(temp_1,m,i_f(x%face))
             call face2Node(temp_N,x,m,x%face,temp_1)
             call export_3D_1C(m,temp_N,dir,name//DL//'_'//CD,pad)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
             call export_3D_1C(m,temp_N,dir,name//DL//'_'//CD,pad)
             call delete(temp_N)
           else; stop 'Error: bad input to export_p_SF_func (1) in export_processed.f90'
           endif
         else; direction = get_plane_direction(m,'export_processed_SF3')
           if (x%is_CC) then
             call init_Face(temp_1,m,1); call init_Edge(temp_2,m,3); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call export_2D_1C(m,temp_N,dir,name//'np',pad,direction)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call export_2D_1C(m,x,dir,name//'np',pad,direction)
           elseif (x%is_Face) then
             call init_Node(temp_N,m); i_f = (/2,1,1/)
             call init_Edge(temp_1,m,i_f(x%face))
             call face2Node(temp_N,x,m,x%face,temp_1)
             call export_2D_1C(m,temp_N,dir,name//DL//'_'//CD,pad,direction)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m,x%edge)
             call export_2D_1C(m,temp_N,dir,name//DL//'_'//CD,pad,direction)
             call delete(temp_N)
           else; stop 'Error: bad input to export_p_SF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       subroutine export_processed_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(VF) :: temp_1,temp_2,temp_N
         integer :: direction
         character(len=2) :: DL ! DL = direction letter, CD = component direction
         real(cp),dimension(3) :: t
         logical,dimension(3) :: L
         real(cp) :: tol
         integer :: j
         DL = 'np' ! "p" means "processed" (interpolated to node)

         tol = 10.0_cp**(-15.0_cp)
         t = (/maxabs(x%x),maxabs(x%y),maxabs(x%z)/)
         L = (/(t(j).lt.tol,j=1,3)/)

         if (.not.export_planar) then
           if (x%is_CC) then
             call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call export_3D_3C(m,temp_N,dir,name//DL,pad)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call export_3D_3C(m,temp_N,dir,name//DL,pad)
           elseif (x%is_Face) then
             call init_Edge(temp_1,m); call init_Node(temp_N,m); 
             call face2Node(temp_N,x,m,temp_1)
             call export_3D_3C(m,temp_N,dir,name//DL,pad)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
             call export_3D_3C(m,temp_N,dir,name//DL,pad)
             call delete(temp_N)
           else; stop 'Error: bad input to export_processed_VF (1) in export_processed.f90'
           endif
         else; direction = get_plane_direction(m,'export_processed_VF')
           if (x%is_CC) then
             call init_Face(temp_1,m); call init_Edge(temp_2,m); call init_Node(temp_N,m)
             call cellcenter2Node(temp_N,x,m,temp_1,temp_2)
             call export_2D_based_on_count(m,temp_N,dir,name,DL,pad,direction,L)
             call delete(temp_1); call delete(temp_2); call delete(temp_N)
           elseif (x%is_Node) then
             call export_2D_based_on_count(m,temp_N,dir,name,DL,pad,direction,L)
           elseif (x%is_Face) then
             call init_Edge(temp_1,m); call init_Node(temp_N,m); 
             call face2Node(temp_N,x,m,temp_1)
             call export_2D_based_on_count(m,temp_N,dir,name,DL,pad,direction,L)
             call delete(temp_1); call delete(temp_N)
           elseif (x%is_Edge) then
             call init_Node(temp_N,m); call edge2Node(temp_N,x,m)
             call export_2D_based_on_count(m,temp_N,dir,name,DL,pad,direction,L)
             call delete(temp_N)
           else; stop 'Error: bad input to export_processed_VF (2) in export_processed.f90'
           endif
         endif
       end subroutine

       subroutine export_2D_based_on_count(m,x,dir,name,DL,pad,direction,L)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name,DL
         integer,intent(in) :: pad,direction
         logical,dimension(3),intent(in) :: L ! logical array of zero components (true means max(x)<tol)
         logical,dimension(3) :: C,A
         integer :: flag
         A = (/direction.eq.1,direction.eq.2,direction.eq.3/)
         C = (/.not.L(1),.not.L(2),.not.L(3)/)
         flag = 0

             if (count(L).eq.2) then ! try to export 1 component, then 2 components
                     if (A(1).and.C(1)) then; call export_2D_1C(m,x%x,dir,name//DL,pad,1)
                 elseif (A(2).and.C(2)) then; call export_2D_1C(m,x%y,dir,name//DL,pad,2)
                 elseif (A(3).and.C(3)) then; call export_2D_1C(m,x%z,dir,name//DL,pad,3)
                 elseif (A(1)) then; call export_2D_2C(m,x,dir,name//DL,pad,1)
                 elseif (A(2)) then; call export_2D_2C(m,x,dir,name//DL,pad,2)
                 elseif (A(3)) then; call export_2D_2C(m,x,dir,name//DL,pad,3)
                 else; flag = 1
                 endif
         elseif (count(L).eq.1) then ! export 2 components
                     if (A(1).and.(.not.C(1))) then; call export_2D_2C(m,x,dir,name//DL,pad,1)
                 elseif (A(2).and.(.not.C(2))) then; call export_2D_2C(m,x,dir,name//DL,pad,2)
                 elseif (A(3).and.(.not.C(3))) then; call export_2D_2C(m,x,dir,name//DL,pad,3)
                 else; flag = 2
                         if (A(1)) then; call export_2D_2C(m,x,dir,name//DL,pad,1)
                     elseif (A(2)) then; call export_2D_2C(m,x,dir,name//DL,pad,2)
                     elseif (A(3)) then; call export_2D_2C(m,x,dir,name//DL,pad,3)
                     endif
                 endif
         else                        ! export all components
                 if (x%is_CC.or.x%is_Node) then; call export_2D_3C(m,x  ,dir,name//DL,pad,direction)
                 else;                           call export_2D_1C(m,x%x,dir,name//DL//'_x',pad,direction)
                                                 call export_2D_1C(m,x%y,dir,name//DL//'_y',pad,direction)
                                                 call export_2D_1C(m,x%z,dir,name//DL//'_z',pad,direction)
                 endif
         endif
         if (flag.eq.0) then
         elseif (flag.eq.1) then
         write(*,*) 'A=',A; write(*,*) 'C=',C
         stop 'Error: bad case in export_based_on_count1 in export_raw_processed.f90'
         elseif (flag.eq.2) then
         write(*,*) 'A=',A; write(*,*) 'C=',C
         stop 'Error: bad case in export_based_on_count2 in export_raw_processed.f90'
         endif
       end subroutine

       subroutine export_processed_transient_VF_2C(m,x,dir,name,pad,nstep)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,nstep
         if (.not.export_planar) then
         stop 'Error: trying to export 3D transient solution in export_processed_transient_VF_2C in export_processed.f90.'
         else; if (m%plane_x) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,1,nstep)
         elseif   (m%plane_y) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,2,nstep)
         elseif   (m%plane_z) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,3,nstep)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_transient_VF_2C in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_processed_transient_VF_3C(m,x,dir,name,pad,nstep)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,nstep
         if (.not.export_planar) then
         stop 'Error: trying to export 3D transient solution in export_processed_transient_VF_3C in export_processed.f90.'
         else; if (m%plane_x) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,1,nstep)
         elseif   (m%plane_y) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,2,nstep)
         elseif   (m%plane_z) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,3,nstep)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_transient_VF_3C in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_p_transient_VF_func(func,m,x,dir,name,pad,direction,nstep)
         implicit none
         external :: func
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: nstep,pad,direction
         type(VF) :: temp_1,temp_2,temp_N
         if (.not.export_planar) then
           stop 'Error: trying to export 3D transient solution in export_p_transient_VF_func in export_processed.f90.'
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
           else; stop 'Error: bad input to export_p_transient_VF_func (2) in export_processed.f90'
           endif
         endif
       end subroutine

       ! *************************************************************************************
       ! ******************************* FUNCTIONS *******************************************
       ! *************************************************************************************

       function get_CD(i,caller) result(CD)
         ! Returns ('x','y','z') for inputs (1,2,3)
         implicit none
         integer,intent(in) :: i
         character(len=*),intent(in) :: caller
         character(len=1) :: CD
         select case (i); case (1); CD = 'x'; case (2); CD = 'y'; case (3); CD = 'z'
         case default; write(*,*) 'Error: i must = 1,2,3 in '//caller
         stop 'Done in export_raw_processed.f90'
         end select
       end function

       function get_DL(x,caller) result(DL)
         implicit none
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: caller
         character(len=1) :: DL
               if (x%is_CC) then; DL = 'c'; elseif (x%is_Node) then; DL = 'n'
         elseif (x%is_Face) then; DL = 'f'; elseif (x%is_Edge) then; DL = 'e'
         else; write(*,*) 'Error: bad input type in '//caller; stop 'Done in export_raw_processed.f90'
         endif
       end function

       function get_DL_SF(x,caller) result(DL)
         implicit none
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: caller
         character(len=1) :: DL
               if (x%is_CC) then; DL = 'c'; elseif (x%is_Node) then; DL = 'n'
         elseif (x%is_Face) then; DL = 'f'; elseif (x%is_Edge) then; DL = 'e'
         else; write(*,*) 'Error: bad input type in '//caller; stop 'Done in export_raw_processed.f90'
         endif
       end function

       function get_plane_direction(m,caller) result(direction)
         implicit none
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: caller
         integer :: direction
             if (m%plane_x) then; direction = 1
         elseif (m%plane_y) then; direction = 2
         elseif (m%plane_z) then; direction = 3
         else; write(*,*) 'Error: attempted plane export of 3D geometry in '//caller
         stop 'Done in export_raw_processed.f90'
         endif
       end function

       end module