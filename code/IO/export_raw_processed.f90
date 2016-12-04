       module export_raw_processed_mod
       use current_precision_mod
       use mesh_mod
       use string_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use data_location_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
       use ops_interp_mod
       use time_marching_params_mod

       implicit none

       private
       public :: export_raw                    ! call export_raw(m,x,dir,name,pad)
       public :: export_processed              ! call export_processed(m,x,dir,name,pad)
       public :: export_processed_transient_2C ! call export_processed_transient(m,x,dir,name,pad,TMP)
       public :: export_processed_transient_3C

       logical :: export_planar = .false.      ! Export 2D data when N_cell = 1 along given direction

       interface export_raw;                    module procedure export_raw_SF;                     end interface
       interface export_raw;                    module procedure export_raw_VF;                     end interface
       interface export_processed;              module procedure export_processed_SF;               end interface
       interface export_processed;              module procedure export_processed_VF;               end interface

       interface export_processed;              module procedure export_processed_transient_SF;     end interface
       interface export_processed;              module procedure export_processed_transient_VF;     end interface

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
         type(string) :: s
         call construct_filename(s,name,x%DL)
         if (.not.export_planar) then; call export_3D_1C(m,x,dir,str(s),pad)
         else; direction = get_plane_direction(m,'export_raw_SF')
               call export_2D_1C(m,x,dir,str(s),pad,direction)
         endif
         call delete(s)
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
         DL = get_char(x%x%DL)

         tol = 10.0_cp**(-15.0_cp)
         t = (/amax(x%x),amax(x%y),amax(x%z)/)
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

       subroutine export_processed_transient_SF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         type(SF) :: temp_1,temp_2,temp_N
         integer :: direction
         character(len=2) :: DL,CD ! DL = direction letter, CD = component direction
         integer,dimension(3) :: i_f
         type(string) :: s
         call construct_filename(s,name,x%DL)
         call location_to_node_SF(m,temp_N,x,temp_1,temp_2,x%DL)
         if (.not.export_planar) then
         call export_3D_1C_transient(m,temp_N,dir,str(s),pad,TMP)
         else; stop 'Error: feature 1 not yet supported in export_processed.f90'
         endif
         call delete(temp_N)
       end subroutine

       subroutine export_processed_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(SF) :: temp_1,temp_2,temp_N
         integer :: direction
         type(string) :: s
         call construct_filename(s,name,x%DL)
         call location_to_node_SF(m,temp_N,x,temp_1,temp_2,x%DL)
         if (.not.export_planar) then
           call export_3D_1C(m,temp_N,dir,str(s),pad)
         else; direction = get_plane_direction(m,'export_processed_SF')
           call export_2D_1C(m,temp_N,dir,str(s),pad,direction)
         endif
         call delete(temp_N)
         call delete(s)
       end subroutine

       subroutine export_processed_transient_VF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         type(VF) :: temp_1,temp_2,temp_N
         integer :: direction
         character(len=2) :: DL ! DL = direction letter, CD = component direction
         real(cp),dimension(3) :: t
         logical,dimension(3) :: L
         real(cp) :: tol
         integer :: j
         DL = 'np' ! "p" means "processed" (interpolated to node)
         tol = 10.0_cp**(-15.0_cp)
         t = (/amax(x%x),amax(x%y),amax(x%z)/)
         L = (/(t(j).lt.tol,j=1,3)/)
         if (.not.export_planar) then
           call location_to_node_VF(m,temp_N,x,temp_1,temp_2,x%x%DL)
           call export_3D_3C_transient(m,temp_N,dir,name//DL,pad,TMP)
           call delete(temp_N)
         else; stop 'Error: feature 2 not yet supported in export_processed.f90'
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
         t = (/amax(x%x),amax(x%y),amax(x%z)/)
         L = (/(t(j).lt.tol,j=1,3)/)

         call location_to_node_VF(m,temp_N,x,temp_1,temp_2,x%x%DL)
         if (.not.export_planar) then
           call export_3D_3C(m,temp_N,dir,name//DL,pad)
         else
           direction = get_plane_direction(m,'export_processed_VF')
           call export_2D_based_on_count(m,temp_N,dir,name,DL,pad,direction,L)
         endif
         call delete(temp_N)
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
                 endif
         else              ! export all components
           if (x%is_CC.or.x%is_Node) then; call export_2D_3C(m,x  ,dir,name//DL,pad,direction)
           else;                           call export_2D_1C(m,x%x,dir,name//DL//'_x',pad,direction)
                                           call export_2D_1C(m,x%y,dir,name//DL//'_y',pad,direction)
                                           call export_2D_1C(m,x%z,dir,name//DL//'_z',pad,direction)
           endif
         endif

         if ((flag.eq.1).or.(flag.eq.2)) then
           if (x%is_CC.or.x%is_Node) then; call export_3D_3C(m,x  ,dir,name//DL,pad)
           else;                           call export_3D_1C(m,x%x,dir,name//DL//'_x_warning',pad)
                                           call export_3D_1C(m,x%y,dir,name//DL//'_y_warning',pad)
                                           call export_3D_1C(m,x%z,dir,name//DL//'_z_warning',pad)
           endif
           write(*,*) 'flag=',flag
           write(*,*) 'A=',A
           write(*,*) 'C=',C
           write(*,*) 'WARNING: bad case in export_based_on_count in export_raw_processed.f90'
         endif
       end subroutine

       subroutine export_processed_transient_VF_2C(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         if (.not.export_planar) then
         stop 'Error: trying to export 3D transient solution in export_processed_transient_VF_2C in export_processed.f90.'
         else; if (m%plane_x) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,1,TMP)
         elseif   (m%plane_y) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,2,TMP)
         elseif   (m%plane_z) then; call export_p_transient_VF_func(export_2D_2C_transient,m,x,dir,name,pad,3,TMP)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_transient_VF_2C in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_processed_transient_VF_3C(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         if (.not.export_planar) then
         stop 'Error: trying to export 3D transient solution in export_processed_transient_VF_3C in export_processed.f90.'
         else; if (m%plane_x) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,1,TMP)
         elseif   (m%plane_y) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,2,TMP)
         elseif   (m%plane_z) then; call export_p_transient_VF_func(export_2D_3C_transient,m,x,dir,name,pad,3,TMP)
         else; stop 'Error: attempted plane export of 3D geometry in export_processed_transient_VF_3C in export_raw_processed.f90'
         endif
         endif
       end subroutine

       subroutine export_p_transient_VF_func(func,m,x,dir,name,pad,direction,TMP)
         implicit none
         procedure(export_unsteady) :: func
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         type(time_marching_params),intent(in) :: TMP
         type(VF) :: temp_1,temp_2,temp_N
         if (.not.export_planar) then
           stop 'Error: trying to export 3D transient solution in export_p_transient_VF_func in export_processed.f90.'
         else
           call location_to_node_VF(m,temp_N,x,temp_1,temp_2,x%x%DL)
           call func(m,temp_N,dir,name//'np',pad,direction,TMP)
           call delete(temp_N)
         endif
       end subroutine

       subroutine location_to_node_SF(m,N,x,E,F,DL)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         type(SF),intent(inout) :: N,E,F
         type(data_location),intent(in) :: DL
         integer,dimension(3) :: i
         integer :: i_f,i_e
         if (is_CC(DL)) then
           call init_Face(F,m,1)
           call init_Edge(E,m,3)
           call init_Node(N,m)
           call cellcenter2Node(N,x,m,F,E)
           call delete(F)
           call delete(E)
         elseif (is_Node(DL)) then
         call init_Node(N,m)
         call assign(N,x)
         elseif (is_Face(DL)) then
           i = (/2,1,1/)
           i_f = get_Face(DL)
           call init_Edge(E,m,i(i_f))
           call init_Node(N,m)
           call face2Node(N,x,m,i_f,E)
           call delete(E)
         elseif (is_Edge(DL)) then
           i_e = get_Edge(DL)
           call init_Node(N,m)
           call edge2Node(N,x,m,i_e)
         else; stop 'Error: bad input to location_to_node in export_processed.f90'
         endif
       end subroutine

       subroutine location_to_node_VF(m,N,x,E,F,DL)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         type(VF),intent(inout) :: N,E,F
         type(data_location),intent(in) :: DL
         if (is_CC(DL)) then
           call init_Face(F,m)
           call init_Edge(E,m)
           call init_Node(N,m)
           call cellcenter2Node(N,x,m,F,E)
           call delete(F)
           call delete(E)
         elseif (is_Node(DL)) then
         call init_Node(N,m)
         call assign(N,x)
         elseif (is_Face(DL)) then
           call init_Edge(E,m)
           call init_Node(N,m)
           call face2Node(N,x,m,E)
           call delete(E)
         elseif (is_Edge(DL)) then
           call init_Node(N,m)
           call edge2Node(N,x,m)
         else; stop 'Error: bad input to location_to_node in export_processed.f90'
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
         select case (i)
         case (1); CD = 'x'
         case (2); CD = 'y'
         case (3); CD = 'z'
         case default
         write(*,*) 'Error: i must = 1,2,3 in '//caller//' in export_raw_processed.f90'
         stop 'Done'
         end select
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

       subroutine construct_filename(s,name,DL)
         implicit none
         type(string),intent(inout) :: s
         type(data_location),intent(in) :: DL
         character(len=*),intent(in) :: name
         call delete(s)
         call init(s,name//get_char(DL))
             if (is_Face(DL)) then; call append(s,'_'//get_CD(DL%face,'construct_filename'))
         elseif (is_Edge(DL)) then; call append(s,'_'//get_CD(DL%edge,'construct_filename'))
         endif
       end subroutine

       subroutine construct_filename_processed(s,name)
         implicit none
         type(string),intent(inout) :: s
         character(len=*),intent(in) :: name
         call delete(s)
         call init(s,name//'np')
       end subroutine

       end module