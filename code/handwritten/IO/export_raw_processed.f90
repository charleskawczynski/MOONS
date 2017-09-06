       module export_raw_processed_mod
       use current_precision_mod
       use mesh_extend_mod
       use string_mod
       use SF_mod
       use VF_mod
       use data_location_mod
       use IO_tools_mod
       use IO_export_mod
       use ops_interp_mod
       use time_marching_params_mod

       implicit none
       private

       public :: export_raw
       interface export_raw;        module procedure er_steady_SF;           end interface
       interface export_raw;        module procedure er_unsteady_SF;         end interface
       interface export_raw;        module procedure er_steady_VF;           end interface
       interface export_raw;        module procedure er_unsteady_VF;         end interface

       public :: export_processed
       interface export_processed;  module procedure ep_3D_1C_steady_SF;     end interface
       interface export_processed;  module procedure ep_3D_1C_unsteady_SF;   end interface
       interface export_processed;  module procedure ep_3D_3C_steady_VF;     end interface
       interface export_processed;  module procedure ep_3D_3C_unsteady_VF;   end interface

       interface export_processed;  module procedure ep_2D_3C_steady_VF;     end interface
       interface export_processed;  module procedure ep_2D_3C_unsteady_VF;   end interface
       interface export_processed;  module procedure ep_2D_1C_steady_SF;     end interface
       interface export_processed;  module procedure ep_2D_1C_unsteady_SF;   end interface

       interface export_processed;  module procedure ep_1D_1C_steady_SF;     end interface
       interface export_processed;  module procedure ep_1D_1C_unsteady_SF;   end interface
       interface export_processed;  module procedure ep_1D_3C_steady_VF;     end interface
       interface export_processed;  module procedure ep_1D_3C_unsteady_VF;   end interface

       contains

       ! **********************************************************************
       ! ***************************** EXPORT RAW *****************************
       ! **********************************************************************

       subroutine er_steady_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         call export_3D_1C(m,x,dir,name,pad)
       end subroutine
       subroutine er_unsteady_SF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         call export_3D_1C(m,x,dir,name,pad,TMP)
       end subroutine

       subroutine er_steady_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         if (is_collocated(x)) then; call export_3D_3C(m,x,dir,name,pad)
         else;                       call export_3D_1C(m,x%x,dir,name,pad)
                                     call export_3D_1C(m,x%y,dir,name,pad)
                                     call export_3D_1C(m,x%z,dir,name,pad)
         endif
       end subroutine
       subroutine er_unsteady_VF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         if (is_collocated(x)) then; call export_3D_3C(m,x,dir,name,pad,TMP)
         else;                       call export_3D_1C(m,x%x,dir,name,pad,TMP)
                                     call export_3D_1C(m,x%y,dir,name,pad,TMP)
                                     call export_3D_1C(m,x%z,dir,name,pad,TMP)
         endif
       end subroutine

       ! **********************************************************************
       ! ************************* EXPORT 3D PROCESSED ************************
       ! **********************************************************************

       subroutine ep_3D_1C_steady_SF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_3D_1C(m,temp_N,dir,name//'p',pad)
         call delete(temp_N)
       end subroutine
       subroutine ep_3D_1C_unsteady_SF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_3D_1C(m,temp_N,dir,name//'p',pad,TMP)
         call delete(temp_N)
       end subroutine

       subroutine ep_3D_3C_steady_VF(m,x,dir,name,pad)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_3D_3C(m,temp_N,dir,name//'p',pad)
         call delete(temp_N)
       end subroutine
       subroutine ep_3D_3C_unsteady_VF(m,x,dir,name,pad,TMP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(time_marching_params),intent(in) :: TMP
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_3D_3C(m,temp_N,dir,name//'p',pad,TMP)
         call delete(temp_N)
       end subroutine


       ! **********************************************************************
       ! ************************* EXPORT 2D PROCESSED ************************
       ! **********************************************************************

       subroutine ep_2D_3C_steady_VF(m,x,dir,name,pad,direction,plane)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction,plane
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_2D_3C(m,temp_N,dir,name//'p',pad,direction,plane)
         call delete(temp_N)
       end subroutine
       subroutine ep_2D_3C_unsteady_VF(m,x,dir,name,pad,TMP,direction,plane)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction,plane
         type(time_marching_params),intent(in) :: TMP
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_2D_3C(m,temp_N,dir,name//'p',pad,TMP,direction,plane)
         call delete(temp_N)
       end subroutine

       subroutine ep_2D_1C_steady_SF(m,x,dir,name,pad,direction,plane)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction,plane
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_2D_1C(m,temp_N,dir,name//'p',pad,direction,plane)
         call delete(temp_N)
       end subroutine
       subroutine ep_2D_1C_unsteady_SF(m,x,dir,name,pad,TMP,direction,plane)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction,plane
         type(time_marching_params),intent(in) :: TMP
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_2D_1C(m,temp_N,dir,name//'p',pad,TMP,direction,plane)
         call delete(temp_N)
       end subroutine


       ! **********************************************************************
       ! ************************* EXPORT 1D PROCESSED ************************
       ! **********************************************************************

       subroutine ep_1D_1C_steady_SF(m,x,dir,name,pad,direction,line)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         integer,dimension(2),intent(in) :: line
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_1D_1C(m,temp_N,dir,name//'p',pad,direction,line)
         call delete(temp_N)
       end subroutine
       subroutine ep_1D_1C_unsteady_SF(m,x,dir,name,pad,TMP,direction,line)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         integer,dimension(2),intent(in) :: line
         type(time_marching_params),intent(in) :: TMP
         type(SF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_1D_1C(m,temp_N,dir,name//'p',pad,TMP,direction,line)
         call delete(temp_N)
       end subroutine

       subroutine ep_1D_3C_steady_VF(m,x,dir,name,pad,direction,line)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         integer,dimension(2),intent(in) :: line
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_1D_3C(m,temp_N,dir,name//'p',pad,direction,line)
         call delete(temp_N)
       end subroutine
       subroutine ep_1D_3C_unsteady_VF(m,x,dir,name,pad,TMP,direction,line)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,direction
         integer,dimension(2),intent(in) :: line
         type(time_marching_params),intent(in) :: TMP
         type(VF) :: temp_1,temp_2,temp_N
         call any_to_node(m,temp_N,x,temp_1,temp_2)
         call export_1D_3C(m,temp_N,dir,name//'p',pad,TMP,direction,line)
         call delete(temp_N)
       end subroutine

       end module