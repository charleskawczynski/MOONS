       module segment_extend_mod
       use IO_tools_mod
       use segment_mod
       use string_mod
       use mesh_quality_params_mod
       implicit none

       integer,parameter :: li = selected_int_kind(16)
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif
       private
       public :: seg_1d

       interface seg_1d;   module procedure seg_1d_segment_minmax;      end interface
       interface seg_1d;   module procedure seg_1d_segment_length;      end interface

       contains

       function seg_1d_segment_minmax(dir,distribution,n_cells,hmin,hmax) result(s)
         implicit none
         type(string),intent(in) :: distribution
         integer,intent(in) :: dir,n_cells
         real(cp),intent(in) :: hmin,hmax
         type(segment) :: s
         s%n_cells = n_cells
         call init(s%distribution,distribution)
         s%hmax = hmax
         s%hmin = hmin
         s%dir = dir
       end function

       function seg_1d_segment_length(dir,distribution,n_cells,L) result(s)
         implicit none
         type(string),intent(in) :: distribution
         integer,intent(in) :: dir,n_cells
         real(cp),intent(in) :: L
         type(segment) :: s
         s%n_cells = n_cells
         call init(s%distribution,distribution)
         s%l = l
         s%dir = dir
       end function

       end module