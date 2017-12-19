       module segment_extend_mod
       use IO_tools_mod
       use current_precision_mod
       use segment_mod
       use string_mod
       use mesh_quality_params_mod
       implicit none

       private
       public :: seg_1d

       interface seg_1d;   module procedure seg_1d_segment_minmax;      end interface
       interface seg_1d;   module procedure seg_1d_segment_length;      end interface
       interface seg_1d;   module procedure seg_1d_segment_N_cells;     end interface

       contains

       function seg_1d_segment_minmax(dir,distribution,n_cells,hmin,hmax,buffer) result(s)
         implicit none
         character(len=*),intent(in) :: distribution
         integer,intent(in) :: dir,n_cells
         real(cp),intent(in) :: hmin,hmax,buffer
         type(segment) :: s
         s%n_cells = n_cells
         call init(s%distribution,distribution)
         s%hmax = hmax
         s%hmin = hmin
         s%dir = dir
         s%buffer = buffer
       end function

       function seg_1d_segment_length(dir,distribution,n_cells,L,buffer) result(s)
         implicit none
         character(len=*),intent(in) :: distribution
         integer,intent(in) :: dir,n_cells
         real(cp),intent(in) :: L,buffer
         type(segment) :: s
         s%n_cells = n_cells
         call init(s%distribution,distribution)
         s%l = l
         s%dir = dir
         s%buffer = buffer
       end function

       function seg_1d_segment_N_cells(dir,distribution,n_cells) result(s)
         implicit none
         character(len=*),intent(in) :: distribution
         integer,intent(in) :: dir,n_cells
         type(segment) :: s
         s%n_cells = n_cells
         call init(s%distribution,distribution)
         s%dir = dir
         s%buffer = 1.0_cp
       end function

       end module