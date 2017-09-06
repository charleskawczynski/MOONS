       module export_raw_processed_symmetry_mod
       use current_precision_mod
       use mesh_extend_mod
       use export_raw_processed_mod
       use face_edge_corner_indexing_mod
       use string_mod
       use string_aux_mod
       use SF_extend_mod
       use VF_mod
       use datatype_conversion_mod
       use IO_export_mod
       use ops_mirror_field_mod
       use mirror_props_mod

       implicit none

       public :: export_raw
       public :: export_processed

       private
       interface export_raw;       module procedure export_raw_symmetry_SF;       end interface
       interface export_raw;       module procedure export_raw_symmetry_VF;       end interface
       interface export_processed; module procedure export_processed_symmetry_SF; end interface
       interface export_processed; module procedure export_processed_symmetry_VF; end interface

       contains

       ! ************************************* RAW ******************************************

       subroutine export_raw_symmetry_SF(m,x,dir,name,pad,MP)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(mirror_props),intent(in) :: MP
         integer :: d
         type(mesh) :: m_temp
         type(SF) :: x_temp
         type(string) :: s
         d = dir_given_face(MP%mirror_face)
         call mirror_field(m_temp,x_temp,m,x,MP)
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_raw(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       subroutine export_raw_symmetry_VF(m,x,dir,name,pad,MP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(mirror_props),intent(in) :: MP
         integer :: d
         type(mesh) :: m_temp
         type(VF) :: x_temp
         type(string) :: s
         d = dir_given_face(MP%mirror_face)
         call mirror_field(m_temp,x_temp,m,x,MP)
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_raw(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       ! *********************************** PROCESSED **************************************

       subroutine export_processed_symmetry_SF(m,x,dir,name,pad,MP)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(mirror_props),intent(in) :: MP
         integer :: d
         type(mesh) :: m_temp
         type(SF) :: x_temp
         type(string) :: s
         d = dir_given_face(MP%mirror_face)
         call mirror_field(m_temp,x_temp,m,x,MP)
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_processed(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       subroutine export_processed_symmetry_VF(m,x,dir,name,pad,MP)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         type(mirror_props),intent(in) :: MP
         integer :: d
         type(mesh) :: m_temp
         type(VF) :: x_temp
         type(string) :: s
         d = dir_given_face(MP%mirror_face)
         call mirror_field(m_temp,x_temp,m,x,MP)
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_processed(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       end module