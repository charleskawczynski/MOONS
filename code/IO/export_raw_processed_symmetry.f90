       module export_raw_processed_symmetry_mod
       use current_precision_mod
       use mesh_mod
       use export_raw_processed_mod
       use face_edge_corner_indexing_mod
       use string_mod
       use string_aux_mod
       use SF_mod
       use VF_mod
       use datatype_conversion_mod
       use IO_export_mod

       implicit none

       public :: export_raw
       public :: export_processed

       private
       interface export_raw;           module procedure export_raw_symmetry_SF;            end interface
       interface export_raw;           module procedure export_raw_symmetry_VF;            end interface
       interface export_processed;     module procedure export_processed_symmetry_SF;      end interface
       interface export_processed;     module procedure export_processed_symmetry_VF;      end interface

       contains

       ! ************************************* RAW ******************************************

       subroutine export_raw_symmetry_SF(m,x,dir,name,pad,face,mirror_sign)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,face
         real(cp),intent(in) :: mirror_sign
         integer :: d
         type(mesh) :: m_temp
         type(SF) :: x_temp
         type(string) :: s
         call init(m_temp,m)
         call init(x_temp,x)
         call assign(x_temp,x)
         d = dir_given_face(face)
         if (min_face(face)) then
          call mirror_about_hmin(m_temp,d)
          call mirror_about_hmin(x_temp,d,mirror_sign)
         endif
         if (max_face(face)) then
          call mirror_about_hmax(m_temp,d)
          call mirror_about_hmax(x_temp,d,mirror_sign)
         endif
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_raw(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       subroutine export_raw_symmetry_VF(m,x,dir,name,pad,face,mirror_sign)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,face
         real(cp),dimension(3),intent(in) :: mirror_sign
         integer :: d
         type(mesh) :: m_temp
         type(VF) :: x_temp
         type(string) :: s
         call init(m_temp,m)
         call init(x_temp,x)
         call assign(x_temp,x)
         d = dir_given_face(face)
         if (min_face(face)) then
          call mirror_about_hmin(m_temp,d)
          call mirror_about_hmin(x_temp%x,d,mirror_sign(1))
          call mirror_about_hmin(x_temp%y,d,mirror_sign(2))
          call mirror_about_hmin(x_temp%z,d,mirror_sign(3))
         endif
         if (max_face(face)) then
          call mirror_about_hmax(m_temp,d)
          call mirror_about_hmax(x_temp%x,d,mirror_sign(1))
          call mirror_about_hmax(x_temp%y,d,mirror_sign(2))
          call mirror_about_hmax(x_temp%z,d,mirror_sign(3))
         endif
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_raw(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       ! *********************************** PROCESSED **************************************

       subroutine export_processed_symmetry_SF(m,x,dir,name,pad,face,mirror_sign)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,face
         real(cp),intent(in) :: mirror_sign
         integer :: d
         type(mesh) :: m_temp
         type(SF) :: x_temp
         type(string) :: s
         call init(m_temp,m)
         call init(x_temp,x)
         call assign(x_temp,x)
         d = dir_given_face(face)
         if (min_face(face)) then
          call mirror_about_hmin(m_temp,d)
          call mirror_about_hmin(x_temp,d,mirror_sign)
         endif
         if (max_face(face)) then
          call mirror_about_hmax(m_temp,d)
          call mirror_about_hmax(x_temp,d,mirror_sign)
         endif
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_processed(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       subroutine export_processed_symmetry_VF(m,x,dir,name,pad,face,mirror_sign)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad,face
         real(cp),dimension(3),intent(in) :: mirror_sign
         integer :: d
         type(mesh) :: m_temp
         type(VF) :: x_temp
         type(string) :: s
         call init(m_temp,m)
         call init(x_temp,x)
         call assign(x_temp,x)
         d = dir_given_face(face)
         if (min_face(face)) then
           call mirror_about_hmin(m_temp,d)
           call mirror_about_hmin(x_temp%x,d,mirror_sign(1))
           call mirror_about_hmin(x_temp%y,d,mirror_sign(2))
           call mirror_about_hmin(x_temp%z,d,mirror_sign(3))
         endif
         if (max_face(face)) then
          call mirror_about_hmax(m_temp,d)
          call mirror_about_hmax(x_temp%x,d,mirror_sign(1))
          call mirror_about_hmax(x_temp%y,d,mirror_sign(2))
          call mirror_about_hmax(x_temp%z,d,mirror_sign(3))
         endif
         call init(s,int2str(d))
         call remove_leading_zeros(s)
         call export_processed(m_temp,x_temp,dir,name//'_mirror_'//str(s),pad)
         call delete(s)
         call delete(m_temp)
         call delete(x_temp)
       end subroutine

       end module