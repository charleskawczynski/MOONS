       module ops_mirror_field_mod
       use current_precision_mod
       use mesh_mod
       use face_edge_corner_indexing_mod
       use SF_mod
       use VF_mod

       implicit none
       private
       public :: mirror_field
       interface mirror_field;  module procedure mirror_field_SF;    end interface
       interface mirror_field;  module procedure mirror_field_VF;    end interface

       public :: mirror_mesh
       interface mirror_mesh;  module procedure mirror_mesh_MF;  end interface

       contains

       subroutine mirror_mesh_MF(m_mirror,m,face)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(mesh),intent(in) :: m
         integer,intent(in) :: face
         integer :: d
         call init(m_mirror,m)
         d = dir_given_face(face)
         if (min_face(face)) call mirror_about_hmin(m_mirror,d)
         if (max_face(face)) call mirror_about_hmax(m_mirror,d)
         call init_props(m_mirror)
         call patch(m_mirror)
       end subroutine

       subroutine mirror_field_SF(m_mirror,x_mirror,m,x,face,mirror_sign)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(SF),intent(inout) :: x_mirror
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         integer,intent(in) :: face
         real(cp),intent(in) :: mirror_sign
         integer :: d
         call mirror_mesh(m_mirror,m,face)
         call init(x_mirror,x)
         call assign(x_mirror,x)
         d = dir_given_face(face)
         if (min_face(face)) call mirror_about_hmin(x_mirror,d,mirror_sign)
         if (max_face(face)) call mirror_about_hmax(x_mirror,d,mirror_sign)
       end subroutine

       subroutine mirror_field_VF(m_mirror,x_mirror,m,x,face,mirror_sign)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(VF),intent(inout) :: x_mirror
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         integer,intent(in) :: face
         real(cp),dimension(3),intent(in) :: mirror_sign
         integer :: d
         call mirror_mesh(m_mirror,m,face)
         call init(x_mirror,x)
         call assign(x_mirror,x)
         d = dir_given_face(face)
         if (min_face(face)) then
          call mirror_about_hmin(x_mirror%x,d,mirror_sign(1))
          call mirror_about_hmin(x_mirror%y,d,mirror_sign(2))
          call mirror_about_hmin(x_mirror%z,d,mirror_sign(3))
         endif
         if (max_face(face)) then
          call mirror_about_hmax(x_mirror%x,d,mirror_sign(1))
          call mirror_about_hmax(x_mirror%y,d,mirror_sign(2))
          call mirror_about_hmax(x_mirror%z,d,mirror_sign(3))
         endif
       end subroutine

       end module