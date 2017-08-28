       module ops_mirror_field_mod
       use current_precision_mod
       use mesh_mod
       use face_edge_corner_indexing_mod
       use mirror_props_mod
       use SF_mod
       use VF_mod

       implicit none
       private
       public :: mirror_field
       interface mirror_field;      module procedure mirror_field_SF;      end interface
       interface mirror_field;      module procedure mirror_field_VF;      end interface

       public :: anti_mirror_field
       interface anti_mirror_field; module procedure anti_mirror_field_VF; end interface

       public :: mirror_mesh
       interface mirror_mesh;       module procedure mirror_mesh_MF;       end interface

       contains

       subroutine mirror_mesh_MF(m_mirror,m,MP)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(mesh),intent(in) :: m
         type(mirror_props),intent(in) :: MP
         integer :: d
         call init(m_mirror,m)
         d = dir_given_face(MP%mirror_face)
         if (min_face(MP%mirror_face)) call mirror_about_hmin(m_mirror,d)
         if (max_face(MP%mirror_face)) call mirror_about_hmax(m_mirror,d)
         call init_props(m_mirror)
         call patch(m_mirror)
       end subroutine

       subroutine mirror_field_SF(m_mirror,x_mirror,m,x,MP)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(SF),intent(inout) :: x_mirror
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         type(mirror_props),intent(in) :: MP
         integer :: d
         call mirror_mesh(m_mirror,m,MP)
         call init(x_mirror,x)
         call assign(x_mirror,x)
         d = dir_given_face(MP%mirror_face)
         if (min_face(MP%mirror_face)) call mirror_about_hmin(x_mirror,d,1.0_cp)
         if (max_face(MP%mirror_face)) call mirror_about_hmax(x_mirror,d,1.0_cp)
       end subroutine

       subroutine mirror_field_VF(m_mirror,x_mirror,m,x,MP)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(VF),intent(inout) :: x_mirror
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         type(mirror_props),intent(in) :: MP
         integer :: d
         call mirror_mesh(m_mirror,m,MP)
         call init(x_mirror,x)
         call assign(x_mirror,x)
         d = dir_given_face(MP%mirror_face)
         if (min_face(MP%mirror_face)) then
          call mirror_about_hmin(x_mirror%x,d,MP%mirror_sign(1))
          call mirror_about_hmin(x_mirror%y,d,MP%mirror_sign(2))
          call mirror_about_hmin(x_mirror%z,d,MP%mirror_sign(3))
         endif
         if (max_face(MP%mirror_face)) then
          call mirror_about_hmax(x_mirror%x,d,MP%mirror_sign(1))
          call mirror_about_hmax(x_mirror%y,d,MP%mirror_sign(2))
          call mirror_about_hmax(x_mirror%z,d,MP%mirror_sign(3))
         endif
       end subroutine

       subroutine anti_mirror_field_VF(m_mirror,x_mirror,m,x,MP)
         implicit none
         type(mesh),intent(inout) :: m_mirror
         type(VF),intent(inout) :: x_mirror
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         type(mirror_props),intent(in) :: MP
         integer :: d
         call mirror_mesh(m_mirror,m,MP)
         call init(x_mirror,x)
         call assign(x_mirror,x)
         d = dir_given_face(MP%mirror_face)
         if (min_face(MP%mirror_face)) then
          call mirror_about_hmin(x_mirror%x,d,MP%mirror_sign_a(1))
          call mirror_about_hmin(x_mirror%y,d,MP%mirror_sign_a(2))
          call mirror_about_hmin(x_mirror%z,d,MP%mirror_sign_a(3))
         endif
         if (max_face(MP%mirror_face)) then
          call mirror_about_hmax(x_mirror%x,d,MP%mirror_sign_a(1))
          call mirror_about_hmax(x_mirror%y,d,MP%mirror_sign_a(2))
          call mirror_about_hmax(x_mirror%z,d,MP%mirror_sign_a(3))
         endif
       end subroutine

       end module