       module mirror_props_extend_mod
       use mirror_props_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use IO_tools_mod

       implicit none
       private
       public :: mirror_props
       public :: init
       public :: anti_mirror

       interface init;           module procedure init_MP;           end interface
       interface anti_mirror;    module procedure anti_mirror_MP;    end interface

       contains

       subroutine init_MP(MP,mirror,mirror_face)
         implicit none
         type(mirror_props),intent(inout) :: MP
         logical,intent(in) :: mirror
         integer,intent(in) :: mirror_face
         integer :: dir
         MP%mirror      = mirror
         MP%mirror_face = mirror_face
         MP%mirror_sign = (/1.0_cp,1.0_cp,1.0_cp/)
         dir = dir_given_face(mirror_face)
         MP%mirror_sign_a = -MP%mirror_sign
         MP%mirror_sign_a(dir) = 1.0_cp
         MP%mirror_sign(dir) = -1.0_cp
       end subroutine

       function anti_mirror_MP(MP_in) result(MP)
         implicit none
         type(mirror_props),intent(in) :: MP_in
         real(cp),dimension(3) :: temp
         type(mirror_props) :: MP
         call init(MP,MP_in)
         temp = MP%mirror_sign
         MP%mirror_sign = MP%mirror_sign_a
         MP%mirror_sign_a = temp
       end function

       end module