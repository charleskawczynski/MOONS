      module VF_subtract_mod
      use VF_obj_mod
      use SF_mod
      use RF_subtract_mod
      implicit none

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif
      
      private
      public :: subtract
      interface subtract;          module procedure VF_VF;         end interface
      interface subtract;          module procedure VF_VF_VF;      end interface
      interface subtract;          module procedure VF_SF;         end interface
      interface subtract;          module procedure VF_S;          end interface
      interface subtract;          module procedure S_VF;          end interface
      contains
      subroutine VF_VF(f,g)
        implicit none
        type(VF),intent(inout) :: f
        type(VF),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call subtract(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call subtract_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call subtract_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine VF_VF_VF(f,g,r)
        implicit none
        type(VF),intent(inout) :: f
        type(VF),intent(in) :: g,r
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call subtract(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),&
                               r%x%RF(i),r%y%RF(i),r%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call subtract_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),&
                               r%x%RF(i),r%y%RF(i),r%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call subtract_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),&
                               r%x%RF(i),r%y%RF(i),r%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine VF_SF(f,g)
        implicit none
        type(VF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call subtract(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call subtract_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call subtract_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine VF_S(f,g)
        implicit none
        type(VF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call subtract(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call subtract_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call subtract_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine S_VF(g2,f)
        implicit none
        type(VF),intent(inout) :: f
        real(cp),intent(in) :: g2
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call subtract(g2,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call subtract_face(g2,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call subtract_edge(g2,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine
      end module