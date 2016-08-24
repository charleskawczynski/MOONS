      subroutine assign_VF_VF(f,g)
        implicit none
        type(VF),intent(inout) :: f
        type(VF),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call assign(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call assign_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call assign_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),&
                               g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine assign_VF_SF(f,g)
        implicit none
        type(VF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call assign(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call assign_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call assign_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),g%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine assign_VF_S(f,g)
        implicit none
        type(VF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call assign(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call assign_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call assign_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),g,(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine
