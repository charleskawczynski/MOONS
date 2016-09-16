      subroutine invert_VF(f)
        implicit none
        type(VF),intent(inout) :: f
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call divide(1.0_cp,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call divide_face(1.0_cp,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call divide_edge(1.0_cp,f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine invert_VF_S(f,g)
        implicit none
        type(VF),intent(inout) :: f
        type(VF),intent(in) :: g
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call divide(f%x%RF(i),f%y%RF(i),f%z%RF(i),1.0_cp,&
        g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call divide_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),1.0_cp,&
        g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call divide_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),1.0_cp,&
        g%x%RF(i),g%y%RF(i),g%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      subroutine square_VF(f)
        implicit none
        type(VF),intent(inout) :: f
        integer :: i
        if (f%is_CC.or.f%is_Node) then
        do i=1,f%x%s; call square(f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%s); enddo
        elseif (f%is_Face) then
        do i=1,f%x%s; call square_face(f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        elseif (f%is_Edge) then
        do i=1,f%x%s; call square_edge(f%x%RF(i),f%y%RF(i),f%z%RF(i),(/1,1,1/),f%x%RF(i)%sc); enddo
        endif
      end subroutine

      function dot_product_VF(A,B,temp) result(dot)
        implicit none
        type(VF),intent(in) :: A,B
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        integer :: i
        call multiply(temp,A,B)
        dot = 0.0_cp
        do i=1,A%s
          if (temp%is_CC.or.temp%is_Node) then; dot = dot + &
          sum(temp%x%RF(i),temp%y%RF(i),temp%z%RF(i),(/1,1,1/),temp%x%RF(i)%s)
          elseif (temp%is_Face) then;           dot = dot + &
          sum_face(temp%x%RF(i),temp%y%RF(i),temp%z%RF(i),(/1,1,1/),A%x%RF(i)%sc)
          elseif (temp%is_Edge) then;           dot = dot + &
          sum_edge(temp%x%RF(i),temp%y%RF(i),temp%z%RF(i),(/1,1,1/),A%x%RF(i)%sc)
          endif
        enddo
      end function

      function sum_VF(A) result(s)
        implicit none
        type(VF),intent(in) :: A
        real(cp) :: s
        integer :: i
        s = 0.0_cp
        do i=1,A%s
          if (A%is_CC.or.A%is_Node) then; s = s + &
          sum(A%x%RF(i),A%y%RF(i),A%z%RF(i),(/1,1,1/),A%x%RF(i)%s)
          elseif (A%is_Face) then;           s = s + &
          sum_face(A%x%RF(i),A%y%RF(i),A%z%RF(i),(/1,1,1/),A%x%RF(i)%sc)
          elseif (A%is_Edge) then;           s = s + &
          sum_edge(A%x%RF(i),A%y%RF(i),A%z%RF(i),(/1,1,1/),A%x%RF(i)%sc)
          endif
        enddo
      end function
