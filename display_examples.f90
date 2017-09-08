        subroutine display_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
              write(un,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

        subroutine display_pad_GF(a,p,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un,p
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1+p,a%s(3)-p; do j=1+p,a%s(2)-p; do i=1+p,a%s(1)-p
              write(un,*) 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

       subroutine display_PD(D,un)
         implicit none
         type(physical_domain),intent(inout) :: D
         integer,intent(in) :: un
         integer :: i
         do i=1,D%s; call display(D%sd(i),'SD_'//int2str(i),un); enddo
       end subroutine



       subroutine display_boundary(B,un)
         implicit none
         type(boundary),intent(in) :: B
         integer,intent(in) :: un
         integer :: i,col_width,precision
         if (B%BCL%defined) then
           precision = 4; col_width = 10
           call export_table('ID         :',(/(i,i=1,B%n)/),col_width,un)
           call export_table('Type       :',(/(get_bctype(B%SB(i)%bct),i=1,B%n)/),col_width,un)
           call export_table('meanVal    :',(/(get_mean_value(B%SB(i)%bct),i=1,B%n)/),col_width,precision,un)
           call export_table('prescribed :',(/(is_prescribed(B%SB(i)%bct),i=1,B%n)/),col_width,un)
         endif
       end subroutine

       subroutine display_face_SD(FSD,name,u)
         implicit none
         type(face_SD),intent(in) :: FSD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ************************* face_SD ************************* '//name
         do i=1,6; call display(FSD%G(i),'G face '//int2str(i),u); enddo
         do i=1,6; call display(FSD%B(i),'B face '//int2str(i),u); enddo
         do i=1,6; call display(FSD%I(i),'I face '//int2str(i),u); enddo
         do i=1,6; call display(FSD%G_periodic_N(i),'G_periodic_N face '//int2str(i),u); enddo
         do i=1,6; call display(FSD%I_OPP(i),'I_OPP face '//int2str(i),u); enddo
         do i=1,6; call display(FSD%I_OPP_periodic_N(i),'I_OPP_periodic_N face '//int2str(i),u); enddo
         write(u,*) 'dh = ',FSD%dh
         write(u,*) 'nhat = ',FSD%nhat
         write(u,*) 'Robin_coeff = ',FSD%Robin_coeff
         write(u,*) 'c_w = ',FSD%c_w
         do i=1,6; write(u,*) 'i_2D = '; write(u,*) FSD%i_2D(i)%i; enddo
         write(u,*) ' *********************************************************** '
       end subroutine

       subroutine display_sub_domain(SD,name,u)
         implicit none
         type(sub_domain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         write(u,*) ' ---------- sub_domain ---------- '//name
         write(u,*) 'defined,g_R1_id,g_R2_id = ',SD%defined,SD%g_R1_id,SD%g_R2_id
         write(u,*) ''
         write(u,*) 'C:'
         call display(SD%C,u)
         write(u,*) 'N:'
         call display(SD%N,u)
         write(u,*) 'M:'
         call display(SD%M,u)
         write(u,*) ' -------------------------------- '
       end subroutine
