SF and array need a more protected IO:

can show percentage_complete when traversing through data structures:
       subroutine export_structured_D_bctype(this,dir,counter,total_count)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,intent(inout),optional :: counter,total_count
         integer :: un
         if( present(counter) )
         counter = counter+1
         else
         counter = 0
         total_count = get_total_count(this)
         endif
         write(*,*) 'percentage_complete = ',real(counter,cp)/real(total_count,cp)*100_cp
         write(*,*) 'Exporting bctype structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine export_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         integer :: s_f
         if (allocated(this%f)) then
           s_f = size(this%f)
           write(un,*) s_f
           if (s_f.gt.0) then
             write(un,*) 'f  = ';write(un,*) this%f
           endif
         else
           write(un,*) 0
         endif
         write(un,*) 'N  = ';write(un,*) this%N
       end subroutine

       subroutine import_array(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         integer :: s_f
         call delete(this)
         read(un,*) s_f
         if (s_f.gt.0) then
           allocate(this%f(s_f))
           read(un,*); read(un,*) this%f
         endif
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine export_SF(this,un)
         implicit none
         type(SF),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           write(un,*) s_BF
           if (s_BF.gt.0) then
             do i_BF=1,s_BF
               call export(this%BF(i_BF),un)
             enddo
           endif
         else
           write(un,*) 0
         endif
         write(un,*) 'all_neumann  = ';write(un,*) this%all_neumann
         write(un,*) 'numEl        = ';write(un,*) this%numEl
         write(un,*) 'numPhysEl    = ';write(un,*) this%numPhysEl
         write(un,*) 'vol          = ';write(un,*) this%vol
         write(un,*) 's            = ';write(un,*) this%s
         call export(this%DL,un)
       end subroutine

       subroutine import_SF(this,un)
         implicit none
         type(SF),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         call delete(this)
         read(un,*) s_BF
         if (s_BF.gt.0) then
           do i_BF=1,s_BF
             call import(this%BF(i_BF),un)
           enddo
         endif
         read(un,*); read(un,*) this%all_neumann
         read(un,*); read(un,*) this%numEl
         read(un,*); read(un,*) this%numPhysEl
         read(un,*); read(un,*) this%vol
         read(un,*); read(un,*) this%s
         call import(this%DL,un)
       end subroutine

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

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call delete(p(i)); enddo
         endif
       end subroutine

       subroutine export_probe_wrapper_dim(p,dir,name)
         implicit none
         type(probe),dimension(:),intent(in) :: p
         character(len=*),intent(in) :: dir,name
         integer :: i
         if (size(p).gt.0) then
           do i=1,size(p); call export(p(i),dir,name); enddo
         endif
       end subroutine

       subroutine import_probe_wrapper_dim(p,dir,name)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         character(len=*),intent(in) :: dir,name
         integer :: i
         if (size(p).gt.0) then
         do i=1,size(p); call import(p(i),dir,name); enddo
         endif
       end subroutine

#ifdef _DEBUG_DATA_LOCATION_
       subroutine insist_defined(DL,caller)
         implicit none
         type(data_location),intent(in) :: DL
         character(len=*),intent(in) :: caller
         if (.not.DL%defined) then
           call print(DL)
           write(*,*) 'Error: undefined DL in ',caller,' in data_location.f90'
           stop 'Done'
         endif
       end subroutine
#endif
