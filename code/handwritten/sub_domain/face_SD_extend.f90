       module face_SD_extend_mod
       use face_SD_mod
       use current_precision_mod
       use index_2D_mod
       use overlap_mod
       use overlap_extend_mod
       use grid_mod
       use datatype_conversion_mod
       use sub_domain_mod
       use sub_domain_extend_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use coordinates_mod

       implicit none

       private
       public :: face_SD
       public :: init,display,print ! Essentials

       public :: init_mixed
       public :: init_Robin_coeff

       interface init;             module procedure init_face_SD;        end interface
       interface display;          module procedure display_face_SD;     end interface
       interface print;            module procedure print_face_SD;       end interface

       interface init_mixed;       module procedure init_mixed_face_SD;  end interface
       interface init_Robin_coeff; module procedure init_Robin_coeff_SD; end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_face_SD(FSD,g,g_b)
         implicit none
         type(face_SD),intent(inout) :: FSD
         type(grid),intent(in) :: g
         type(grid),dimension(6),intent(in) :: g_b
         type(sub_domain) :: temp
         real(cp) :: tol
         integer :: i,dir,i_opp
         logical :: many_cell
         tol = 10.0_cp**(-12.0_cp)
         call delete(FSD)
         do i=1,6
           dir = dir_given_face(i)
           call init(temp,g_b(i),g,i,0,tol,0)
           call init(FSD%G(i),temp)
           call init(FSD%B(i),temp)
           call init(FSD%I(i),temp)
           call init(FSD%I_OPP(i),temp)
           call init(FSD%G_periodic_N(i),temp)
           call init(FSD%I_OPP_periodic_N(i),temp)
           call p_from_boundary_C(FSD%G(i)%C(dir),temp%C(dir),g,g_b(i),dir,tol,1)
           call p_from_boundary_C(FSD%I(i)%C(dir),temp%C(dir),g,g_b(i),dir,tol,2)

           call p_from_boundary_N(FSD%G(i)%N(dir),temp%N(dir),g,g_b(i),dir,tol,1)
           call p_from_boundary_N(FSD%B(i)%N(dir),temp%N(dir),g,g_b(i),dir,tol,2)
           call p_from_boundary_N(FSD%I(i)%N(dir),temp%N(dir),g,g_b(i),dir,tol,3)
           FSD%dh(i) = get_dh_boundary((/g,g_b(i)/),dir)
           FSD%nhat(i) = nhat_given_face(i)
           FSD%i_2D(i)%i = adj_dir_given_face(i)
         enddo

         do i=1,6
           i_opp = opp_face_given_face(i)
           call init(FSD%I_OPP(i_opp),FSD%I(i))
         enddo
         do i=1,6
           call init(FSD%I_OPP_periodic_N(i),FSD%I_OPP(i))
           call init(FSD%G_periodic_N(i),FSD%G(i))
         enddo

         ! For Periodic BCs, node points must be truncated by 1,
         ! to solve the system correctly. If there's a single
         ! interior cell (not multicell), however, then periodic
         ! BCs should be symmetric and simply enforce no changes
         ! along the single interior cell.
         do i=1,6
           i_opp = opp_face_given_face(i)
           dir = dir_given_face(i)
           many_cell = g%c(dir)%sn.gt.4
           if (max_face(i).and.many_cell) call init(FSD%G_periodic_N(i),FSD%B(i))
           if (max_face(i).and.many_cell) call init(FSD%I_OPP_periodic_N(i),FSD%B(i_opp))
         enddo
         call delete(temp)
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

       subroutine print_face_SD(FSD,name)
         implicit none
         type(face_SD),intent(in) :: FSD
         character(len=*),intent(in) :: name
         call display(FSD,name,6)
       end subroutine

       subroutine init_mixed_face_SD(FSD,DL)
         implicit none
         type(face_SD),intent(inout) :: FSD
         type(data_location),intent(in) :: DL
         integer :: i
         do i=1,6; call init_mixed(FSD%G(i)%M,FSD%G(i)%C,FSD%G(i)%N,DL); enddo
         do i=1,6; call init_mixed(FSD%B(i)%M,FSD%B(i)%C,FSD%B(i)%N,DL); enddo
         do i=1,6; call init_mixed(FSD%I(i)%M,FSD%I(i)%C,FSD%I(i)%N,DL); enddo
         do i=1,6; call init_mixed(FSD%I_OPP(i)%M,FSD%I_OPP(i)%C,FSD%I_OPP(i)%N,DL); enddo
         do i=1,6; call init_mixed(FSD%G_periodic_N(i)%M,&
          FSD%G_periodic_N(i)%C,FSD%G_periodic_N(i)%N,DL); enddo

         do i=1,6; call init_mixed(FSD%I_OPP_periodic_N(i)%M,&
          FSD%I_OPP_periodic_N(i)%C,FSD%I_OPP_periodic_N(i)%N,DL); enddo
       end subroutine

       subroutine init_Robin_coeff_SD(FSD,c_w,Robin_coeff)
         implicit none
         type(face_SD),intent(inout) :: FSD
         real(cp),dimension(6),intent(in) :: c_w,Robin_coeff
         FSD%c_w = c_w
         FSD%Robin_coeff = Robin_coeff
       end subroutine

       end module