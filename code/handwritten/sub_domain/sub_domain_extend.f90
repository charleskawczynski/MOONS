       module sub_domain_extend_mod
       use sub_domain_mod
       use current_precision_mod
       use overlap_mod
       use overlap_extend_mod
       use grid_mod
       use data_location_mod
       use face_edge_corner_indexing_mod
       use coordinates_mod

       implicit none

       private
       public :: init,display,print ! Essentials

       ! public::init               ! call init(SD,g_R1,g_R2,tol,p)
       public :: define_C           ! call define_C(OL_inout,g1,g2,dir,p)
       public :: define_N           ! call define_N(OL_inout,g1,g2,dir,p)
       public :: p_from_boundary_C  ! call p_from_boundary_C(OL_out,OL_in,g1,g2,dir,p)
       public :: p_from_boundary_N  ! call p_from_boundary_N(OL_out,OL_in,g1,g2,dir,p)
       public :: get_dh_boundary    ! call get_dh_boundary(g1,g2,dir)
       public :: init_mixed

       interface init;            module procedure init_sub_domain;              end interface
       interface display;         module procedure display_sub_domain;           end interface
       interface print;           module procedure print_sub_domain;             end interface

       interface init_mixed;      module procedure init_mixed_sub_domain;        end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_sub_domain(SD,g_R1,g_R2,g_R1_id,g_R2_id,tol,p)
         implicit none
         type(sub_domain),intent(inout) :: SD
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_R1_id,g_R2_id,p
         real(cp),intent(in) :: tol
         integer :: i
         logical,dimension(3) :: L
         call delete(SD)
         do i=1,3; L(i) = is_overlap(g_R1%c(i),g_R2%c(i),tol); enddo
         SD%defined = all(L)
         if (SD%defined) then
           do i=1,3; call define_C(SD%C(i),g_R1,g_R2,i,tol,p); enddo
           do i=1,3; call define_N(SD%N(i),g_R1,g_R2,i,tol,p); enddo
           SD%g_R1_id = g_R1_id
           SD%g_R2_id = g_R2_id
         endif
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

       subroutine print_sub_domain(SD,name)
         implicit none
         type(sub_domain),intent(in) :: SD
         character(len=*),intent(in) :: name
         call display(SD,name,6)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine define_C(OL_inout,g1,g2,dir,tol,p)
         implicit none
         type(overlap),intent(inout) :: OL_inout
         type(grid),intent(in) :: g1,g2
         integer,intent(in) :: dir,p
         real(cp),intent(in) :: tol
         call init(OL_inout,get_C_overlap((/g1%c(dir),g2%c(dir)/),tol,p))
       end subroutine

       subroutine define_N(OL_inout,g1,g2,dir,tol,p)
         implicit none
         type(overlap),intent(inout) :: OL_inout
         type(grid),intent(in) :: g1,g2
         integer,intent(in) :: dir,p
         real(cp),intent(in) :: tol
         call init(OL_inout,get_N_overlap((/g1%c(dir),g2%c(dir)/),tol,p))
       end subroutine

       subroutine p_from_boundary_C(OL_out,OL_in,g1,g2,dir,tol,p)
         implicit none
         type(overlap),intent(inout) :: OL_out
         type(overlap),intent(in) :: OL_in
         type(grid),intent(in) :: g1,g2
         integer,intent(in) :: dir,p
         real(cp),intent(in) :: tol
         call init(OL_out,get_p_from_boundary_C(OL_in,(/g1%c(dir),g2%c(dir)/),tol,p))
       end subroutine

       subroutine p_from_boundary_N(OL_out,OL_in,g1,g2,dir,tol,p)
         implicit none
         type(overlap),intent(inout) :: OL_out
         type(overlap),intent(in) :: OL_in
         type(grid),intent(in) :: g1,g2
         integer,intent(in) :: dir,p
         real(cp),intent(in) :: tol
         call init(OL_out,get_p_from_boundary_N(OL_in,(/g1%c(dir),g2%c(dir)/),tol,p))
       end subroutine

       ! *********************************************************************
       ! *************************** BOUNDARY dh *****************************
       ! *********************************************************************

       function get_dh_boundary(g,dir) result(dh)
         implicit none
         type(grid),dimension(2),intent(in) :: g
         integer,intent(in) :: dir
         real(cp) :: dh
             if (g(1)%c(dir)%sn.eq.3) then; dh = g(1)%c(dir)%dhn%f(1)
         elseif (g(2)%c(dir)%sn.eq.3) then; dh = g(2)%c(dir)%dhn%f(1)
         else; dh = 0.0_cp
         endif
       end function

       subroutine init_mixed_sub_domain(M,C,N,DL)
         implicit none
         type(overlap),dimension(3),intent(inout) :: M
         type(data_location),intent(in) :: DL
         type(overlap),dimension(3),intent(in) :: C,N
         integer :: i
         if (is_Face(DL)) then
          do i=1,3; call init(M(i),C(i)); enddo
          call init(M(get_Face(DL)),N(get_Face(DL)))
         elseif (is_Edge(DL)) then
          do i=1,3; call init(M(i),N(i)); enddo
          call init(M(get_Edge(DL)),C(get_Edge(DL)))
         elseif (is_CC(DL)) then
          do i=1,3; call init(M(i),C(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(M(i),N(i)); enddo
         else; stop 'Error: no type found in init_props_physical_overlap in physical_overlap.f90'
         endif
       end subroutine

       end module