       module sub_domain_mod
       use current_precision_mod
       use overlap_mod
       use grid_mod
       use data_location_mod
       use face_edge_corner_indexing_mod
       use coordinates_mod

       implicit none

       private
       public :: sub_domain
       public :: init,delete,display,print,export,import ! Essentials

       ! public::init               ! call init(SD,g_R1,g_R2,tol,p)
       public :: define_C           ! call define_C(OL_inout,g1,g2,dir,p)
       public :: define_N           ! call define_N(OL_inout,g1,g2,dir,p)
       public :: p_from_boundary_C  ! call p_from_boundary_C(OL_out,OL_in,g1,g2,dir,p)
       public :: p_from_boundary_N  ! call p_from_boundary_N(OL_out,OL_in,g1,g2,dir,p)
       public :: get_dh_boundary    ! call get_dh_boundary(g1,g2,dir)
       public :: init_mixed

       interface init;            module procedure init_sub_domain;              end interface
       interface init;            module procedure init_copy_sub_domain;         end interface
       interface delete;          module procedure delete_sub_domain;            end interface
       interface display;         module procedure display_sub_domain;           end interface
       interface print;           module procedure print_sub_domain;             end interface
       interface export;          module procedure export_sub_domain;            end interface
       interface import;          module procedure import_sub_domain;            end interface

       interface init_mixed;      module procedure init_mixed_sub_domain;        end interface

       type sub_domain
         type(overlap),dimension(3) :: C ! cell center
         type(overlap),dimension(3) :: N ! node
         type(overlap),dimension(3) :: M ! mixed
         logical :: defined = .false.
         integer :: g_R1_id = 0
         integer :: g_R2_id = 0
       end type

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

       subroutine init_copy_sub_domain(SD,SD_in)
         implicit none
         type(sub_domain),intent(inout) :: SD
         type(sub_domain),intent(in) :: SD_in
         integer :: i
         do i=1,3; call init(SD%C(i),SD_in%C(i)); enddo
         do i=1,3; call init(SD%N(i),SD_in%N(i)); enddo
         do i=1,3; call init(SD%M(i),SD_in%M(i)); enddo
         SD%defined = SD_in%defined
         SD%g_R1_id = SD_in%g_R1_id
         SD%g_R2_id = SD_in%g_R2_id
       end subroutine

       subroutine delete_sub_domain(SD)
         implicit none
         type(sub_domain),intent(inout) :: SD
         integer :: i
         do i=1,3; call delete(SD%C(i)); enddo
         do i=1,3; call delete(SD%N(i)); enddo
         do i=1,3; call delete(SD%M(i)); enddo
         SD%defined = .false.
         SD%g_R1_id = 0
         SD%g_R2_id = 0
       end subroutine

       subroutine display_sub_domain(SD,name,u)
         implicit none
         type(sub_domain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ---------- sub_domain ---------- '//name
         write(u,*) 'defined,g_R1_id,g_R2_id = ',SD%defined,SD%g_R1_id,SD%g_R2_id
         write(u,*) ''
         write(u,*) 'C_iR = ',(/(SD%C(i)%iR,i=1,3)/)
         write(u,*) 'N_iR = ',(/(SD%N(i)%iR,i=1,3)/)
         write(u,*) ''
         write(u,*) 'C_i1(1) = ',(/(SD%C(i)%i1(1),i=1,3)/)
         write(u,*) 'C_i1(2) = ',(/(SD%C(i)%i1(2),i=1,3)/)
         write(u,*) 'C_i2(1) = ',(/(SD%C(i)%i2(1),i=1,3)/)
         write(u,*) 'C_i2(2) = ',(/(SD%C(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'N_i1(1) = ',(/(SD%N(i)%i1(1),i=1,3)/)
         write(u,*) 'N_i1(2) = ',(/(SD%N(i)%i1(2),i=1,3)/)
         write(u,*) 'N_i2(1) = ',(/(SD%N(i)%i2(1),i=1,3)/)
         write(u,*) 'N_i2(2) = ',(/(SD%N(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'M_i1(1) = ',(/(SD%M(i)%i1(1),i=1,3)/)
         write(u,*) 'M_i1(2) = ',(/(SD%M(i)%i1(2),i=1,3)/)
         write(u,*) 'M_i2(1) = ',(/(SD%M(i)%i2(1),i=1,3)/)
         write(u,*) 'M_i2(2) = ',(/(SD%M(i)%i2(2),i=1,3)/)
         write(u,*) ' -------------------------------- '
       end subroutine

       subroutine print_sub_domain(SD,name)
         implicit none
         type(sub_domain),intent(in) :: SD
         character(len=*),intent(in) :: name
         call display(SD,name,6)
       end subroutine

       subroutine export_sub_domain(SD,u)
         implicit none
         type(sub_domain),intent(in) :: SD
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** sub_domain ************ '
         write(u,*) 'defined = '; write(u,*) SD%defined
         write(u,*) 'g_R1_id = '; write(u,*) SD%g_R1_id
         write(u,*) 'g_R2_id = '; write(u,*) SD%g_R2_id
         do i=1,3; call export(SD%C(i),u); enddo
         do i=1,3; call export(SD%N(i),u); enddo
         do i=1,3; call export(SD%M(i),u); enddo
         write(u,*) ' ********************************* '
       end subroutine

       subroutine import_sub_domain(SD,u)
         implicit none
         type(sub_domain),intent(inout) :: SD
         integer,intent(in) :: u
         integer :: i
         read(u,*)
         read(u,*); read(u,*) SD%defined
         read(u,*); read(u,*) SD%g_R1_id
         read(u,*); read(u,*) SD%g_R2_id
         do i=1,3; call import(SD%C(i),u); enddo
         do i=1,3; call import(SD%N(i),u); enddo
         do i=1,3; call import(SD%M(i),u); enddo
         read(u,*)
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
             if (g(1)%c(dir)%sn.eq.3) then; dh = g(1)%c(dir)%dhn(1)
         elseif (g(2)%c(dir)%sn.eq.3) then; dh = g(2)%c(dir)%dhn(1)
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
          call init(M(DL%face),N(DL%face))
         elseif (is_Edge(DL)) then
          do i=1,3; call init(M(i),N(i)); enddo
          call init(M(DL%edge),C(DL%edge))
         elseif (is_CC(DL)) then
          do i=1,3; call init(M(i),C(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(M(i),C(i)); enddo
         else; stop 'Error: no type found in init_props_physical_overlap in physical_overlap.f90'
         endif
       end subroutine

       end module