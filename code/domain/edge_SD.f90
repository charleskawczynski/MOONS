       module edge_SD_mod
       use current_precision_mod
       use overlap_mod
       use grid_mod
       use datatype_conversion_mod
       use sub_domain_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use coordinates_mod

       implicit none

       private
       public :: edge_SD
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_mixed

       interface init;       module procedure init_edge_SD;        end interface
       interface init;       module procedure init_copy_edge_SD;   end interface
       interface delete;     module procedure delete_edge_SD;      end interface
       interface display;    module procedure display_edge_SD;     end interface
       interface print;      module procedure print_edge_SD;       end interface
       interface export;     module procedure export_edge_SD;      end interface
       interface import;     module procedure import_edge_SD;      end interface

       interface init_mixed; module procedure init_mixed_edge_SD;  end interface

       type b_data
         real(cp),dimension(2) :: dh,nhat = 0.0_cp
       end type

       type edge_SD
         type(sub_domain),dimension(12) :: G
         type(sub_domain),dimension(12) :: B ! C is non-sense here
         type(sub_domain),dimension(12) :: I
         integer,dimension(12) :: i_1D
         type(b_data),dimension(12) :: BD
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_edge_SD(FSD,g,g_b)
         implicit none
         type(edge_SD),intent(inout) :: FSD
         type(grid),intent(in) :: g
         type(grid),dimension(6),intent(in) :: g_b
         type(sub_domain) :: temp,A_temp,B_temp
         real(cp) :: tol
         integer :: i
         integer,dimension(2) :: d
         tol = 10.0_cp**(-12.0_cp)
         call delete(FSD)
         do i=1,12
           d = adj_dir_given_edge(i)
           call init(temp,g_b(i),g,i,0,tol,0)
           call init(FSD%G(i),temp)
           call init(FSD%B(i),temp)
           call init(FSD%I(i),temp)
           call init(A_temp,temp)
           call init(B_temp,temp)

           call p_from_boundary_C(B_temp%C(d(1)),A_temp%C(d(1)),g,g_b(i),d(1),tol,1)
           call p_from_boundary_C(A_temp%C(d(2)),B_temp%C(d(2)),g,g_b(i),d(2),tol,1)
           call p_from_boundary_N(B_temp%N(d(1)),A_temp%N(d(1)),g,g_b(i),d(1),tol,1)
           call p_from_boundary_N(A_temp%N(d(2)),B_temp%N(d(2)),g,g_b(i),d(2),tol,1)
           call init(FSD%G(i),A_temp); call init(A_temp,temp)

           call p_from_boundary_N(A_temp%N(d(1)),B_temp%N(d(1)),g,g_b(i),d(1),tol,2)
           call p_from_boundary_N(A_temp%N(d(2)),B_temp%N(d(2)),g,g_b(i),d(2),tol,2)
           call init(FSD%B(i),A_temp); call init(A_temp,temp)

           call p_from_boundary_C(B_temp%C(d(1)),A_temp%C(d(1)),g,g_b(i),d(1),tol,2)
           call p_from_boundary_C(A_temp%C(d(2)),B_temp%C(d(2)),g,g_b(i),d(2),tol,2)
           call p_from_boundary_N(B_temp%N(d(1)),A_temp%N(d(1)),g,g_b(i),d(1),tol,2)
           call p_from_boundary_N(A_temp%N(d(2)),B_temp%N(d(2)),g,g_b(i),d(2),tol,2)
           call init(FSD%I(i),A_temp); call init(A_temp,temp)

           FSD%BD(i)%dh=(/get_dh_boundary((/g,g_b(i)/),d(1)),get_dh_boundary((/g,g_b(i)/),d(2))/)
           FSD%BD(i)%nhat = nhat_given_edge(i)
           FSD%i_1D(i) = dir_given_edge(i)
         enddo
         call delete(temp)
       end subroutine

       subroutine init_copy_edge_SD(FSD,FSD_in)
         implicit none
         type(edge_SD),intent(inout) :: FSD
         type(edge_SD),intent(in) :: FSD_in
         integer :: i
         do i=1,12; call init(FSD%G(i),FSD_in%G(i)); enddo
         do i=1,12; call init(FSD%B(i),FSD_in%B(i)); enddo
         do i=1,12; call init(FSD%I(i),FSD_in%I(i)); enddo
         do i=1,12; FSD%i_1D(i) = FSD_in%i_1D(i); enddo
         do i=1,12; FSD%BD(i)%nhat = FSD_in%BD(i)%nhat; enddo
         do i=1,12; FSD%BD(i)%dh = FSD_in%BD(i)%dh; enddo
       end subroutine

       subroutine delete_edge_SD(FSD)
         implicit none
         type(edge_SD),intent(inout) :: FSD
         integer :: i
         do i=1,12; call delete(FSD%G(i)); enddo
         do i=1,12; call delete(FSD%B(i)); enddo
         do i=1,12; call delete(FSD%I(i)); enddo
         do i=1,12; FSD%i_1D(i) = 0; enddo
         do i=1,12; FSD%BD(i)%nhat = 0.0_cp; enddo
         do i=1,12; FSD%BD(i)%dh = 0.0_cp; enddo
       end subroutine

       subroutine display_edge_SD(FSD,name,u)
         implicit none
         type(edge_SD),intent(in) :: FSD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ************************* edge_SD ************************* '//name
         do i=1,12; call display(FSD%G(i),'G face '//int2str(i),u); enddo
         do i=1,12; call display(FSD%B(i),'B face '//int2str(i),u); enddo
         do i=1,12; call display(FSD%I(i),'I face '//int2str(i),u); enddo
         write(u,*) 'i_1D = ',FSD%i_1D
         do i=1,12; write(u,*) 'nhat = '; write(u,*) FSD%BD(i)%nhat; enddo
         do i=1,12; write(u,*) 'dh = '; write(u,*) FSD%BD(i)%dh; enddo
         write(u,*) ' *********************************************************** '
       end subroutine

       subroutine print_edge_SD(FSD,name)
         implicit none
         type(edge_SD),intent(in) :: FSD
         character(len=*),intent(in) :: name
         call display(FSD,name,6)
       end subroutine

       subroutine export_edge_SD(FSD,u)
         implicit none
         type(edge_SD),intent(in) :: FSD
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** edge_SD ************ '
         do i=1,12; call export(FSD%G(i),u); enddo
         do i=1,12; call export(FSD%B(i),u); enddo
         do i=1,12; call export(FSD%I(i),u); enddo
         write(u,*) 'i_1D = '; write(u,*) FSD%i_1D
         do i=1,12; write(u,*) 'nhat = '; write(u,*) FSD%BD(i)%nhat; enddo
         do i=1,12; write(u,*) 'dh = '; write(u,*) FSD%BD(i)%dh; enddo
         write(u,*) ' ********************************* '
       end subroutine

       subroutine import_edge_SD(FSD,u)
         implicit none
         type(edge_SD),intent(inout) :: FSD
         integer,intent(in) :: u
         integer :: i
         read(u,*);
         do i=1,12; call import(FSD%G(i),u); enddo
         do i=1,12; call import(FSD%B(i),u); enddo
         do i=1,12; call import(FSD%I(i),u); enddo
         read(u,*); read(u,*) FSD%i_1D
         do i=1,12; read(u,*); read(u,*) FSD%BD(i)%nhat; enddo
         do i=1,12; read(u,*); read(u,*) FSD%BD(i)%dh; enddo
         read(u,*);
       end subroutine

       subroutine init_mixed_edge_SD(FSD,DL)
         implicit none
         type(edge_SD),intent(inout) :: FSD
         type(data_location),intent(in) :: DL
         integer :: i
         do i=1,12; call init_mixed(FSD%G(i)%M,FSD%G(i)%C,FSD%G(i)%N,DL); enddo
         do i=1,12; call init_mixed(FSD%B(i)%M,FSD%B(i)%C,FSD%B(i)%N,DL); enddo
         do i=1,12; call init_mixed(FSD%I(i)%M,FSD%I(i)%C,FSD%I(i)%N,DL); enddo
       end subroutine

       end module