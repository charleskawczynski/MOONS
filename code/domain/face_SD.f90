       module face_SD_mod
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
       public :: face_SD
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_mixed
       public :: init_Robin_coeff

       interface init;             module procedure init_face_SD;        end interface
       interface init;             module procedure init_copy_face_SD;   end interface
       interface delete;           module procedure delete_face_SD;      end interface
       interface display;          module procedure display_face_SD;     end interface
       interface print;            module procedure print_face_SD;       end interface
       interface export;           module procedure export_face_SD;      end interface
       interface import;           module procedure import_face_SD;      end interface

       interface init_mixed;       module procedure init_mixed_face_SD;  end interface
       interface init_Robin_coeff; module procedure init_Robin_coeff_SD; end interface

       type index_2D
         integer,dimension(2) :: i = 0
       end type

       type face_SD
         type(sub_domain),dimension(6) :: G
         type(sub_domain),dimension(6) :: G_periodic_N
         type(sub_domain),dimension(6) :: B ! B%C is non-sense here
         type(sub_domain),dimension(6) :: I
         type(sub_domain),dimension(6) :: I_OPP
         type(sub_domain),dimension(6) :: I_OPP_periodic_N
         type(index_2D),dimension(6) :: i_2D
         real(cp),dimension(6) :: dh,nhat,Robin_coeff = 0.0_cp
       end type

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

       subroutine init_copy_face_SD(FSD,FSD_in)
         implicit none
         type(face_SD),intent(inout) :: FSD
         type(face_SD),intent(in) :: FSD_in
         integer :: i
         do i=1,6; call init(FSD%G(i),FSD_in%G(i)); enddo
         do i=1,6; call init(FSD%B(i),FSD_in%B(i)); enddo
         do i=1,6; call init(FSD%I(i),FSD_in%I(i)); enddo
         do i=1,6; call init(FSD%G_periodic_N(i),FSD_in%G_periodic_N(i)); enddo
         do i=1,6; call init(FSD%I_OPP(i),FSD_in%I_OPP(i)); enddo
         do i=1,6; call init(FSD%I_OPP_periodic_N(i),FSD_in%I_OPP_periodic_N(i)); enddo
         do i=1,6; FSD%i_2D(i)%i = FSD_in%i_2D(i)%i; enddo
         FSD%dh = FSD_in%dh
         FSD%nhat = FSD_in%nhat
         FSD%Robin_coeff = FSD_in%Robin_coeff
       end subroutine

       subroutine delete_face_SD(FSD)
         implicit none
         type(face_SD),intent(inout) :: FSD
         integer :: i
         do i=1,6; call delete(FSD%G(i)); enddo
         do i=1,6; call delete(FSD%B(i)); enddo
         do i=1,6; call delete(FSD%I(i)); enddo
         do i=1,6; call delete(FSD%G_periodic_N(i)); enddo
         do i=1,6; call delete(FSD%I_OPP(i)); enddo
         do i=1,6; call delete(FSD%I_OPP_periodic_N(i)); enddo
         do i=1,6; FSD%i_2D(i)%i = 0; enddo
         FSD%dh = 0.0_cp
         FSD%nhat = 0.0_cp
         FSD%Robin_coeff = 0.0_cp
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
         do i=1,6; write(u,*) 'i_2D = '; write(u,*) FSD%i_2D(i)%i; enddo
         write(u,*) ' *********************************************************** '
       end subroutine

       subroutine print_face_SD(FSD,name)
         implicit none
         type(face_SD),intent(in) :: FSD
         character(len=*),intent(in) :: name
         call display(FSD,name,6)
       end subroutine

       subroutine export_face_SD(FSD,u)
         implicit none
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** face_SD ************ '
         do i=1,6; call export(FSD%G(i),u); enddo
         do i=1,6; call export(FSD%B(i),u); enddo
         do i=1,6; call export(FSD%I(i),u); enddo
         do i=1,6; call export(FSD%G_periodic_N(i),u); enddo
         do i=1,6; call export(FSD%I_OPP(i),u); enddo
         do i=1,6; call export(FSD%I_OPP_periodic_N(i),u); enddo
         write(u,*) 'dh = ';      write(u,*) FSD%dh
         write(u,*) 'nhat = ';    write(u,*) FSD%nhat
         write(u,*) 'Robin_coeff = ';    write(u,*) FSD%Robin_coeff
         do i=1,6; write(u,*) 'i_2D = '; write(u,*) FSD%i_2D(i)%i; enddo
         write(u,*) ' ********************************* '
       end subroutine

       subroutine import_face_SD(FSD,u)
         implicit none
         type(face_SD),intent(inout) :: FSD
         integer,intent(in) :: u
         integer :: i
         read(u,*);
         do i=1,6; call import(FSD%G(i),u); enddo
         do i=1,6; call import(FSD%B(i),u); enddo
         do i=1,6; call import(FSD%I(i),u); enddo
         do i=1,6; call import(FSD%G_periodic_N(i),u); enddo
         do i=1,6; call import(FSD%I_OPP(i),u); enddo
         do i=1,6; call import(FSD%I_OPP_periodic_N(i),u); enddo
         read(u,*); write(u,*) FSD%dh
         read(u,*); write(u,*) FSD%nhat
         read(u,*); write(u,*) FSD%Robin_coeff
         do i=1,6; read(u,*); read(u,*) FSD%i_2D(i)%i; enddo
         read(u,*);
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

       subroutine init_Robin_coeff_SD(FSD,Robin_coeff)
         implicit none
         type(face_SD),intent(inout) :: FSD
         real(cp),dimension(6),intent(in) :: Robin_coeff
         FSD%Robin_coeff = Robin_coeff
       end subroutine

       end module