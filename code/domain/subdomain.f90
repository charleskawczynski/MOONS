       module subdomain_mod
       use current_precision_mod
       use overlap_mod
       use grid_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use coordinates_mod

       implicit none

       private
       public :: subdomain
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_props

       interface init;       module procedure init_subdomain;        end interface
       interface init;       module procedure init_copy_subdomain;   end interface
       interface delete;     module procedure delete_subdomain;      end interface
       interface display;    module procedure display_subdomain;     end interface
       interface print;      module procedure print_subdomain;       end interface
       interface export;     module procedure export_subdomain;      end interface
       interface import;     module procedure import_subdomain;      end interface

       interface init_props; module procedure init_props_subdomain;  end interface

       type subdomain
         ! Legend:
         !        C = Cell Center
         !              E = exclude first exterior point
         !              I = include first exterior point
         !        N = Node
         !              B = include boundary point (for node data)
         !              I = include first exterior point
         !              E = exclude boundary point

         ! Entire overlaps (for testing)
         type(overlap),dimension(3) :: CI_all
         type(overlap),dimension(3) :: NI_all

         ! Physical overlaps
         type(overlap),dimension(3) :: CE_all ! For inter-grid overlap
         type(overlap),dimension(3) :: NB_all ! For inter-grid overlap

         ! Boundary (intra-grid) overlaps
         type(overlap),dimension(3) :: NG      ! Ghost node
         type(overlap),dimension(3) :: CG      ! Ghost cell center
         type(overlap),dimension(3) :: NB      ! First interior node
         type(overlap),dimension(3) :: NI      ! First interior node
         type(overlap),dimension(3) :: CI      ! First interior cell center

         ! Data-location dependent overlaps
         type(overlap),dimension(3) :: phys_all

         type(overlap),dimension(3) :: OL_DL
         type(overlap),dimension(3) :: OL_face

         ! Overlap sizes
         integer,dimension(2) :: i_2D
         integer :: i_1D

         logical,dimension(3) :: defined = .false.
         real(cp),dimension(6) :: dh,nhat
         integer :: g_R1_id,g_R2_id
         integer,dimension(3) :: sn_R1,sn_R2
         real(cp),dimension(3) :: hmin_R1,hmin_R2,hmax_R1,hmax_R2
       end type

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_subdomain(SD,g_R1,g_R2,g_R1_id,g_R2_id)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_R1_id,g_R2_id
         integer :: i,j
         real(cp) :: tol
         tol = 10.0_cp**(-12.0_cp)
         call delete(SD)
         do i=1,3; call set_defined(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
         if (all(SD%defined)) then
           SD%g_R1_id = g_R1_id
           SD%g_R2_id = g_R2_id
           do i=1,3; call define_CI_all(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NI_all(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_CE_all(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NB_all(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NG(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_CG(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NB(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_CI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo

           SD%sn_R1 = (/(g_R1%c(j)%sn,j=1,3)/)
           SD%sn_R2 = (/(g_R2%c(j)%sn,j=1,3)/)
           SD%hmin_R1 = (/(g_R1%c(j)%hmin,j=1,3)/)
           SD%hmin_R2 = (/(g_R2%c(j)%hmin,j=1,3)/)
           SD%hmax_R1 = (/(g_R1%c(j)%hmax,j=1,3)/)
           SD%hmax_R2 = (/(g_R2%c(j)%hmax,j=1,3)/)
           do i=1,6; SD%nhat(i) = nhat_given_face(i); enddo
           do i=1,6; SD%dh(i) = get_dh_boundary((/g_R1,g_R2/),dir_given_face(i)); enddo
         endif
       end subroutine

       subroutine init_copy_subdomain(SD_out,SD_in)
         implicit none
         type(subdomain),intent(inout) :: SD_out
         type(subdomain),intent(in) :: SD_in
         integer :: i
         do i=1,3
           call init(SD_out%CI_all(i),SD_in%CI_all(i))
           call init(SD_out%NI_all(i),SD_in%NI_all(i))
           call init(SD_out%CE_all(i),SD_in%CE_all(i))
           call init(SD_out%NB_all(i),SD_in%NB_all(i))
           call init(SD_out%NG(i),SD_in%NG(i))
           call init(SD_out%CG(i),SD_in%CG(i))
           call init(SD_out%NB(i),SD_in%NB(i))
           call init(SD_out%NI(i),SD_in%NI(i))
           call init(SD_out%CI(i),SD_in%CI(i))
           call init(SD_out%phys_all(i),SD_in%phys_all(i))
         enddo
         SD_out%dh = SD_in%dh
         SD_out%nhat = SD_in%nhat
         SD_out%hmin_R1 = SD_in%hmin_R1
         SD_out%hmin_R2 = SD_in%hmin_R2
         SD_out%hmax_R1 = SD_in%hmax_R1
         SD_out%hmax_R2 = SD_in%hmax_R2
         SD_out%sn_R1 = SD_in%sn_R1
         SD_out%sn_R2 = SD_in%sn_R2
         SD_out%i_2D = SD_in%i_2D
         SD_out%i_1D = SD_in%i_1D
         SD_out%g_R1_id = SD_in%g_R1_id
         SD_out%g_R2_id = SD_in%g_R2_id
         SD_out%defined = SD_in%defined
       end subroutine

       subroutine delete_subdomain(SD)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer :: i
         do i=1,3
           call delete(SD%CI_all(i))
           call delete(SD%NI_all(i))
           call delete(SD%CE_all(i))
           call delete(SD%NB_all(i))
           call delete(SD%NG(i))
           call delete(SD%NB(i))
           call delete(SD%CG(i))
           call delete(SD%NI(i))
           call delete(SD%CI(i))
           call delete(SD%phys_all(i))
         enddo
         SD%dh = 0.0_cp
         SD%nhat = 0.0_cp
         SD%sn_R1 = 0
         SD%sn_R2 = 0
         SD%g_R1_id = 0
         SD%g_R2_id = 0
         SD%defined = .false.
         SD%hmin_R1 = 0.0_cp
         SD%hmin_R2 = 0.0_cp
         SD%hmax_R1 = 0.0_cp
         SD%hmax_R2 = 0.0_cp
       end subroutine

       subroutine display_subdomain(SD,name,u)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** Subdomain ************ '//name
         write(u,*) 'sn_R1 = ',SD%sn_R1
         write(u,*) 'sn_R2 = ',SD%sn_R2
         write(u,*) 'g_R1_id = ',SD%g_R1_id
         write(u,*) 'g_R2_id = ',SD%g_R2_id
         write(u,*) 'CI_all_i1(1) = ',(/(SD%CI_all(i)%i1(1),i=1,3)/)
         write(u,*) 'CI_all_i1(2) = ',(/(SD%CI_all(i)%i1(2),i=1,3)/)
         write(u,*) 'CI_all_i2(1) = ',(/(SD%CI_all(i)%i2(1),i=1,3)/)
         write(u,*) 'CI_all_i2(2) = ',(/(SD%CI_all(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NI_all_i1(1) = ',(/(SD%NI_all(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_all_i2(2) = ',(/(SD%NI_all(i)%i2(2),i=1,3)/)
         write(u,*) 'NI_all_i1(1) = ',(/(SD%NI_all(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_all_i2(2) = ',(/(SD%NI_all(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'CE_all_i1(1) = ',(/(SD%CE_all(i)%i1(1),i=1,3)/)
         write(u,*) 'CE_all_i2(2) = ',(/(SD%CE_all(i)%i2(2),i=1,3)/)
         write(u,*) 'CE_all_i1(1) = ',(/(SD%CE_all(i)%i1(1),i=1,3)/)
         write(u,*) 'CE_all_i2(2) = ',(/(SD%CE_all(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NB_all_i1(1) = ',(/(SD%NB_all(i)%i1(1),i=1,3)/)
         write(u,*) 'NB_all_i2(2) = ',(/(SD%NB_all(i)%i2(2),i=1,3)/)
         write(u,*) 'NB_all_i1(1) = ',(/(SD%NB_all(i)%i1(1),i=1,3)/)
         write(u,*) 'NB_all_i2(2) = ',(/(SD%NB_all(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NG_i1(1) = ',(/(SD%NG(i)%i1(1),i=1,3)/)
         write(u,*) 'NG_i1(2) = ',(/(SD%NG(i)%i1(2),i=1,3)/)
         write(u,*) 'NG_i2(1) = ',(/(SD%NG(i)%i2(1),i=1,3)/)
         write(u,*) 'NG_i2(2) = ',(/(SD%NG(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'CG_i1(1) = ',(/(SD%CG(i)%i1(1),i=1,3)/)
         write(u,*) 'CG_i1(2) = ',(/(SD%CG(i)%i1(2),i=1,3)/)
         write(u,*) 'CG_i2(1) = ',(/(SD%CG(i)%i2(1),i=1,3)/)
         write(u,*) 'CG_i2(2) = ',(/(SD%CG(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NB_i1(1) = ',(/(SD%NB(i)%i1(1),i=1,3)/)
         write(u,*) 'NB_i1(2) = ',(/(SD%NB(i)%i1(2),i=1,3)/)
         write(u,*) 'NB_i2(1) = ',(/(SD%NB(i)%i2(1),i=1,3)/)
         write(u,*) 'NB_i2(2) = ',(/(SD%NB(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NI_i1(1) = ',(/(SD%NI(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_i1(2) = ',(/(SD%NI(i)%i1(2),i=1,3)/)
         write(u,*) 'NI_i2(1) = ',(/(SD%NI(i)%i2(1),i=1,3)/)
         write(u,*) 'NI_i2(2) = ',(/(SD%NI(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'CI_i1(1) = ',(/(SD%CI(i)%i1(1),i=1,3)/)
         write(u,*) 'CI_i1(2) = ',(/(SD%CI(i)%i1(2),i=1,3)/)
         write(u,*) 'CI_i2(1) = ',(/(SD%CI(i)%i2(1),i=1,3)/)
         write(u,*) 'CI_i2(2) = ',(/(SD%CI(i)%i2(2),i=1,3)/)
         write(u,*) ''

         write(u,*) 'phys_all_i1(1) = ',(/(SD%phys_all(i)%i1(1),i=1,3)/)
         write(u,*) 'phys_all_i1(2) = ',(/(SD%phys_all(i)%i1(2),i=1,3)/)
         write(u,*) 'phys_all_i2(1) = ',(/(SD%phys_all(i)%i2(1),i=1,3)/)
         write(u,*) 'phys_all_i2(2) = ',(/(SD%phys_all(i)%i2(2),i=1,3)/)
         write(u,*) ''

         write(u,*) 'OL_DL(1) = ',(/(SD%OL_DL(i)%i2(1),i=1,3)/)
         write(u,*) 'OL_DL(2) = ',(/(SD%OL_DL(i)%i2(2),i=1,3)/)
         write(u,*) ''

         write(u,*) 'dh = ',SD%dh
         write(u,*) 'nhat = ',SD%nhat
         write(u,*) 'i_1D = ',SD%i_1D
         write(u,*) 'i_2D = ',SD%i_2D
         write(u,*) 'hmin_R1 = ',SD%hmin_R1
         write(u,*) 'hmax_R1 = ',SD%hmax_R1
         write(u,*) 'hmin_R2 = ',SD%hmin_R2
         write(u,*) 'hmax_R2 = ',SD%hmax_R2
         write(u,*) 'defined = ',SD%defined
         write(u,*) ' ********************************* '
       end subroutine

       subroutine print_subdomain(SD,name)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         call display(SD,name,6)
       end subroutine

       subroutine export_subdomain(SD,u)
         implicit none
         type(subdomain),intent(in) :: SD
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** Subdomain ************ '
         do i=1,3; call export(SD%CI_all(i),u); enddo
         do i=1,3; call export(SD%NI_all(i),u); enddo
         do i=1,3; call export(SD%CE_all(i),u); enddo
         do i=1,3; call export(SD%NB_all(i),u); enddo
         do i=1,3; call export(SD%NG(i),u); enddo
         do i=1,3; call export(SD%NB(i),u); enddo
         do i=1,3; call export(SD%CG(i),u); enddo
         do i=1,3; call export(SD%NI(i),u); enddo
         do i=1,3; call export(SD%CI(i),u); enddo
         do i=1,3; call export(SD%phys_all(i),u); enddo
         write(u,*) 'g_R1_id = '; write(u,*) SD%g_R1_id
         write(u,*) 'g_R2_id = '; write(u,*) SD%g_R2_id
         write(u,*) 'dh = ';      write(u,*) SD%dh
         write(u,*) 'nhat = ';    write(u,*) SD%nhat
         write(u,*) 'sn_R1 = ';   write(u,*) SD%sn_R1
         write(u,*) 'sn_R2 = ';   write(u,*) SD%sn_R2
         write(u,*) 'defined = '; write(u,*) SD%defined
         write(u,*) ' ********************************* '
       end subroutine

       subroutine import_subdomain(SD,u)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: u
         integer :: i
         read(u,*) 
         do i=1,3; call import(SD%CI_all(i),u); enddo
         do i=1,3; call import(SD%NI_all(i),u); enddo
         do i=1,3; call import(SD%CE_all(i),u); enddo
         do i=1,3; call import(SD%NB_all(i),u); enddo
         do i=1,3; call import(SD%NG(i),u); enddo
         do i=1,3; call import(SD%NB(i),u); enddo
         do i=1,3; call import(SD%CG(i),u); enddo
         do i=1,3; call import(SD%NI(i),u); enddo
         do i=1,3; call import(SD%CI(i),u); enddo
         do i=1,3; call import(SD%phys_all(i),u); enddo
         read(u,*); read(u,*) SD%g_R1_id
         read(u,*); read(u,*) SD%g_R2_id
         read(u,*); read(u,*) SD%dh
         read(u,*); read(u,*) SD%nhat
         read(u,*); read(u,*) SD%sn_R1
         read(u,*); read(u,*) SD%sn_R2
         read(u,*); read(u,*) SD%defined
         read(u,*)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine set_defined(SD,R1,R2,dir,tol)
         ! Checks for PHYSICAL overlap (defined by hmin / hmax, see coordinates.f90)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         SD%defined(dir) = is_overlap(R1,R2,tol)
       end subroutine

       ! *********************************************************************
       ! ************************* GENERAL OVERLAP ***************************
       ! *********************************************************************

       subroutine define_CI_all(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CI_all(dir),get_C_overlap((/R1,R2/),tol,'define_CI_all',0))
       end subroutine

       subroutine define_NI_all(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NI_all(dir),get_N_overlap((/R1,R2/),tol,'define_NI_all',0))
       end subroutine

       subroutine define_CE_all(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CE_all(dir),get_C_overlap((/R1,R2/),tol,'define_CE_all',1))
       end subroutine

       subroutine define_NB_all(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NB_all(dir),get_N_overlap((/R1,R2/),tol,'define_NB_all',1))
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

       ! *********************************************************************
       ! ************************* BOUNDARY CASES ****************************
       ! *********************************************************************

       subroutine define_NG(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NG(dir),get_p_from_boundary_N(SD%NI_all(dir),(/R1,R2/),tol,'define_NG',1))
       end subroutine

       subroutine define_CG(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CG(dir),get_p_from_boundary_C(SD%CI_all(dir),(/R1,R2/),tol,'define_CG',1))
       end subroutine

       subroutine define_NB(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NB(dir),get_p_from_boundary_N(SD%NI_all(dir),(/R1,R2/),tol,'define_NB',2))
       end subroutine

       subroutine define_NI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NI(dir),get_p_from_boundary_N(SD%NI_all(dir),(/R1,R2/),tol,'define_NI',3))
       end subroutine

       subroutine define_CI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CI(dir),get_p_from_boundary_C(SD%CI_all(dir),(/R1,R2/),tol,'define_CI',2))
       end subroutine

       ! **********************************************************************
       ! **********************************************************************
       ! **********************************************************************

       subroutine init_props_subdomain(SD,DL)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(data_location),intent(in) :: DL
         logical,dimension(3) :: L
         logical,dimension(5) :: cond
         integer :: i,C
         if (is_Face(DL)) then ! ENTIRE PHYSICAL OVERLAP
          do i=1,3; call init(SD%phys_all(i),SD%CE_all(i)); enddo
          call init(SD%phys_all(DL%face),SD%NB_all(DL%face))
         elseif (is_Edge(DL)) then
          do i=1,3; call init(SD%phys_all(i),SD%NB_all(i)); enddo
          call init(SD%phys_all(DL%edge),SD%CE_all(DL%edge))
         elseif (is_CC(DL)) then
          do i=1,3; call init(SD%phys_all(i),SD%CE_all(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(SD%phys_all(i),SD%NB_all(i)); enddo
         else; stop 'Error: no type found in init_props_subdomain in subdomain.f90'
         endif

         if (is_Face(DL)) then ! INTRA-GRID BOUNDARY OVERLAP
          do i=1,3; call init(SD%OL_DL(i),SD%CI_all(i)); enddo
          SD%OL_DL(DL%face) = SD%NG(DL%face)
         elseif (is_Edge(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%NG(i)); enddo
          call init(SD%OL_DL(DL%face),SD%CI_all(DL%face))
         elseif (is_CC(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%CI_all(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%NG(i)); enddo
         else; stop 'Error: no type found in init_props_subdomain in subdomain.f90'
         endif
 
         SD%i_2D = 0 ! default
         SD%i_1D = 0 ! default
         L = (/(SD%OL_DL(i)%iR.eq.1,i=1,3)/)
         C = count(L)
             if (C.eq.0) then ! 3D overlap
         elseif (C.eq.1) then ! 2D overlap
         if (L(1)) SD%i_2D = adj_dir_given_dir(1)
         if (L(2)) SD%i_2D = adj_dir_given_dir(2)
         if (L(3)) SD%i_2D = adj_dir_given_dir(3)
         elseif (C.eq.2) then ! 1D overlap
         if (L(1)) SD%i_1D = 1
         if (L(2)) SD%i_1D = 2
         if (L(3)) SD%i_1D = 3
         elseif (C.eq.3) then ! point overlap
         else; write(*,*) 'Error: bad case in init_props_subdomain in subdomain.f90'; stop 'Done'
         endif
         cond(1) = is_Face(DL)
         cond(2) = DL%face.eq.2
         cond(3) = SD%g_R1_id.eq.6
         cond(4) = same_point(SD%hmax_R1(3),1.0_cp)
         cond(5) = (SD%sn_R1(3).eq.2).or.(SD%sn_R2(3).eq.2)
         ! if (cond(3)) then
         !   call print(SD,'Face(face=2) data')
         !   stop 'Done in subdomain.f90'
         ! endif
       end subroutine

       end module