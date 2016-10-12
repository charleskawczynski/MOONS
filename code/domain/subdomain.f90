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

         type(overlap),dimension(3) :: CE ! For inter-grid overlap
         type(overlap),dimension(3) :: CI ! For inter-grid overlap
         type(overlap),dimension(3) :: NB ! For inter-grid overlap
         type(overlap),dimension(3) :: NI ! For inter-grid overlap
         type(overlap),dimension(3) :: NI_only ! For intra-grid overlap
         type(overlap),dimension(3) :: NG_only ! For intra-grid overlap

         type(overlap),dimension(3) :: OL_DL,OL_phys
         integer,dimension(2) :: i_2D
         integer :: i_1D

         logical,dimension(3) :: defined = .false.
         integer :: g_R1_id,g_R2_id
         integer,dimension(3) :: sn_R1,sn_R2
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
           do i=1,3; call define_CI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_CE(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NB(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NI_only(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NG_only(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           SD%sn_R1 = (/(g_R1%c(j)%sn,j=1,3)/)
           SD%sn_R2 = (/(g_R2%c(j)%sn,j=1,3)/)
         endif
       end subroutine

       subroutine init_copy_subdomain(SD_out,SD_in)
         implicit none
         type(subdomain),intent(inout) :: SD_out
         type(subdomain),intent(in) :: SD_in
         integer :: i
         do i=1,3
           call init(SD_out%CE(i),SD_in%CE(i))
           call init(SD_out%CI(i),SD_in%CI(i))
           call init(SD_out%NI(i),SD_in%NI(i))
           call init(SD_out%NB(i),SD_in%NB(i))
           call init(SD_out%NI_only(i),SD_in%NI_only(i))
           call init(SD_out%NG_only(i),SD_in%NG_only(i))
           call init(SD_out%OL_DL(i),SD_in%OL_DL(i))
           call init(SD_out%OL_phys(i),SD_in%OL_phys(i))
         enddo
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
           call delete(SD%CE(i))
           call delete(SD%CI(i))
           call delete(SD%NB(i))
           call delete(SD%NI(i))
           call delete(SD%NI_only(i))
           call delete(SD%NG_only(i))
           call delete(SD%OL_DL(i))
           call delete(SD%OL_phys(i))
         enddo
         SD%sn_R1 = 0
         SD%sn_R2 = 0
         SD%g_R1_id = 0
         SD%g_R2_id = 0
         SD%defined = .false.
       end subroutine

       subroutine display_subdomain(SD,name,u)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** Subdomain ************ '//name
         write(u,*) 'CI_i1(1) = ',(/(SD%CI(i)%i1(1),i=1,3)/)
         write(u,*) 'CI_i1(2) = ',(/(SD%CI(i)%i1(2),i=1,3)/)
         write(u,*) 'NI_i1(1) = ',(/(SD%NI(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_i1(2) = ',(/(SD%NI(i)%i1(2),i=1,3)/)
         write(u,*) 'CE_i1(1) = ',(/(SD%CE(i)%i1(1),i=1,3)/)
         write(u,*) 'CE_i1(2) = ',(/(SD%CE(i)%i1(2),i=1,3)/)
         write(u,*) 'NB_i1(1) = ',(/(SD%NB(i)%i1(1),i=1,3)/)
         write(u,*) 'NB_i1(2) = ',(/(SD%NB(i)%i1(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'CI_i2(1) = ',(/(SD%CI(i)%i2(1),i=1,3)/)
         write(u,*) 'CI_i2(2) = ',(/(SD%CI(i)%i2(2),i=1,3)/)
         write(u,*) 'NI_i2(1) = ',(/(SD%NI(i)%i2(1),i=1,3)/)
         write(u,*) 'NI_i2(2) = ',(/(SD%NI(i)%i2(2),i=1,3)/)
         write(u,*) 'CE_i2(1) = ',(/(SD%CE(i)%i2(1),i=1,3)/)
         write(u,*) 'CE_i2(2) = ',(/(SD%CE(i)%i2(2),i=1,3)/)
         write(u,*) 'NB_i2(1) = ',(/(SD%NB(i)%i2(1),i=1,3)/)
         write(u,*) 'NB_i2(2) = ',(/(SD%NB(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NI_only_i1(1) = ',(/(SD%NI_only(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_only_i1(2) = ',(/(SD%NI_only(i)%i1(2),i=1,3)/)
         write(u,*) 'NG_only_i1(1) = ',(/(SD%NG_only(i)%i1(1),i=1,3)/)
         write(u,*) 'NG_only_i1(2) = ',(/(SD%NG_only(i)%i1(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'NI_only_i2(1) = ',(/(SD%NI_only(i)%i2(1),i=1,3)/)
         write(u,*) 'NI_only_i2(2) = ',(/(SD%NI_only(i)%i2(2),i=1,3)/)
         write(u,*) 'NG_only_i2(1) = ',(/(SD%NG_only(i)%i2(1),i=1,3)/)
         write(u,*) 'NG_only_i2(2) = ',(/(SD%NG_only(i)%i2(2),i=1,3)/)
         write(u,*) ''
         write(u,*) 'OL_DL(1) = ',(/(SD%OL_DL(i)%i2(1),i=1,3)/)
         write(u,*) 'OL_DL(2) = ',(/(SD%OL_DL(i)%i2(2),i=1,3)/)
         write(u,*) 'OL_phys(1) = ',(/(SD%OL_phys(i)%i2(1),i=1,3)/)
         write(u,*) 'OL_phys(2) = ',(/(SD%OL_phys(i)%i2(2),i=1,3)/)

         write(u,*) ''
         write(u,*) 'SD%NB(3)%i1 = ',SD%NB(3)%i1
         write(u,*) 'SD%NB(3)%i2 = ',SD%NB(3)%i2
         write(u,*) ''
         write(u,*) 'OL_DL_iR = ',(/(SD%OL_DL(i)%iR,i=1,3)/)
         write(u,*) 'OL_phys_iR = ',(/(SD%OL_phys(i)%iR,i=1,3)/)
         write(u,*) 'i_1D = ',SD%i_1D
         write(u,*) 'i_2D = ',SD%i_2D
         write(u,*) 'g_R1_id = ',SD%g_R1_id
         write(u,*) 'g_R2_id = ',SD%g_R2_id
         write(u,*) 'sn_R1 = ',SD%sn_R1
         write(u,*) 'sn_R2 = ',SD%sn_R2
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
         do i=1,3; call export(SD%CE(i),u); enddo
         do i=1,3; call export(SD%CI(i),u); enddo
         do i=1,3; call export(SD%NB(i),u); enddo
         do i=1,3; call export(SD%NI(i),u); enddo
         do i=1,3; call export(SD%NI_only(i),u); enddo
         do i=1,3; call export(SD%NG_only(i),u); enddo
         do i=1,3; call export(SD%OL_DL(i),u); enddo
         do i=1,3; call export(SD%OL_phys(i),u); enddo
         write(u,*) 'g_R1_id = '; write(u,*) SD%g_R1_id
         write(u,*) 'g_R2_id = '; write(u,*) SD%g_R2_id
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
         do i=1,3; call import(SD%CE(i),u); enddo
         do i=1,3; call import(SD%CI(i),u); enddo
         do i=1,3; call import(SD%NB(i),u); enddo
         do i=1,3; call import(SD%NI(i),u); enddo
         do i=1,3; call import(SD%NI_only(i),u); enddo
         do i=1,3; call import(SD%NG_only(i),u); enddo
         do i=1,3; call import(SD%OL_DL(i),u); enddo
         do i=1,3; call import(SD%OL_phys(i),u); enddo
         read(u,*); read(u,*) SD%g_R1_id
         read(u,*); read(u,*) SD%g_R2_id
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

       subroutine define_NI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NI(dir),get_N_overlap((/R1,R2/),tol,'define_NI',0))
       end subroutine

       subroutine define_CI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CI(dir),get_C_overlap((/R1,R2/),tol,'define_CI',0))
       end subroutine

       subroutine define_NB(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NB(dir),get_N_overlap((/R1,R2/),tol,'define_NB',1))
       end subroutine

       subroutine define_CE(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%CE(dir),get_C_overlap((/R1,R2/),tol,'define_CE',1))
       end subroutine

       ! *********************************************************************
       ! ************************** SPECIAL CASES ****************************
       ! *********************************************************************

       subroutine define_NG_only(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         call init(SD%NG_only(dir),get_N_overlap((/R1,R2/),tol,'define_NB',0))
       end subroutine

       subroutine define_NG_only_old(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%NG_only(dir)%i1(1) = 1; SD%NG_only(dir)%i1(2) = R1%sn
         SD%NG_only(dir)%i2(1) = 1; SD%NG_only(dir)%i2(2) = R2%sn
         ! Look for overlap elsewhere, SHOULD BE TRUE:
         ! Modify both i(1) and i(2) since ghost is only 1 node.
         do i=R1%sn,1,-1
         if (inside(R1%hn(i),R2%amin,R2%amax,tol)) then
         SD%NG_only(dir)%i1(1)=i
         endif
         enddo
         do i=1,R1%sn;  
         if (inside(R1%hn(i),R2%amin,R2%amax,tol)) then
         SD%NG_only(dir)%i1(2)=i
         endif
         enddo
         do i=R2%sn,1,-1
         if (inside(R2%hn(i),R1%amin,R1%amax,tol)) then
         SD%NG_only(dir)%i2(1)=i
         endif
         enddo
         do i=1,R2%sn;  
         if (inside(R2%hn(i),R1%amin,R1%amax,tol)) then
         SD%NG_only(dir)%i2(2)=i
         endif
         enddo
         call init_props(SD%NG_only(dir),'define_NG_only')
       end subroutine

       subroutine define_NI_only(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%NI_only(dir)%i1(1) = 1; SD%NI_only(dir)%i1(2) = R1%sn
         SD%NI_only(dir)%i2(1) = 1; SD%NI_only(dir)%i2(2) = R2%sn
         ! Look for overlap elsewhere, SHOULD BE TRUE:
         ! Modify both i(1) and i(2) since ghost is only 1 node.
         do i=R1%sn,1,-1
         if (inside(R1%hn(i),R2%amin,R2%amax,tol)) then
         SD%NI_only(dir)%i1(1)=i
         endif
         enddo
         do i=1,R1%sn;  
         if (inside(R1%hn(i),R2%amin,R2%amax,tol)) then
         SD%NI_only(dir)%i1(2)=i
         endif
         enddo
         do i=R2%sn,1,-1
         if (inside(R2%hn(i),R1%amin,R1%amax,tol)) then
         SD%NI_only(dir)%i2(1)=i
         endif
         enddo
         do i=1,R2%sn;  
         if (inside(R2%hn(i),R1%amin,R1%amax,tol)) then
         SD%NI_only(dir)%i2(2)=i
         endif
         enddo
         call init_props(SD%NI_only(dir),'define_NI_only')
       end subroutine

       ! **********************************************************************
       ! **********************************************************************
       ! **********************************************************************

       subroutine init_props_subdomain(SD,DL)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(data_location),intent(in) :: DL
         logical,dimension(3) :: L
         integer :: i,C
         if (is_Face(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%CI(i)); enddo
          SD%OL_DL(DL%face) = SD%NG_only(DL%face)
         elseif (is_Edge(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%NG_only(i)); enddo
          call init(SD%OL_DL(DL%face),SD%CI(DL%face))
         elseif (is_CC(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%CI(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(SD%OL_DL(i),SD%NG_only(i)); enddo
         else; stop 'Error: no type found in init_props_subdomain in subdomain.f90'
         endif
 
         if (is_Face(DL)) then
          do i=1,3; call init(SD%OL_phys(i),SD%CE(i)); enddo
          call init(SD%OL_phys(DL%face),SD%NB(DL%face))
         elseif (is_Edge(DL)) then
          do i=1,3; call init(SD%OL_phys(i),SD%NB(i)); enddo
          call init(SD%OL_phys(DL%edge),SD%CE(DL%edge))
         elseif (is_CC(DL)) then
          do i=1,3; call init(SD%OL_phys(i),SD%CE(i)); enddo
         elseif (is_Node(DL)) then
          do i=1,3; call init(SD%OL_phys(i),SD%NB(i)); enddo
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
         if (is_Face(DL).and.(DL%face.eq.2).and.(SD%g_R1_id.eq.5)) then
           call print(SD,'Face(face=2) data')
           stop 'Done in subdomain.f90'
         endif
       end subroutine

       end module