       module subdomain_mod
       use current_precision_mod
       use overlap_mod
       use grid_mod
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

         type(overlap),dimension(3) :: CE
         type(overlap),dimension(3) :: CI
         type(overlap),dimension(3) :: NB
         type(overlap),dimension(3) :: NI

         type(overlap),dimension(3) :: OL_DL
         integer,dimension(2) :: i_2D
         integer :: i_1D

         logical,dimension(3) :: defined = .false.
         integer :: g_R1_id,g_R2_id
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
         integer :: i
         real(cp) :: tol
         tol = 10.0_cp**(-12.0_cp)
         call delete(SD)
         do i=1,3; call set_defined(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
         if (all(SD%defined)) then
           do i=1,3; call define_CE(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_CI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NB(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           do i=1,3; call define_NI(SD,g_R1%c(i),g_R2%c(i),i,tol); enddo
           SD%g_R1_id = g_R1_id
           SD%g_R2_id = g_R2_id
           do i=1,3; call init_props(SD%CE(i)); enddo
           do i=1,3; call init_props(SD%CI(i)); enddo
           do i=1,3; call init_props(SD%NB(i)); enddo
           do i=1,3; call init_props(SD%NI(i)); enddo
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
           call init(SD_out%NB(i),SD_in%NB(i))
           call init(SD_out%NI(i),SD_in%NI(i))
         enddo
         SD_out%OL_DL = SD_in%OL_DL
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
         enddo
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
         write(u,*) 'CE_i1(1) = ',(/(SD%CE(i)%i1(1),i=1,3)/)
         write(u,*) 'CE_i1(2) = ',(/(SD%CE(i)%i1(2),i=1,3)/)
         write(u,*) 'CI_i1(1) = ',(/(SD%CI(i)%i1(1),i=1,3)/)
         write(u,*) 'CI_i1(2) = ',(/(SD%CI(i)%i1(2),i=1,3)/)
         write(u,*) 'NB_i1(1) = ',(/(SD%NB(i)%i1(1),i=1,3)/)
         write(u,*) 'NB_i1(2) = ',(/(SD%NB(i)%i1(2),i=1,3)/)
         write(u,*) 'NI_i1(1) = ',(/(SD%NI(i)%i1(1),i=1,3)/)
         write(u,*) 'NI_i1(2) = ',(/(SD%NI(i)%i1(2),i=1,3)/)

         write(u,*) ''
         write(u,*) 'CE_i2(1) = ',(/(SD%CE(i)%i2(1),i=1,3)/)
         write(u,*) 'CE_i2(2) = ',(/(SD%CE(i)%i2(2),i=1,3)/)
         write(u,*) 'CI_i2(1) = ',(/(SD%CI(i)%i2(1),i=1,3)/)
         write(u,*) 'CI_i2(2) = ',(/(SD%CI(i)%i2(2),i=1,3)/)
         write(u,*) 'NB_i2(1) = ',(/(SD%NB(i)%i2(1),i=1,3)/)
         write(u,*) 'NB_i2(2) = ',(/(SD%NB(i)%i2(2),i=1,3)/)
         write(u,*) 'NI_i2(1) = ',(/(SD%NI(i)%i2(1),i=1,3)/)
         write(u,*) 'NI_i2(2) = ',(/(SD%NI(i)%i2(2),i=1,3)/)

         write(u,*) ''
         write(u,*) 'OL_DL(1) = ',(/(SD%OL_DL(i)%i2(1),i=1,3)/)
         write(u,*) 'OL_DL(2) = ',(/(SD%OL_DL(i)%i2(2),i=1,3)/)
         write(u,*) 'g_R1_id = ',SD%g_R1_id
         write(u,*) 'g_R2_id = ',SD%g_R2_id
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
         write(u,*) 'g_R1_id = '; write(u,*) SD%g_R1_id
         write(u,*) 'g_R2_id = '; write(u,*) SD%g_R2_id
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
         read(u,*); read(u,*) SD%g_R1_id
         read(u,*); read(u,*) SD%g_R2_id
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

       subroutine define_NI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%NI(dir)%i1(1) = 1; SD%NI(dir)%i1(2) = R1%sn
         SD%NI(dir)%i2(1) = 1; SD%NI(dir)%i2(2) = R2%sn
         ! Look for overlap elsewhere:
         do i=R1%sn,1,-1;if (inside(R1%hn(i),R2%amin,R2%amax,tol)) SD%NI(dir)%i1(1)=i;enddo
         do i=1,R1%sn   ;if (inside(R1%hn(i),R2%amin,R2%amax,tol)) SD%NI(dir)%i1(2)=i;enddo
         do i=R2%sn,1,-1;if (inside(R2%hn(i),R1%amin,R1%amax,tol)) SD%NI(dir)%i2(1)=i;enddo
         do i=1,R2%sn   ;if (inside(R2%hn(i),R1%amin,R1%amax,tol)) SD%NI(dir)%i2(2)=i;enddo
       end subroutine

       subroutine define_CI(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%CI(dir)%i1(1) = 1; SD%CI(dir)%i1(2) = R1%sc
         SD%CI(dir)%i2(1) = 1; SD%CI(dir)%i2(2) = R2%sc
         ! Look for overlap elsewhere:
         do i=R1%sc,1,-1;if (inside(R1%hc(i),R2%amin,R2%amax,tol)) SD%CI(dir)%i1(1)=i;enddo
         do i=1,R1%sc   ;if (inside(R1%hc(i),R2%amin,R2%amax,tol)) SD%CI(dir)%i1(2)=i;enddo
         do i=R2%sc,1,-1;if (inside(R2%hc(i),R1%amin,R1%amax,tol)) SD%CI(dir)%i2(1)=i;enddo
         do i=1,R2%sc   ;if (inside(R2%hc(i),R1%amin,R1%amax,tol)) SD%CI(dir)%i2(2)=i;enddo
       end subroutine

       subroutine define_NB(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%NB(dir)%i1(1) = 1; SD%NB(dir)%i1(2) = R1%sn
         SD%NB(dir)%i2(1) = 1; SD%NB(dir)%i2(2) = R2%sn
         ! Look for overlap elsewhere, SHOULD BE TRUE:
         do i=R1%sn-1,2,-1;if (inside(R1%hn(i),R2%hmin,R2%hmax,tol)) SD%NB(dir)%i1(1)=i;enddo
         do i=2,R1%sn-1   ;if (inside(R1%hn(i),R2%hmin,R2%hmax,tol)) SD%NB(dir)%i1(2)=i;enddo
         do i=R2%sn-1,2,-1;if (inside(R2%hn(i),R1%hmin,R1%hmax,tol)) SD%NB(dir)%i2(1)=i;enddo
         do i=2,R2%sn-1   ;if (inside(R2%hn(i),R1%hmin,R1%hmax,tol)) SD%NB(dir)%i2(2)=i;enddo
       end subroutine

       subroutine define_CE(SD,R1,R2,dir,tol)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: R1,R2
         integer,intent(in) :: dir
         real(cp),intent(in) :: tol
         integer :: i
         ! Overlap is end-to-end unless found elsewhere:
         SD%CE(dir)%i1(1) = 2; SD%CE(dir)%i1(2) = R1%sc-1
         SD%CE(dir)%i2(1) = 2; SD%CE(dir)%i2(2) = R2%sc-1
         ! Look for overlap elsewhere:
         do i=R1%sc-1,2,-1;if (inside(R1%hc(i),R2%hmin,R2%hmax,tol)) SD%CE(dir)%i1(1)=i;enddo
         do i=2,R1%sc-1   ;if (inside(R1%hc(i),R2%hmin,R2%hmax,tol)) SD%CE(dir)%i1(2)=i;enddo
         do i=R2%sc-1,2,-1;if (inside(R2%hc(i),R1%hmin,R1%hmax,tol)) SD%CE(dir)%i2(1)=i;enddo
         do i=2,R2%sc-1   ;if (inside(R2%hc(i),R1%hmin,R1%hmax,tol)) SD%CE(dir)%i2(2)=i;enddo
       end subroutine

       subroutine init_props_subdomain(SD,DL)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(data_location),intent(in) :: DL
         logical,dimension(3) :: L
         integer :: i,C
         if (is_Face(DL)) then
           select case (DL%face)
           case (1); SD%OL_DL = (/SD%NB(1),SD%CI(2),SD%CI(3)/)
           case (2); SD%OL_DL = (/SD%CI(1),SD%NB(2),SD%CI(3)/)
           case (3); SD%OL_DL = (/SD%CI(1),SD%CI(2),SD%NB(3)/)
           case default; stop 'Error: f%face must = 1,2,3 in init_props_subdomain in subdomain.f90'
           end select
         elseif (is_Edge(DL)) then
           select case (DL%edge)
           case (1); SD%OL_DL = (/SD%CI(1),SD%NB(2),SD%NB(3)/)
           case (2); SD%OL_DL = (/SD%NB(1),SD%CI(2),SD%NB(3)/)
           case (3); SD%OL_DL = (/SD%NB(1),SD%NB(2),SD%CI(3)/)
           case default; stop 'Error: f%edge must = 1,2,3 in init_props_subdomain in subdomain.f90'
           end select
         elseif (is_CC(DL)) then
           SD%OL_DL = (/SD%CI(1),SD%CI(2),SD%CI(3)/)
         elseif (is_Node(DL)) then
           SD%OL_DL = (/SD%NB(1),SD%NB(2),SD%NB(3)/)
         else; stop 'Error: no type found in init_props_subdomain in subdomain.f90'
         endif

         SD%i_2D = 0 ! default
         SD%i_1D = 0 ! default
         L = (/(SD%OL_DL(i)%iR.eq.1,i=1,3)/)
         C = count(L)
             if (C.eq.0) then ! 3D overlap
         elseif (C.eq.1) then ! 2D overlap
         if (L(1)) SD%i_2D = (/2,3/)
         if (L(2)) SD%i_2D = (/1,3/)
         if (L(3)) SD%i_2D = (/1,2/)
         elseif (C.eq.2) then ! 1D overlap
         if (L(1)) SD%i_1D = 1
         if (L(2)) SD%i_1D = 2
         if (L(3)) SD%i_1D = 3
         elseif (C.eq.3) then ! point overlap
         else; write(*,*) 'Error: bad case in init_props_subdomain in subdomain.f90'; stop 'Done'
         endif
       end subroutine

       end module