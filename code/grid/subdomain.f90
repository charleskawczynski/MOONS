       module subdomain_mod
       use grid_mod
       use SF_mod
       use VF_mod

       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: subdomain
       public :: init,delete

       interface init;               module procedure init_subdomain;        end interface
       interface init;               module procedure init_subdomain_copy;   end interface
       interface delete;             module procedure delete_subdomain;      end interface

       type subdomain
         ! Legend:
         !        C = Cell Center
         !              E = exclude first exterior point
         !              I = include first exterior point
         !        N = Node
         !              B = include boundary point (for node data)
         !              I = include first exterior point
         !              E = exclude boundary point
         !        T = total domain, follow above legend following T
         ! 
         integer,dimension(3) :: CE1,CE2
         integer,dimension(3) :: CI1,CI2
         integer,dimension(3) :: NB1,NB2
         integer,dimension(3) :: NI1,NI2
         integer,dimension(3) :: NE1,NE2

         integer,dimension(3) :: TCE1,TCE2
         integer,dimension(3) :: TCI1,TCI2
         integer,dimension(3) :: TNB1,TNB2
         integer,dimension(3) :: TNI1,TNI2
         integer,dimension(3) :: TNE1,TNE2 ! Excludes wall normal values (momentum eq)

         integer,dimension(3) :: out1,out2
         integer,dimension(3) :: in1,in2
         type(grid) :: g_in,g_tot
       end type

       contains

       subroutine init_subdomain(SD,g_in,g_tot)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: g_in,g_tot
         call define_CE(SD,g_in%c(1),g_tot%c(1),1)
         call define_CE(SD,g_in%c(2),g_tot%c(2),2)
         call define_CE(SD,g_in%c(3),g_tot%c(3),3)
         call define_CI(SD,1)
         call define_CI(SD,2)
         call define_CI(SD,3)
         call define_T(SD,1)
         call define_T(SD,2)
         call define_T(SD,3)
         call define_N(SD)
         call init(SD%g_in,g_in)
         call init(SD%g_tot,g_tot)
       end subroutine

       subroutine init_subdomain_copy(SD_out,SD_in)
         implicit none
         type(subdomain),intent(inout) :: SD_out
         type(subdomain),intent(in) :: SD_in
         SD_out%CE1 = SD_in%CE1; SD_out%CE2 = SD_in%CE2
         SD_out%CI1 = SD_in%CI1; SD_out%CI2 = SD_in%CI2
         SD_out%NB1 = SD_in%NB1; SD_out%NB2 = SD_in%NB2
         SD_out%NI1 = SD_in%NI1; SD_out%NI2 = SD_in%NI2
         SD_out%NE1 = SD_in%NE1; SD_out%NE2 = SD_in%NE2
         SD_out%TCE1 = SD_in%TCE1; SD_out%TCE2 = SD_in%TCE2
         SD_out%TCI1 = SD_in%TCI1; SD_out%TCI2 = SD_in%TCI2
         SD_out%TNB1 = SD_in%TNB1; SD_out%TNB2 = SD_in%TNB2
         SD_out%TNI1 = SD_in%TNI1; SD_out%TNI2 = SD_in%TNI2
         SD_out%TNE1 = SD_in%TNE1; SD_out%TNE2 = SD_in%TNE2
         call init(SD_out%g_in,SD_in%g_in)
         call init(SD_out%g_tot,SD_in%g_tot)
       end subroutine

       subroutine delete_subdomain(SD)
         implicit none
         type(subdomain),intent(inout) :: SD
         SD%CE1 = 0; SD%CE2 = 0
         SD%CI1 = 0; SD%CI2 = 0
         SD%NB1 = 0; SD%NB2 = 0
         SD%NI1 = 0; SD%NI2 = 0
         SD%NE1 = 0; SD%NE2 = 0
         SD%TCE1 = 0; SD%TCE2 = 0
         SD%TCI1 = 0; SD%TCI2 = 0
         SD%TNB1 = 0; SD%TNB2 = 0
         SD%TNI1 = 0; SD%TNI2 = 0
         SD%TNE1 = 0; SD%TNE2 = 0
         call delete(SD%g_in)
         call delete(SD%g_tot)
       end subroutine

       subroutine define_CE(SD,c_in,c_tot,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: c_in,c_tot
         integer,intent(in) :: dir
         logical :: before,after
         integer :: i
         SD%CE1(dir) = 0
         SD%CE2(dir) = 0
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmin
           after  = c_tot%hc(i+1).gt.c_in%hmin
           if (before.and.after) SD%CE1(dir) = i+1
         enddo
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmax
           after  = c_tot%hc(i+1).gt.c_in%hmax
           if (before.and.after) SD%CE2(dir) = i
         enddo
         if (SD%CE1(dir).eq.0) stop 'Error: CE1 not defined in subdomain.f90'
         if (SD%CE2(dir).eq.0) stop 'Error: CE2 not defined in subdomain.f90'
       end subroutine

       subroutine define_CI(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%CI1(dir) = SD%CE1(dir)-1
         SD%CI2(dir) = SD%CE2(dir)+1
       end subroutine

       subroutine define_N(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%N1(dir) = SD%CE1(dir)-1
         SD%N2(dir) = SD%CE2(dir)+1
         SD%NE1(dir) = SD%N1(dir)+1
         SD%NE2(dir) = SD%N2(dir)-1
       end subroutine

       subroutine define_T(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%TN1(dir) = 2
         SD%TNE1(dir) = SD%TN1(dir)+1
         SD%CE1(dir) = 3
         SD%CI1(dir) = 2
         SD%TN2(dir)  = SD%g_tot%c(dir)sn-1
         SD%TNE2(dir) = SD%TN2(dir)-1
         SD%TCI2(dir) = SD%g_tot%c(dir)sc-1
         SD%TCE2(dir) = SD%g_tot%c(dir)sc-2
       end subroutine

       end module