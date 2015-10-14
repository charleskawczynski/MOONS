       module subdomain_mod
       use grid_mod
       use coordinates_mod

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
       public :: print,export

       interface init;               module procedure init_subdomain;        end interface
       interface init;               module procedure init_subdomain_copy;   end interface
       interface delete;             module procedure delete_subdomain;      end interface

       interface print;              module procedure print_subdomain;       end interface
       interface export;             module procedure export_subdomain;       end interface

       type subdomain
         ! Legend:
         !        C = Cell Center
         !              E = exclude first exterior point
         !              I = include first exterior point
         !        N = Node
         !              B = include boundary point (for node data)
         !              I = include first exterior point
         !              E = exclude boundary point
         !        T = total domain (fluid, e.g.)
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

         logical,dimension(3) :: defined = .false.
         integer :: g_in_id,g_tot_id
       end type

       contains

       subroutine init_subdomain(SD,g_in,g_tot,g_in_id,g_tot_id)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: g_in,g_tot
         integer,intent(in) :: g_in_id,g_tot_id
         call delete(SD)
         call define_CE(SD,g_in%c(1),g_tot%c(1),1)
         call define_CE(SD,g_in%c(2),g_tot%c(2),2)
         call define_CE(SD,g_in%c(3),g_tot%c(3),3)
         if (all(SD%defined)) then
           call define_CI(SD,1)
           call define_CI(SD,2)
           call define_CI(SD,3)
           call define_N(SD,1)
           call define_N(SD,2)
           call define_N(SD,3)
           SD%g_in_id = g_in_id
           SD%g_tot_id = g_tot_id
         endif
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
         SD_out%g_in_id = SD_in%g_in_id
         SD_out%g_tot_id = SD_in%g_tot_id
         SD_out%defined = SD_in%defined
       end subroutine

       subroutine delete_subdomain(SD)
         implicit none
         type(subdomain),intent(inout) :: SD
         SD%CE1 = 0; SD%CE2 = 0; SD%TCE1 = 0; SD%TCE2 = 0
         SD%CI1 = 0; SD%CI2 = 0; SD%TCI1 = 0; SD%TCI2 = 0
         SD%NB1 = 0; SD%NB2 = 0; SD%TNB1 = 0; SD%TNB2 = 0
         SD%NI1 = 0; SD%NI2 = 0; SD%TNI1 = 0; SD%TNI2 = 0
         SD%NE1 = 0; SD%NE2 = 0; SD%TNE1 = 0; SD%TNE2 = 0
         SD%defined = .false.
       end subroutine

       subroutine define_CE(SD,c_in,c_tot,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: c_in,c_tot
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
         do i=1,c_in%sc-1
           before = c_in%hc( i ).lt.c_tot%hmin
           after  = c_in%hc(i+1).gt.c_tot%hmin
           if (before.and.after) SD%TCE1(dir) = i+1
         enddo
         do i=1,c_in%sc-1
           before = c_in%hc( i ).lt.c_tot%hmax
           after  = c_in%hc(i+1).gt.c_tot%hmax
           if (before.and.after) SD%TCE2(dir) = i
         enddo

          ! Note that in the following case is assumed
          ! to not occur:
          ! Case: better not happen
          ! 
          !    TCE1                            TCE2
          !     |-------------fluid---------------|
          !          |--------wall---------|
          !         CE1                    CE2
          ! 
         if ((SD%CE1(dir).ne.0).and.(SD%CE2(dir).ne.0)) then
           ! Case 1 (complete overlap of fluid)
           ! Traversing c_tot, both hmin and hmax are found in fluid
           ! 
           !         TCE1                  TCE2
           !          |--------fluid---------|
           !     |-------------wall---------------|
           !    CE1                              CE2
           ! 
           SD%defined(dir) = .true.
           SD%TCE1(dir) = 2; SD%TCE2(dir) = c_in%sc-1
           
           SD%TCI1(dir) = 1; SD%TCI2(dir) = c_in%sc
           SD%TNI1(dir) = 1; SD%TNI2(dir) = c_in%sn
           SD%TNB1(dir) = 2; SD%TNB2(dir) = c_in%sn-1
           SD%TNE1(dir) = 3; SD%TNE2(dir) = c_in%sn-2
         elseif ((SD%CE1(dir).eq.0).and.(SD%CE2(dir).eq.0)) then
           ! Case 3 (no overlap)
           !  |---------fluid---------|
           !                                 |---------wall---------|
           SD%defined(dir) = .false.
         elseif (SD%CE1(dir).eq.0) then
           ! Case 3 (partial overlap of fluid, fluid "enters" wall domain)
           ! Traversing c_tot and c_in, only CE2 and TCE1 are found
           ! 
           !                               TCE1     TCE2
           !  |----------------fluid------------------|
           !                                 |-------------wall---------------|
           !                                CE1      CE2
           ! 
           SD%defined(dir) = .true.

           SD%CE1(dir) = 2
           SD%TCE2(dir) = c_in%sc-1

           SD%TCI1(dir) = 1; SD%TCI2(dir) = c_in%sc
           SD%TNI1(dir) = 1; SD%TNI2(dir) = c_in%sn
           SD%TNB1(dir) = 2; SD%TNB2(dir) = c_in%sn-1
           SD%TNE1(dir) = 3; SD%TNE2(dir) = c_in%sn-2
         elseif (SD%CE2(dir).eq.0) then
           ! Case 2 (partial overlap of fluid, fluid "leaves" wall domain)
           ! Traversing c_tot and c_in, only CE1 and TCE2 are found
           ! 
           !                           TCE1     TCE2
           !                            |----------------fluid------------------|
           !     |-------------wall---------------|
           !                           CE1       CE2
           !
           SD%defined(dir) = .true.

           SD%CE2(dir) = c_tot%sc-1
           SD%TCE1(dir) = 2

           SD%TNI1(dir) = 1; SD%TNI2(dir) = c_in%sn
           SD%TNB1(dir) = 2; SD%TNB2(dir) = c_in%sn-1
           SD%TNE1(dir) = 3; SD%TNE2(dir) = c_in%sn-2
           SD%TCI1(dir) = 1; SD%TCI2(dir) = c_in%sc
         endif
       end subroutine

       subroutine define_CI(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%CI1(dir) = SD%CE1(dir)-1; SD%CI2(dir) = SD%CE2(dir)+1
       end subroutine

       subroutine define_N(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%NB1(dir) = SD%CE1(dir)
         SD%NB2(dir) = SD%CE2(dir)+1
         SD%NE1(dir) = SD%NB1(dir)+1
         SD%NE2(dir) = SD%NB2(dir)-1
         SD%NI1(dir) = SD%NB1(dir)-1
         SD%NI2(dir) = SD%NB2(dir)+1
       end subroutine

       subroutine print_subdomain(SD,name)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         call export(SD,name,6)
       end subroutine

       subroutine export_subdomain(SD,name,u)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         write(u,*) ' ********** Subdomain ************ '//name
         write(u,*) 'CE1 = ',SD%CE1
         write(u,*) 'CE2 = ',SD%CE2
         ! write(u,*) 'CI1 = ',SD%CI1
         ! write(u,*) 'CI2 = ',SD%CI2
         ! write(u,*) 'NB1 = ',SD%NB1
         ! write(u,*) 'NB2 = ',SD%NB2
         ! write(u,*) 'NI1 = ',SD%NI1
         ! write(u,*) 'NI2 = ',SD%NI2
         ! write(u,*) 'NE1 = ',SD%NE1
         ! write(u,*) 'NE2 = ',SD%NE2
         write(u,*) 'TCE1 = ',SD%TCE1
         write(u,*) 'TCE2 = ',SD%TCE2
         ! write(u,*) 'TCI1 = ',SD%TCI1
         ! write(u,*) 'TCI2 = ',SD%TCI2
         ! write(u,*) 'TNB1 = ',SD%TNB1
         ! write(u,*) 'TNB2 = ',SD%TNB2
         ! write(u,*) 'TNI1 = ',SD%TNI1
         ! write(u,*) 'TNI2 = ',SD%TNI2
         ! write(u,*) 'TNE1 = ',SD%TNE1
         ! write(u,*) 'TNE2 = ',SD%TNE2
         write(u,*) 'g_in_id = ',SD%g_in_id
         write(u,*) 'g_tot_id = ',SD%g_tot_id
         write(u,*) 'defined = ',SD%defined
         write(u,*) ' ********************************* '
       end subroutine

       end module