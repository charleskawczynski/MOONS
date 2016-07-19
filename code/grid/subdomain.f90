       module subdomain_mod
       use current_precision_mod
       use overlap_mod
       use grid_mod
       use coordinates_mod

       implicit none

       private
       public :: subdomain
       public :: init,delete
       public :: export,import
       public :: print,display

       interface init;       module procedure init_subdomain;        end interface
       interface init;       module procedure init_copy_subdomain;   end interface
       interface delete;     module procedure delete_subdomain;      end interface
       interface print;      module procedure print_subdomain;       end interface
       interface display;    module procedure display_subdomain;     end interface

       interface export;     module procedure export_subdomain;      end interface
       interface import;     module procedure import_subdomain;      end interface

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

         type(overlap),dimension(3) :: CE
         type(overlap),dimension(3) :: CI
         type(overlap),dimension(3) :: NB
         type(overlap),dimension(3) :: NI
         type(overlap),dimension(3) :: NE

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
           call init(SD_out%NE(i),SD_in%NE(i))
         enddo
         SD_out%g_in_id = SD_in%g_in_id
         SD_out%g_tot_id = SD_in%g_tot_id
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
           call delete(SD%NE(i))
         enddo
         SD%defined = .false.
       end subroutine

       subroutine define_CE(SD,c_in,c_tot,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(coordinates),intent(in) :: c_in,c_tot
         integer,intent(in) :: dir
         logical :: before,after
         integer :: i
         SD%CE(dir)%R1(1) = 0
         SD%CE(dir)%R2(1) = 0
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmin
           after  = c_tot%hc(i+1).gt.c_in%hmin
           if (before.and.after) SD%CE(dir)%R1(1) = i+1
         enddo
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmax
           after  = c_tot%hc(i+1).gt.c_in%hmax
           if (before.and.after) SD%CE(dir)%R2(1) = i
         enddo
         do i=1,c_in%sc-1
           before = c_in%hc( i ).lt.c_tot%hmin
           after  = c_in%hc(i+1).gt.c_tot%hmin
           if (before.and.after) SD%CE(dir)%R1(2) = i+1
         enddo
         do i=1,c_in%sc-1
           before = c_in%hc( i ).lt.c_tot%hmax
           after  = c_in%hc(i+1).gt.c_tot%hmax
           if (before.and.after) SD%CE(dir)%R2(2) = i
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
         if ((SD%CE(dir)%R1(1).ne.0).and.(SD%CE(dir)%R2(1).ne.0)) then
           ! Case 1 (complete overlap of fluid)
           ! Traversing c_tot, both hmin and hmax are found in fluid
           ! 
           !         TCE1                  TCE2
           !          |--------fluid---------|
           !     |-------------wall---------------|
           !    CE1                              CE2
           ! 
           SD%defined(dir) = .true.
           SD%CE(dir)%R1(2) = 2; SD%CE(dir)%R2(2) = c_in%sc-1
           
           SD%CI(dir)%R1(2) = 1; SD%CI(dir)%R2(2) = c_in%sc
           SD%NI(dir)%R1(2) = 1; SD%NI(dir)%R2(2) = c_in%sn
           SD%NB(dir)%R1(2) = 2; SD%NB(dir)%R2(2) = c_in%sn-1
           SD%NE(dir)%R1(2) = 3; SD%NE(dir)%R2(2) = c_in%sn-2
         elseif ((SD%CE(dir)%R1(1).eq.0).and.(SD%CE(dir)%R2(1).eq.0)) then
           ! Case 3 (no overlap)
           !  |---------fluid---------|
           !                                 |---------wall---------|
           SD%defined(dir) = .false.
         elseif (SD%CE(dir)%R1(1).eq.0) then
           ! Case 3 (partial overlap of fluid, fluid "enters" wall domain)
           ! Traversing c_tot and c_in, only CE2 and TCE1 are found
           ! 
           !                               TCE1     TCE2
           !  |----------------fluid------------------|
           !                                 |-------------wall---------------|
           !                                CE1      CE2
           ! 
           SD%defined(dir) = .true.

           SD%CE(dir)%R1(1) = 2
           SD%CE(dir)%R2(2) = c_in%sc-1

           SD%CI(dir)%R1(2) = 1; SD%CI(dir)%R2(2) = c_in%sc
           SD%NI(dir)%R1(2) = 1; SD%NI(dir)%R2(2) = c_in%sn
           SD%NB(dir)%R1(2) = 2; SD%NB(dir)%R2(2) = c_in%sn-1
           SD%NE(dir)%R1(2) = 3; SD%NE(dir)%R2(2) = c_in%sn-2
         elseif (SD%CE(dir)%R2(1).eq.0) then
           ! Case 2 (partial overlap of fluid, fluid "leaves" wall domain)
           ! Traversing c_tot and c_in, only CE1 and TCE2 are found
           ! 
           !                           TCE1     TCE2
           !                            |----------------fluid------------------|
           !     |-------------wall---------------|
           !                           CE1       CE2
           !
           SD%defined(dir) = .true.

           SD%CE(dir)%R2(1) = c_tot%sc-1
           SD%CE(dir)%R1(2) = 2

           SD%NI(dir)%R1(2) = 1; SD%NI(dir)%R2(2) = c_in%sn
           SD%NB(dir)%R1(2) = 2; SD%NB(dir)%R2(2) = c_in%sn-1
           SD%NE(dir)%R1(2) = 3; SD%NE(dir)%R2(2) = c_in%sn-2
           SD%CI(dir)%R1(2) = 1; SD%CI(dir)%R2(2) = c_in%sc
         endif
       end subroutine

       subroutine define_CI(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%CI(dir)%R1(1) = SD%CE(dir)%R1(1)-1
         SD%CI(dir)%R2(1) = SD%CE(dir)%R2(1)+1
       end subroutine

       subroutine define_N(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%NB(dir)%R1(1) = SD%CE(dir)%R1(1)
         SD%NB(dir)%R2(1) = SD%CE(dir)%R2(1)+1
         SD%NE(dir)%R1(1) = SD%NB(dir)%R1(1)+1
         SD%NE(dir)%R2(1) = SD%NB(dir)%R2(1)-1
         SD%NI(dir)%R1(1) = SD%NB(dir)%R1(1)-1
         SD%NI(dir)%R2(1) = SD%NB(dir)%R2(1)+1
       end subroutine

       subroutine print_subdomain(SD,name)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         call display(SD,name,6)
       end subroutine

       subroutine display_subdomain(SD,name,u)
         implicit none
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: name
         integer,intent(in) :: u
         integer :: i
         write(u,*) ' ********** Subdomain ************ '//name
         write(u,*) 'CE1 = ',(/(SD%CE(i)%R1(1),i=1,3)/)
         write(u,*) 'CE2 = ',(/(SD%CE(i)%R2(1),i=1,3)/)
         write(u,*) 'CI1 = ',(/(SD%CI(i)%R1(1),i=1,3)/)
         write(u,*) 'CI2 = ',(/(SD%CI(i)%R2(1),i=1,3)/)
         write(u,*) 'NB1 = ',(/(SD%NB(i)%R1(1),i=1,3)/)
         write(u,*) 'NB2 = ',(/(SD%NB(i)%R2(1),i=1,3)/)
         write(u,*) 'NI1 = ',(/(SD%NI(i)%R1(1),i=1,3)/)
         write(u,*) 'NI2 = ',(/(SD%NI(i)%R2(1),i=1,3)/)
         write(u,*) 'NE1 = ',(/(SD%NE(i)%R1(1),i=1,3)/)
         write(u,*) 'NE2 = ',(/(SD%NE(i)%R2(1),i=1,3)/)

         write(u,*) 'TCE1 = ',(/(SD%CE(i)%R1(2),i=1,3)/)
         write(u,*) 'TCE2 = ',(/(SD%CE(i)%R2(2),i=1,3)/)
         write(u,*) 'TCI1 = ',(/(SD%CI(i)%R1(2),i=1,3)/)
         write(u,*) 'TCI2 = ',(/(SD%CI(i)%R2(2),i=1,3)/)
         write(u,*) 'TNB1 = ',(/(SD%NB(i)%R1(2),i=1,3)/)
         write(u,*) 'TNB2 = ',(/(SD%NB(i)%R2(2),i=1,3)/)
         write(u,*) 'TNI1 = ',(/(SD%NI(i)%R1(2),i=1,3)/)
         write(u,*) 'TNI2 = ',(/(SD%NI(i)%R2(2),i=1,3)/)
         write(u,*) 'TNE1 = ',(/(SD%NE(i)%R1(2),i=1,3)/)
         write(u,*) 'TNE2 = ',(/(SD%NE(i)%R2(2),i=1,3)/)
         write(u,*) 'g_in_id = ',SD%g_in_id
         write(u,*) 'g_tot_id = ',SD%g_tot_id
         write(u,*) 'defined = ',SD%defined
         write(u,*) ' ********************************* '
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
         do i=1,3; call export(SD%NE(i),u); enddo
         write(u,*) 'g_in_id = ';  write(u,*) SD%g_in_id
         write(u,*) 'g_tot_id = '; write(u,*) SD%g_tot_id
         write(u,*) 'defined = ';  write(u,*) SD%defined
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
         do i=1,3; call import(SD%NE(i),u); enddo
         read(u,*); read(u,*) SD%g_in_id
         read(u,*); read(u,*) SD%g_tot_id
         read(u,*); read(u,*) SD%defined
         read(u,*)
       end subroutine

       end module