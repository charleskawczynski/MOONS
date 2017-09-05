       module physical_domain_extend_mod
       use physical_domain_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use physical_sub_domain_mod
       use physical_sub_domain_extend_mod
       use data_location_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: init,display,print

       public :: add,init_mixed

       public :: pick_extrema_bot
       public :: pick_extrema_top

       interface init;             module procedure init_PD_mesh;            end interface
       interface init;             module procedure init_PD_grid;            end interface
       interface print;            module procedure print_PD;                end interface

       interface add;              module procedure add_physical_sub_domain; end interface
       interface add;              module procedure add_PD_grid;             end interface

       interface init_mixed;       module procedure init_mixed_PD;           end interface

       interface pick_extrema_bot; module procedure pick_extrema_bot_PD;     end interface
       interface pick_extrema_top; module procedure pick_extrema_top_PD;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_PD_mesh(D,m_R1,m_R2)
         implicit none
         type(physical_domain),intent(inout) :: D
         type(mesh),intent(in) :: m_R1,m_R2
         type(physical_sub_domain) :: temp
         integer :: j,k
         call delete(D)
         ! Make all possible/necessary physical_sub_domains:
         if (m_R2%s.gt.m_R1%s) then
           do k=1,m_R2%s; do j=1,m_R1%s
             call init(temp,m_R1%B(j)%g,m_R2%B(k)%g,j,k)
             if (temp%defined) call add(D,temp)
           enddo; enddo
         else
           do k=1,m_R1%s; do j=1,m_R2%s
             call init(temp,m_R1%B(k)%g,m_R2%B(j)%g,k,j)
             if (temp%defined) call add(D,temp)
           enddo; enddo
         endif
         D%defined = size(D%sd).gt.0
       end subroutine

       subroutine init_PD_grid(D,g_R1,g_R2,g_id_1,g_id_2)
         implicit none
         type(physical_domain),intent(inout) :: D
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_id_1,g_id_2
         call delete(D)
         call add(D,g_R1,g_R2,g_id_1,g_id_2)
         D%defined = size(D%sd).gt.0
       end subroutine

       subroutine init_mixed_PD(D,DL)
         implicit none
         type(physical_domain),intent(inout) :: D
         type(data_location),intent(in) :: DL
         integer :: i
         if (D%defined) then
           do i=1,D%s; call init_mixed(D%sd(i),DL); enddo
         endif
       end subroutine

       subroutine print_PD(D,name)
         implicit none
         type(physical_domain),intent(in) :: D
         character(len=*),intent(in) :: name
         integer :: i
         write(*,*) 'N-physical_sub_domains = ',D%s
         do i=1,D%s; call print(D%sd(i),name//'_'//int2str(i)); enddo
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine add_PD_grid(D,g_R1,g_R2,g_id_1,g_id_2)
         implicit none
         type(physical_domain),intent(inout) :: D
         type(grid),intent(in) :: g_R1,g_R2
         integer,intent(in) :: g_id_1,g_id_2
         type(physical_sub_domain) :: temp
         call init(temp,g_R1,g_R2,g_id_1,g_id_2)
         if (temp%defined) call add(D,temp)
         call delete(temp)
         D%defined = size(D%sd).gt.0
       end subroutine

       subroutine add_physical_sub_domain(D,sd)
         implicit none
         type(physical_domain),intent(inout) :: D
         type(physical_sub_domain),intent(in) :: sd
         type(physical_domain) :: temp
         integer :: i
         if (.not.allocated(D%sd)) then
           D%s = 1
           allocate(D%sd(D%s))
           call init(D%sd(D%s),sd)
         else
           call init(temp,D)
           call delete(D)
           D%s = temp%s + 1
           allocate(D%sd(D%s))
           do i=1,D%s-1; call init(D%sd(i),temp%sd(i)); enddo
           call init(D%sd(D%s),sd)
           call delete(temp)
         endif
         D%defined = size(D%sd).gt.0
       end subroutine

       subroutine pick_extrema_bot_PD(D,i,dir)
         implicit none
         type(physical_domain),intent(inout) :: D
         integer,intent(in) :: i,dir
         call pick_extrema_bot(D%sd(i),dir)
       end subroutine

       subroutine pick_extrema_top_PD(D,i,dir)
         implicit none
         type(physical_domain),intent(inout) :: D
         integer,intent(in) :: i,dir
         call pick_extrema_top(D%sd(i),dir)
       end subroutine

       end module