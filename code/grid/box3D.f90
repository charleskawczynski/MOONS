       module box3D_mod
       use grid_mod
       use gridGen_mod
       use gridGenTools_mod
       use matchGridStretching_mod
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

       public :: box3D_uniform
       public :: box3D_Roberts_L,box3D_Roberts_R,box3D_Roberts_B
       ! public :: box3D_uniformBL

       contains

       subroutine process(g,gg,dir)
         implicit none
         type(grid),intent(inout) :: g
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         call applyGhost(gg,dir)
         call init(g,gg%g%c(dir)%hn,dir)
         ! call init_Stencils(gg%g%c(dir))
         call init(g%c(dir),gg%g%c(dir))
         call initProps(g)
         call delete(gg)
       end subroutine

       subroutine box3D_uniform(g,hmin,hmax,N,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax
         type(gridGenerator) :: gg
         call init(gg,(/uniform(hmin,hmax,N)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine box3D_Roberts_L(g,hmin,hmax,N,beta,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(gridGenerator) :: gg
         call init(gg,(/robertsLeft(hmin,hmax,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine box3D_Roberts_R(g,hmin,hmax,N,beta,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(gridGenerator) :: gg
         call init(gg,(/robertsRight(hmin,hmax,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine box3D_Roberts_B(g,hmin,hmax,N,beta,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,beta
         type(gridGenerator) :: gg
         call init(gg,(/robertsBoth(hmin,hmax,N,beta)/),dir)
         call process(g,gg,dir)
       end subroutine

       subroutine box3D_cluster(g,hmin,hmax,N,yc,tau,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: N,dir
         real(cp),intent(in) :: hmin,hmax,yc,tau
         type(gridGenerator) :: gg
         call init(gg,(/cluster(hmin,hmax,N,yc,tau)/),dir)
         call process(g,gg,dir)
       end subroutine

       ! subroutine box3D_Roberts_L_UBL(g,hmin,hmax,N,beta,N_uniform,dir)
       !   ! Implements a uniform boundary layer near clustered region
       !   implicit none
       !   type(grid),intent(inout) :: g
       !   integer,intent(in) :: N,dir,N_uniform
       !   real(cp),intent(in) :: hmin,hmax,beta
       !   integer :: i
       !   real(cp) :: dh1,dh2,temp
       !   type(gridGenerator) :: gg
       !   call init(gg,(/robertsLeft(hmin,hmax,N,beta)/),dir)
       !   dh1 = gg%g%c(dir)%hn(2)-gg%g%c(dir)%hn(1)
       !   dh2 = gg%g%c(dir)%hn(gg%g%c(dir)%sn)-gg%g%c(dir)%hn(gg%g%c(dir)%sn-1)
       !   call init(gg,(/robertsBoth(hmin,hmax,N,temp)/),dir)
       !   do i=1,N_uniform+1
       !     call snip(gg,dir)
       !   enddo
       !   call prep(gg,(/uniformRight(hmin,dh1,N_uniform)/),dir)
       !   do i=1,N_uniform+1
       !     call pop(gg,dir)
       !   enddo
       !   call app(gg,(/uniformLeft(hmax,dh2,N_uniform)/),dir)
       !   call applyGhost(gg,dir)
       !   call init(g,gg%g%c(dir)%hn,dir,2)
       !   call init_Stencils(gg%g%c(dir))
       !   call init(g%c(dir),gg%g%c(dir))
       !   call delete(gg)
       ! end subroutine

       end module