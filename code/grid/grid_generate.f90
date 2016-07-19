       module grid_generate_mod
       use current_precision_mod
       use grid_mod
       use grid_genHelper_mod
       use grid_distribution_funcs_mod
       use grid_stretchParamMatch_mod
       implicit none

       private

       public :: cavity3D_uniform
       public :: cavity3D_nonUniform
       public :: cavity3D_uniformBL

       public :: extend_uniform
       public :: extend_nonuniform
       public :: extend_nonuniform_both
       public :: extend_nonuniform_both_safe

       contains

       ! *********************************************************************
       ! ************************* GENERATE ROUTINES *************************
       ! *********************************************************************

       subroutine cavity3D_uniform(g,hmin,hmax,Ni)
         implicit none
         type(grid),intent(inout) :: g
         integer,dimension(3),intent(in) :: Ni
         real(cp),dimension(3),intent(in) :: hmin,hmax
         integer :: i
         type(gridGenerator) :: gg
         do i=1,3
           call init(gg,(/uniform(hmin(i),hmax(i),Ni(i))/),i)
           call applyGhost(gg,i)
           call init(g,gg%g%c(i)%hn,i)
         enddo
         call init(g,gg%g)
         call delete(gg)
       end subroutine

       subroutine cavity3D_nonUniform(g,hmin,hmax,Ni,betai)
         implicit none
         type(grid),intent(inout) :: g
         integer,dimension(3),intent(in) :: Ni
         real(cp),dimension(3),intent(in) :: hmin,hmax,betai
         integer :: i
         type(gridGenerator) :: gg
         do i=1,3
           call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
           call applyGhost(gg,i)
           call init(g,gg%g%c(i)%hn,i)
         enddo
         call init(g,gg%g)
         call delete(gg)
       end subroutine

       subroutine cavity3D_uniformBL(g,hmin,hmax,Ni,betai)
         implicit none
         type(grid),intent(inout) :: g
         integer,dimension(3),intent(in) :: Ni
         real(cp),dimension(3),intent(in) :: hmin,hmax,betai
         integer :: i,j,N_cells_uniform
         real(cp) :: dh1,dh2,temp
         type(gridGenerator) :: gg
         N_cells_uniform = 5
         do i=1,3
           call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
           dh1 = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
           dh2 = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
           if ((i.eq.2).or.(i.eq.3)) then
             temp = (betai(i)-1.0_cp)/real(3.5,cp)+1.0_cp
             call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),temp)/),i)
           else
             call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),temp)/),i)
           endif
           do j=1,N_cells_uniform+1
             call snip(gg,i)
           enddo
           call prep(gg,(/uniformRight(hmin(i),dh1,N_cells_uniform)/),i)
           do j=1,N_cells_uniform+1
             call pop(gg,i)
           enddo
           call app(gg,(/uniformLeft(hmax(i),dh2,N_cells_uniform)/),i)
           call applyGhost(gg,i)
           call init(g,gg%g%c(i)%hn,i)
         enddo
         call init(g,gg%g)
         call delete(gg)
       end subroutine

       ! *********************************************************************
       ! ************************** EXTEND ROUTINES **************************
       ! *********************************************************************

       subroutine extend_uniform(g,g_in,Ntop,Nbot)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,dimension(3),intent(in) :: Ntop,Nbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp) :: dh
         integer :: i
         type(gridGenerator) :: gg
         call init(gg%g,g_in)
         hmin = (/(g_in%c(i)%hmin,i=1,3)/)
         hmax = (/(g_in%c(i)%hmax,i=1,3)/)
         do i=1,3 ! Add walls in all directions
           call snip(gg,i); call pop(gg,i) ! Remove ghost nodes
           if (Nbot(i).gt.0) then
             dh = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
             call snip(gg,i)
             call prep(gg,(/uniformLeft(hmin(i),dh,Nbot(i))/),i)
           endif
           if (Ntop(i).gt.0) then
             dh = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
             call pop(gg,i)
             call app(gg,(/uniformRight(hmax(i),dh,Ntop(i))/),i)
           endif
           call applyGhost(gg,i) ! re-apply ghosts
           call init(g,gg%g%c(i)%hn,i)
         enddo
         call delete(gg)
       end subroutine

       subroutine extend_nonuniform(g,g_in,ttop,tbot,Ntop,Nbot)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),dimension(3),intent(in) :: ttop,tbot
         integer,dimension(3),intent(in) :: Ntop,Nbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: betaTop,betaBot
         integer :: i
         type(gridGenerator) :: gg
         call init(gg%g,g_in)
         hmin = (/(g_in%c(i)%hmin,i=1,3)/)
         hmax = (/(g_in%c(i)%hmax,i=1,3)/)
         do i=1,3 ! Compute stretching parameters:
           betaBot(i) = betaRobertsRight(hmin(i)-tbot(i),hmin(i),Nbot(i),g_in%c(i)%dhn(1))
           betaTop(i) = betaRobertsLeft(hmax(i),hmax(i)+ttop(i),Ntop(i),g_in%c(i)%dhn(g_in%c(i)%sn-1))
         enddo
         do i=1,3 ! Add walls in all directions
           call snip(gg,i); call pop(gg,i) ! Remove ghost nodes
           if (Nbot(i).gt.0) then
             call snip(gg,i)
             call prep(gg,(/robertsRight(hmin(i)-tbot(i),hmin(i),Nbot(i),betaBot(i))/),i)
           endif
           if (Ntop(i).gt.0) then
             call pop(gg,i)
             call app(gg,(/robertsLeft(hmax(i),hmax(i)+ttop(i),Ntop(i),betaTop(i))/),i)
           endif
           call applyGhost(gg,i) ! re-apply ghosts
           call init(g,gg%g%c(i)%hn,i) ! Final assignment
         enddo
         call delete(gg)
       end subroutine

       subroutine extend_nonuniform_both(g,g_in,ttop,tbot,Ntop,Nbot)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),dimension(3),intent(in) :: ttop,tbot
         integer,dimension(3),intent(in) :: Ntop,Nbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: betaTop,betaBot
         integer :: i
         type(gridGenerator) :: gg
         call init(gg%g,g_in)
         hmin = (/(g_in%c(i)%hmin,i=1,3)/)
         hmax = (/(g_in%c(i)%hmax,i=1,3)/)
         do i=1,3 ! Compute stretching parameters:
           betaBot(i) = betaRobertsBoth(hmin(i)-tbot(i),hmin(i),Nbot(i),g_in%c(i)%dhn(1))
           betaTop(i) = betaRobertsBoth(hmax(i),hmax(i)+ttop(i),Ntop(i),g_in%c(i)%dhn(g_in%c(i)%sn-1))
         enddo
         do i=1,3 ! Add walls in all directions
           call snip(gg,i); call pop(gg,i) ! Remove ghost nodes
           if (Nbot(i).gt.0) then
             call snip(gg,i)
             call prep(gg,(/robertsBoth(hmin(i)-tbot(i),hmin(i),Nbot(i),betaBot(i))/),i)
           endif
           if (Ntop(i).gt.0) then
             call pop(gg,i)
             call app(gg,(/robertsBoth(hmax(i),hmax(i)+ttop(i),Ntop(i),betaTop(i))/),i)
           endif
           call applyGhost(gg,i) ! re-apply ghosts
           call init(g,gg%g%c(i)%hn,i) ! Final assignment
         enddo
         call delete(gg)
       end subroutine

       subroutine extend_nonuniform_both_safe(g,g_in,ttop,tbot,Ntop,Nbot)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         real(cp),dimension(3),intent(in) :: ttop,tbot
         integer,dimension(3),intent(in) :: Ntop,Nbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: betaTop,betaBot
         integer :: i
         type(gridGenerator) :: gg,temp
         call init(gg%g,g_in)
         hmin = (/(g_in%c(i)%hmin,i=1,3)/)
         hmax = (/(g_in%c(i)%hmax,i=1,3)/)
         do i=1,3 ! Compute stretching parameters:
           betaBot(i) = betaRobertsBoth(hmin(i)-tbot(i),hmin(i),Nbot(i),g_in%c(i)%dhn(1))
           betaTop(i) = betaRobertsBoth(hmax(i),hmax(i)+ttop(i),Ntop(i),g_in%c(i)%dhn(g_in%c(i)%sn-1))
         enddo
         do i=1,3 ! Add walls in all directions
           if (Nbot(i).gt.0) then 
             call init(temp,(/robertsBoth(hmin(i)-tbot(i),hmin(i),Nbot(i),betaBot(i))/),i)
             call pop(temp,i); call pop(temp,i)
             call prep(gg,temp%g%c(i)%hn,i)
             call prepGhost(gg,i) ! re-apply ghosts
           endif
           if (Ntop(i).gt.0) then
             call init(temp,(/robertsBoth(hmax(i),hmax(i)+ttop(i),Ntop(i),betaTop(i))/),i)
             call snip(temp,i); call snip(temp,i)
             call app(gg,temp%g%c(i)%hn,i)
             call appGhost(gg,i) ! re-apply ghosts
           endif
           call init(g,gg%g%c(i)%hn,i) ! Final assignment
         enddo
         call delete(gg)
         call delete(temp)
       end subroutine

       end module