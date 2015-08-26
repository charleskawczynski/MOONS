       module modelProblem_mod
       use grid_mod
       use BCs_mod
       use applyBCs_mod
       use ops_discrete_mod
       use ops_aux_mod
       implicit none
       private

       public :: get_ModelProblem

       ! integer,parameter :: modelProblem = 1

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: PI = real(3.14159265358979,cp)

       contains

       subroutine defineBCs(u_bcs,g,s,bctype)
         implicit none
         type(BCs),intent(inout) :: u_bcs
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: bctype
         type(grid),intent(in) :: g
         integer :: i,WC,WI ! Wall coincident / Wall incoincident

         ! bctype = 1 ! Dirichlet
         !          2 ! Neumann

         select case (bctype)
         case (1); WC = 1; WI = 2
         case (2); WC = 4; WI = 5
         case default
         stop 'Error: bctype must = 1,2 in symmetry.f90'
         end select

         if ( all( (/ (g%c(i)%sn.eq.s(i),i=1,3) /) ) ) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
         elseif (all((/(g%c(i)%sc.eq.s(i),i=1,3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)

         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setXminType(u_bcs,WC); call setXmaxType(u_bcs,WC)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setYminType(u_bcs,1); call setYmaxType(u_bcs,1)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setZminType(u_bcs,WC); call setZmaxType(u_bcs,WC)

         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setXminType(u_bcs,WI); call setXmaxType(u_bcs,WI)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setYminType(u_bcs,WI); call setYmaxType(u_bcs,WI)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setZminType(u_bcs,WI); call setZmaxType(u_bcs,WI)
         else
          stop 'Error: Bad sizes in defineBCs in symmetry.f90'
         endif

         call setGrid(u_bcs,g)
         call checkBCs(u_bcs)
       end subroutine

       subroutine defineFunction(u,x,y,z)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: x,y,z
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(u)

         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u(i,j,k) = real(x(i),cp) * real(x(i),cp)
         enddo;enddo;enddo
         ! if (mod(s(1),2).eq.0) then
         !   do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         !   u(i,j,k) = real(i,cp) - (s(1))/2
         !   enddo;enddo;enddo
         ! else
         !   do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         !   u(i,j,k) = real(i,cp) - (s(1)+1)/2
         !   enddo;enddo;enddo
         ! endif
       end subroutine

       subroutine get_ModelProblem(g,f,u_bcs)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(BCs),intent(inout) :: u_bcs
         integer,dimension(3) :: s
         integer :: bctype,i
         s = shape(f)

         bctype = 1 ! Dirichlet
         ! bctype = 2 ! Neumann
         call defineBCs(u_bcs,g,s,bctype)

         ! Node data
         if (all((/(g%c(i)%sn.eq.s(i),i=1,3)/))) then
           call defineFunction(f,g%c(1)%hn,g%c(2)%hn,g%c(3)%hn)

           ! CC data
         elseif (all((/(g%c(i)%sc.eq.s(i),i=1,3)/))) then
           call defineFunction(f,g%c(1)%hc,g%c(2)%hc,g%c(3)%hc)

           ! Face data
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hn,g%c(2)%hc,g%c(3)%hc)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hc,g%c(2)%hn,g%c(3)%hc)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hc,g%c(2)%hc,g%c(3)%hn)

           ! Edge data
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hc,g%c(2)%hn,g%c(3)%hn)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hn,g%c(2)%hc,g%c(3)%hn)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(f,g%c(1)%hn,g%c(2)%hn,g%c(3)%hc)
         else
          stop 'Error: Bad sizes in defineBCs in symmetry.f90'
         endif
       end subroutine

       end module

       module test_symmetry_mod
       use simParams_mod
       use IO_scalarFields_mod
       use myTime_mod
       use grid_mod
       use myError_mod
       use ops_discrete_mod
       use del_mod
       use BCs_mod
       use applyBCs_mod
       use gridGen_mod
       use gridGenTools_mod
       use ops_aux_mod
       use interpOps_mod

       use modelProblem_mod

       implicit none
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine test_symmetry(dir)
         implicit none
         character(len=*),intent(in) :: dir

         type(grid) :: g
         integer,dimension(3),parameter :: N = 2**5 ! Number of cells
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: sn,sc,s
         integer :: i
         real(cp),dimension(3) :: dh
         type(BCs) :: u_bcs
         type(gridGenerator) :: gg
         type(del) :: d
         real(cp) :: y_c,tau,beta
         ! Field quantities
         integer :: symmTest
         real(cp),dimension(:,:,:),allocatable :: u,f

         write(*,*) 'Number of cells = ',N

         symmTest = 1

         ! ***********************************************************
         ! ***********************************************************
         ! ******************** GRID GENERATION **********************
         ! ***********************************************************
         ! ***********************************************************
         hmin = real(-0.5,cp); hmax = real(0.5,cp)
         dh = (hmax-hmin)/real(N,cp)
         y_c = real(0.5,cp)
         tau = real(5.0,cp)
         beta = real(1.01,cp)

         ! Uniform
         ! call init(gg,(/uniform(hmin(1),hmax(1),N(1))/),1)
         call init(gg,(/robertsBoth(hmin(1),hmax(1),N(1),beta)/),1)
         call init(gg,(/uniform(hmin(2),hmax(2),N(2))/),2)
         call init(gg,(/uniform(hmin(3),hmax(3),N(3))/),3)
         ! Roberts
         ! call init(gg,(/cluster(hmin(1),hmax(1),N(1),y_c,tau)/),1)
         ! call prep(gg,(/robertsLeft(hmin(1),hmax(1),N(1),beta)/),1)

         call applyGhost(gg,1)
         call applyGhost(gg,2)
         call applyGhost(gg,3)
         call init(g,gg%g%c(1)%hn,1,2) ! 2 means node values given
         call init(g,gg%g%c(2)%hn,2,2) ! 2 means node values given
         call init(g,gg%g%c(3)%hn,3,2) ! 2 means node values given
         call export(g,dir,'g_base')

         sc = (/(g%c(i)%sc,i=1,3)/) ! Cell centered data
         sn = (/(g%c(i)%sn,i=1,3)/) ! Node data

         ! ***********************************************************
         ! ***********************************************************
         ! ********************* DEL OPERATOR ************************
         ! ***********************************************************
         ! ***********************************************************

         ! ******************** STAGGERED CC->N **********************
         if (symmTest.eq.1) then
           allocate(u(sn(1),sn(2),sn(3)))
           allocate(f(sc(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,1,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hn,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hc,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ******************* STAGGERED N->CC ***********************
         if (symmTest.eq.2) then
           allocate(u(sc(1),sn(2),sn(3)))
           allocate(f(sn(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,1,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hc,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hn,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ****************** COLLOCATED n=1, CC *********************
         if (symmTest.eq.3) then
           allocate(u(sc(1),sn(2),sn(3)))
           allocate(f(sc(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,1,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hc,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hc,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ****************** COLLOCATED n=1, N  *********************
         if (symmTest.eq.4) then
           allocate(u(sn(1),sn(2),sn(3)))
           allocate(f(sn(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,1,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hn,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hn,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ****************** COLLOCATED n=2, CC *********************
         if (symmTest.eq.5) then
           allocate(u(sc(1),sn(2),sn(3)))
           allocate(f(sc(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,2,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hc,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hc,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ****************** COLLOCATED n=2, N  *********************
         if (symmTest.eq.6) then
           allocate(u(sn(1),sn(2),sn(3)))
           allocate(f(sn(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call d%assign(f,u,g,2,1,0)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hn,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hn,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ***********************************************************
         ! ***********************************************************
         ! ******************* INTERP OPERATOR ***********************
         ! ***********************************************************
         ! ***********************************************************

         ! ************************* CC->N  **************************
         if (symmTest.eq.7) then
           allocate(u(sn(1),sn(2),sn(3)))
           allocate(f(sc(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call myNode2Edge(f,u,g,1)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hn,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hc,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ************************* N->CC  **************************
         if (symmTest.eq.8) then
           allocate(u(sc(1),sn(2),sn(3)))
           allocate(f(sn(1),sn(2),sn(3)))
           call get_ModelProblem(g,u,u_bcs)
           call myEdge2Node(f,u,g,1)
           call mirrorX(f,.true.)
           call writeToFile(g%c(1)%hc,u(:,5,5),dir,'u')
           call writeToFile(g%c(1)%hn,f(:,5,5),dir,'f')
           deallocate(u,f)
         endif

         ! ***********************************************************
         ! ***********************************************************
         ! ************************ APPLY BCs ************************
         ! ***********************************************************
         ! ***********************************************************

         call delete(g)
       end subroutine

       subroutine mirrorX(f,TF)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         logical,intent(in),optional :: TF
         integer,dimension(3) :: s
         logical :: TF_temp
         integer :: i
         s = shape(f)
         if (present(TF)) then
          TF_temp = TF
        else
          TF_temp = .false.
        endif

         if (TF_temp) then
           if (mod(s(1),2).eq.0) then ! Even
             do i = 1,s(1)/2
              f(i,:,:) = abs(f(i,:,:)) - abs(f(s(1)-i+1,:,:))
              f(s(1)-i+1,:,:) = f(i,:,:)
             enddo
           else ! Odd
             do i = 1,(s(1)+1)/2
              f(i,:,:) = abs(f(i,:,:)) - abs(f(s(1)-i+1,:,:))
              f(s(1)-i+1,:,:) = f(i,:,:)
             enddo
           endif
         else
           if (mod(s(1),2).eq.0) then ! Even
             do i = 1,s(1)/2
              f(i,:,:) = f(i,:,:) - f(s(1)-i+1,:,:)
              f(s(1)-i+1,:,:) = f(i,:,:)
             enddo
           else ! Odd
             do i = 1,(s(1)+1)/2
              f(i,:,:) = f(i,:,:) - f(s(1)-i+1,:,:)
              f(s(1)-i+1,:,:) = f(i,:,:)
             enddo
           endif
         endif
       end subroutine

       end module
