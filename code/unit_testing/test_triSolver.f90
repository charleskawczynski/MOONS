       module modelProblem_mod
       use grid_mod
       use SF_mod
       use BCs_mod
       implicit none
       private

       public :: get_ModelProblem,defineBCs

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

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
         stop 'Error: bctype must = 1,2 in poisson.f90'
         end select

         call init(u_bcs,g,s)
         if (bctype.eq.1) then
               call init_Dirichlet(u_bcs)
         else; call init_Neumann(u_bcs)
         endif
         call init(u_bcs,0.0_cp)

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
          stop 'Error: Bad sizes in defineBCs in poisson.f90'
         endif

         call setGrid(u_bcs,g)
         call checkBCs(u_bcs)
       end subroutine

       subroutine defineFunction(u,x,y,z)
         implicit none
         type(SF),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: x,y,z
         integer :: i,j,k
         real(cp),dimension(3) :: p
         integer,dimension(3) :: s
         s = u%RF(1)%s

         p = (/4.0_cp,4.0_cp,4.0_cp/)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u%RF(1)%f(i,j,k) = cos(p(1)*PI*x(i))*&
                            cos(p(2)*PI*y(j))*&
                            cos(p(3)*PI*z(k))
         enddo;enddo;enddo
       end subroutine

       subroutine get_ModelProblem(g,f)
         implicit none
         type(grid),intent(in) :: g
         type(SF),intent(inout) :: f
         integer :: i
         integer,dimension(3) :: s
         s = f%RF(1)%s

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
          stop 'Error: Bad sizes in defineBCs in poisson.f90'
         endif

       end subroutine

       end module

       module unit_test_mod
       use simParams_mod
       use grid_mod
       use gridGen_mod
       use gridGenTools_mod
       use norms_mod
       use del_mod
       use SF_mod
       use BCs_mod
       use applyBCs_mod
       use IO_SF_mod
       use triSolver_mod
       use triDiag_mod
       use modelProblem_mod
       use ops_aux_mod

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

       subroutine unit_test(dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid) :: g
         integer,dimension(3) :: N = 40 ! Number of cells
         real(cp),dimension(3) :: hmin,hmax
         type(gridGenerator) :: gg
         type(SF) :: u,Au,f,u2,e
         type(triSolver) :: TS
         type(del) :: d
         type(norms) :: norm
         integer,dimension(3) :: s

         write(*,*) 'Number of cells = ',N

         hmin = 0.0_cp; hmax = 1.0_cp
         call init(gg,(/robertsBoth(hmin(1),hmax(1),N(1),1.01_cp)/),1)
         call init(gg,(/robertsBoth(hmin(2),hmax(2),N(2),1.01_cp)/),2)
         call init(gg,(/robertsBoth(hmin(3),hmax(3),N(3),1.01_cp)/),3)

         ! call init(gg,(/uniform(hmin(1),hmax(1),N(1))/),1)
         ! call init(gg,(/uniform(hmin(2),hmax(2),N(2))/),2)
         ! call init(gg,(/uniform(hmin(3),hmax(3),N(3))/),3)
         call applyGhost(gg); call init(g,gg%g)
         call init_stencils(g)
         ! What to change:
         !     derivative stencil
         !     triSolver that is called
         !     type of BCs in u2
         !     bvals for u2

         call init_CC(u,g)
         call init(Au,u)
         call init(f,u)
         call init(u2,u)
         call init(e,u)
         s = u%RF(1)%s

         call assign(u,0.0_cp)
         call assign(u2,0.0_cp)
         call assign(Au,0.0_cp)
         call assign(f,0.0_cp)
         call assign(e,0.0_cp)
         call get_ModelProblem(g,u)
         call assign(u,1.0_cp)
         call d%assign(Au,u,g,2,1,0)
         call assign(f,Au)

         call init(TS,g%c(1)%lapCC)

         call init(u2%RF(1)%b,u2%RF(1)%s)
         call setGrid(u2%RF(1)%b,g)
         ! call setAllZero(u2%RF(1)%b,1) ! Dirichlet, Node
         call setAllZero(u2%RF(1)%b,2) ! Dirichlet, CC
         call setAllBVals(u2%RF(1)%b,u%RF(1)%f,g)

         call checkBCs(u2%RF(1)%b)

         call applyAllBCs(u2,g) ! Need to define boundary value

         call apply(TS,u2,f,1,1)   ! u = A^-1 f


         call assign(e,u)
         call subtract(e,u2)
         call zeroGhostPoints(e) ! Ghost should not be compared
         call compute(norm,e,g)
         call print(norm,'e')

         call export_1C_SF(g,u,dir,'u',0)
         call export_1C_SF(g,Au,dir,'Au',0)
         call export_1C_SF(g,f,dir,'f',0)
         call export_1C_SF(g,u2,dir,'u2',0)
         call export_1C_SF(g,e,dir,'e',0)

         call delete(u)
         call delete(Au)
         call delete(f)
         call delete(u2)
         call delete(e)
       end subroutine

       end module
