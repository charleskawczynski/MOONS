       module modelProblem_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       use VF_mod
       use applyBCs_mod
       use ops_discrete_mod
       use ops_aux_mod
       implicit none
       private

       public :: get_ModelProblem,defineSig

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

       subroutine defineFunction(u,x,y,z,bctype)
         implicit none
         type(SF),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: x,y,z
         integer,intent(in) :: bctype
         integer :: i,j,k,t,nmodes
         real(cp),dimension(3) :: p
         integer,dimension(3) :: s
         s = u%RF(1)%s

         call assign(u,0.0_cp)
         nmodes = 1
         do t=1,nmodes
           select case (bctype)
           case (1); p = 3.0_cp
           case (2); p = 2.0_cp
           end select

           p = p*real(t,cp)

           if (bctype.eq.2) then
             do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
             u%RF(1)%f(i,j,k) = u%RF(1)%f(i,j,k) +(cos(p(1)*PI*x(i))*&
                                                   cos(p(2)*PI*y(j))*&
                                                   cos(p(3)*PI*z(k)))/real(t,cp)
             enddo;enddo;enddo
           elseif (bctype.eq.1) then
             do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
             u%RF(1)%f(i,j,k) = u%RF(1)%f(i,j,k) +(sin(p(1)*PI*x(i))*&
                                                   sin(p(2)*PI*y(j))*&
                                                   sin(p(3)*PI*z(k)))/real(t,cp)
             enddo;enddo;enddo
           elseif (bctype.eq.3) then ! Heated plate type problem
             u%RF(1)%f = 0.0_cp
           endif

           ! if (bctype.eq.2) then
           !   do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
           !   u%RF(1)%f(i,:,:) = u%RF(1)%f(i,:,:) + cos(p(1)*PI*x(i))/real(t,cp)
           !   enddo;enddo;enddo
           ! elseif (bctype.eq.1) then
           !   do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
           !   u%RF(1)%f(i,:,:) = u%RF(1)%f(i,:,:) + sin(p(1)*PI*x(i))/real(t,cp)
           !   enddo;enddo;enddo
           ! endif
         enddo
       end subroutine

       subroutine get_ModelProblem(m,f,u,u_exact,bctype)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: u,u_exact,f
         integer,intent(in) :: bctype
         type(VF) :: temp
         integer,dimension(3) :: s
         s = f%RF(1)%s

         call init(u%RF(1)%b,m%g(1),s)
         if (bctype.eq.1) then
          call init_Dirichlet(u%RF(1)%b)
          call init(u%RF(1)%b,0.0_cp)
         elseif (bctype.eq.2) then
          call init_Neumann(u%RF(1)%b)
          call init(u%RF(1)%b,0.0_cp)
         elseif (bctype.eq.3) then
          call init_Dirichlet(u%RF(1)%b)
          call init(u%RF(1)%b,1.0_cp)
          call init(u%RF(1)%b,0.0_cp,3)
          call init(u%RF(1)%b,0.0_cp,4)
          call init_Neumann(u%RF(1)%b,1)
          call init_Neumann(u%RF(1)%b,2)
          call init(u%RF(1)%b,0.0_cp,1)
          call init(u%RF(1)%b,0.0_cp,2)
         else; stop 'Error: bctype must = 1,2 in test_JAC.f90'
         endif

         ! Node data
         if (f%is_Node) then
           call defineFunction(u_exact,m%g(1)%c(1)%hn,m%g(1)%c(2)%hn,m%g(1)%c(3)%hn,bctype)
           call init_Edge(temp,m)

           ! CC data
         elseif (f%is_CC) then
           call defineFunction(u_exact,m%g(1)%c(1)%hc,m%g(1)%c(2)%hc,m%g(1)%c(3)%hc,bctype)
           call init_Face(temp,m)
           ! Face data
         elseif (f%is_Face.and.f%face.eq.1) then
           call defineFunction(u_exact,m%g(1)%c(1)%hn,m%g(1)%c(2)%hc,m%g(1)%c(3)%hc,bctype)
         elseif (f%is_Face.and.f%face.eq.2) then
           call defineFunction(u_exact,m%g(1)%c(1)%hc,m%g(1)%c(2)%hn,m%g(1)%c(3)%hc,bctype)
         elseif (f%is_Face.and.f%face.eq.3) then
           call defineFunction(u_exact,m%g(1)%c(1)%hc,m%g(1)%c(2)%hc,m%g(1)%c(3)%hn,bctype)

           ! Edge data
         elseif (f%is_Edge.and.f%edge.eq.1) then
           call defineFunction(u_exact,m%g(1)%c(1)%hc,m%g(1)%c(2)%hn,m%g(1)%c(3)%hn,bctype)
         elseif (f%is_Edge.and.f%edge.eq.2) then
           call defineFunction(u_exact,m%g(1)%c(1)%hn,m%g(1)%c(2)%hc,m%g(1)%c(3)%hn,bctype)
         elseif (f%is_Edge.and.f%edge.eq.3) then
           call defineFunction(u_exact,m%g(1)%c(1)%hn,m%g(1)%c(2)%hn,m%g(1)%c(3)%hc,bctype)
         else
          stop 'Error: Bad sizes in defineBCs in poisson.f90'
         endif

         ! f must reach to ghost nodes, which must be defined
         select case (bctype)
         case (1); call applyAllBCs(u_exact,m,u)
                   call grad(temp,u_exact,m); call div(f,temp,m) ! Results in strange res, but converges slowly
         case (2); call subtract(u_exact,mean(u_exact))
                   call applyAllBCs(u_exact,m,f)
                   ! call lap(f,u_exact,m) ! Results in good looking res but diverges
                   call grad(temp,u_exact,m); call div(f,temp,m) ! Results in strange res, but converges slowly
         case (3); f%RF(1)%f = 0.0_cp
                   call applyAllBCs(f,m,u)
         end select
         
         call delete(temp)
         
         call applyAllBCs(u,m)
         call zeroGhostPoints(f)
       end subroutine

       subroutine defineSig(sig)
         implicit none
         type(SF),intent(inout) :: sig
         integer :: i,j,k
         integer,dimension(3) :: s
         logical,dimension(3) :: TF
         integer :: frac
         s = sig%RF(1)%s
         frac = 3
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
         TF(1) = (i.gt.s(1)/frac).and.(i.lt.s(1) - s(1)/frac)
         TF(2) = (j.gt.s(2)/frac).and.(j.lt.s(2) - s(2)/frac)
         TF(3) = (k.gt.s(3)/frac).and.(k.lt.s(3) - s(3)/frac)
         ! if (all(TF)) sig%RF(1)%f(i,j,k) = 0.01_cp
         enddo; enddo; enddo

       end subroutine

       end module

       module unit_test_mod
       use simParams_mod
       use IO_SF_mod
       use IO_tools_mod
       use grid_mod
       use mesh_mod
       use norms_mod
       use ops_discrete_mod
       use BCs_mod
       use applyBCs_mod
       use CG_mod
       use SOR_mod
       use SF_mod
       use VF_mod
       use geometries_mod
       use gridGen_mod
       use gridGenTools_mod
       use solverSettings_mod
       use ops_interp_mod
       use ops_aux_mod

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

       subroutine unit_test(dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid) :: g
         type(mesh) :: m
         character(len=3) :: name
         type(norms) :: norm_res,norm_e
         type(gridGenerator) :: gg
         type(SF) :: u,u_exact,f,Au,e,R,sig,temp_SF,temp2,Aug,ug
         real(cp) :: dt
         integer :: i,NU,bctype

         call cube(m)

         call export(m,dir,'m_base')

         ! *************************************************************
         ! ****************** PARAMETERS TO DEFINE *********************
         ! *************************************************************

         ! bctype = 1 ! Dirichlet
         ! bctype = 2 ! Neumann
         bctype = 3 ! Hot plate type problem
         call init_Node(u,m)

         call init(temp_SF,u)
         call init(temp2,u)
         call init(sig,u)

         call init(e,u)
         call init(R,u)
         call init(u_exact,u)
         call init(f,u)
         call init(Au,u)
         call init(Aug,u)
         call init(ug,u)

         write(*,*) 'Before model problem'
         call get_ModelProblem(m,f,u,u_exact,bctype)
         write(*,*) 'Model problem finished!'

         call export_3D_1C(m,u_exact,dir,'u_exact',0)
         call export_3D_1C(m,f,dir,'f',0)

         ! *************************************************************
         ! *************************************************************
         ! *************************************************************

         ! Steady State
         name = 'CG '
         call assign(u,0.0_cp)
         ! call CG(x,b,m,n,norm,displayTF,temp,Ap,r,p)
         call CG(u,f,m,1000,norm_res,.true.,temp_SF,Au,R,temp2)
         call compute_Ax(Au,u,m)
         call subtract(e,u,u_exact)
         call subtract(R,Au,f)
         call zeroGhostPoints(R)
         call zeroWall(R,m,u)
         call export_3D_1C(m,R,dir,'R_'//name,0)
         call export_3D_1C(m,u,dir,'u_'//name,0)
         call export_3D_1C(m,u,dir,'u_phys_'//name,1)
         call export_3D_1C(m,e,dir,'e_'//name,0)
         call zeroGhostPoints(e)
         call compute(norm_e,e,m)
         call print(norm_e,'u_'//name//' vs u_exact')

         call delete(m)
         call delete(g)
         call delete(sig)
         call delete(e)
         call delete(u)
         call delete(temp_SF)
         call delete(temp2)
         call delete(f)
         call delete(Aug)
         call delete(ug)
         call delete(u_exact)
         call delete(Au)
         call delete(R)
       end subroutine

       end module
