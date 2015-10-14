       module modelProblem_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       use applyBCs_mod
       use ops_discrete_mod
       use ops_aux_mod
       implicit none
       private

       public :: get_ModelProblem,defineSig

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

       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       contains

       subroutine defineBCs(u_bcs,g,s,bctype)
         implicit none
         type(BCs),intent(inout) :: u_bcs
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: bctype
         type(grid),intent(in) :: g
         ! bctype = 1 ! Dirichlet
         !          2 ! Neumann
         call init(u_bcs,g,s)
         if (bctype.eq.1) then
          call init_Dirichlet(u_bcs)
         elseif (bctype.eq.2) then
          call init_Neumann(u_bcs)
         else; stop 'Error: bctype must = 1,2 in test_JAC.f90'
         endif
         call init(u_bcs,0.0_cp)
       end subroutine

       subroutine defineFunction(u,x,y,z,bctype)
         implicit none
         type(SF),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: x,y,z
         integer,intent(in) :: bctype
         integer :: i,j,k,t,nmodes
         real(cp),dimension(3) :: p
         integer,dimension(3) :: s
         s = (/size(x),size(y),size(z)/)

         call assign(u,0.0_cp)
         nmodes = 1
         do t=1,nmodes
           select case (bctype)
           case (1); p = 3.0_cp
           case (2); p = 2.0_cp
           case default
           stop 'Error: bctype must = 1,2 in defineFunction in poisson.f90'
           end select

           p = p*real(t,cp)

           if (bctype.eq.2) then
             do k = 2,s(3)-1; do j = 2,s(2)-1; do i = 2,s(1)-1
             u%RF(1)%f(i,j,k) = u%RF(1)%f(i,j,k) +(cos(p(1)*PI*x(i))*&
                                                   cos(p(2)*PI*y(j))*&
                                                   cos(p(3)*PI*z(k)))/real(t,cp)
             enddo;enddo;enddo
           elseif (bctype.eq.1) then
             do k = 2,s(3)-1; do j = 2,s(2)-1; do i = 2,s(1)-1
             u%RF(1)%f(i,j,k) = u%RF(1)%f(i,j,k) +(sin(p(1)*PI*x(i))*&
                                                   sin(p(2)*PI*y(j))*&
                                                   sin(p(3)*PI*z(k)))/real(t,cp)
             enddo;enddo;enddo
           endif
         enddo

         ! call zeroGhostPoints(u)        ! Necessary for BOTH Dirichlet problems AND Neumann
       end subroutine

       subroutine get_ModelProblem(m,f,u,u_exact)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: u,u_exact,f
         integer,dimension(3) :: s
         integer :: bctype,i
         s = f%RF(1)%s

         bctype = 1 ! Dirichlet
         ! bctype = 2 ! Neumann
         do i=1,m%s
           call defineBCs(u%RF(i)%b,m%g(i),s,bctype)
           call defineBCs(u_exact%RF(i)%b,m%g(i),s,bctype)
           call defineBCs(f%RF(i)%b,m%g(i),s,bctype)
         enddo

         ! Node data
         if (f%is_Node) then
           call defineFunction(u_exact,m%g(1)%c(1)%hn,m%g(1)%c(2)%hn,m%g(1)%c(3)%hn,bctype)

           ! CC data
         elseif (f%is_CC) then
           call defineFunction(u_exact,m%g(1)%c(1)%hc,m%g(1)%c(2)%hc,m%g(1)%c(3)%hc,bctype)

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
         call applyAllBCs(u_exact,m)
         call lap(f,u_exact,m)

         ! If Dirichlet, apply BCs to u so u = u_exact
         ! on boundary, otherwise make f satisfy BCs
         if (bctype.eq.1) then
               call applyAllBCs(u,m)
         else; call applyAllBCs(f,m)
         endif
         call assignMinus(f,u_exact)
         call multiply(f,266.4793_cp)
         ! f%RF(1)%f(52,:,:) = 0.0_cp
         ! call assign(f%RF(2),0.0_cp)
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
         if (all(TF)) sig%RF(1)%f(i,j,k) = 0.01_cp
         enddo; enddo; enddo
         if (size(sig%RF).eq.2) then
           s = sig%RF(2)%s
           frac = 3
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
           TF(1) = (i.gt.s(1)/frac).and.(i.lt.s(1) - s(1)/frac)
           TF(2) = (j.gt.s(2)/frac).and.(j.lt.s(2) - s(2)/frac)
           TF(3) = (k.gt.s(3)/frac).and.(k.lt.s(3) - s(3)/frac)
           if (all(TF)) sig%RF(2)%f(i,j,k) = 0.01_cp
           enddo; enddo; enddo
         endif

       end subroutine

       end module

       module unit_test_mod
       use simParams_mod
       use IO_SF_mod
       use IO_tools_mod
       use grid_mod
       use mesh_mod
       use geometries_mod
       use norms_mod
       use generateGrid_mod
       use ops_discrete_mod
       use BCs_mod
       use applyBCs_mod
       use JAC_mod
       use SOR_mod
       use SF_mod
       use VF_mod
       use gridGen_mod
       use gridGenTools_mod
       use solverSettings_mod
       use ops_embedExtract_mod
       use ops_interp_mod
       use ops_aux_mod
       use domain_mod
       use extendGrid_mod
       use connectGrid_mod

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
         type(grid) :: g,g2,g1
         type(mesh) :: m,m_temp,m_tot
         integer,dimension(3) :: N = (/50,50,50/) ! Number of cells
         real(cp),dimension(3) :: hmin,hmax
         type(JACSolver) :: JAC
         type(SORSolver) :: SOR
         character(len=3) :: name
         type(norms) :: norm_res,norm_e
         type(solverSettings) :: ss
         type(gridGenerator) :: gg
         type(domain) :: D
         type(SF) :: u,u_exact,f,lapU,e,R,sig,temp_SF,sig_tot
         type(VF) :: sigma,temp
         real(cp) :: dt
         integer :: i,NU

         write(*,*) 'Number of cells = ',N

         ! Fluid domain (separate)
         hmin = 0.0_cp; hmax = 1.0_cp
         call cavity3D_uniform(g,hmin,hmax,N); call init(m,g)
         hmin = 1.0_cp; hmax = 2.0_cp
         call cavity3D_uniform(g1,hmin,hmax,N); call add(m,g1)
         call initProps(m)
         call patch(m)

         ! Wall domain (total)
         call ext_app_uniform(g2,g,10,1); call ext_prep_uniform(g,g2,10,1)
         call ext_app_uniform(g2,g,10,2); call ext_prep_uniform(g,g2,10,2)
         call ext_app_uniform(g2,g,10,3); call ext_prep_uniform(g,g2,10,3)
         call init(m_tot,g)
         call initProps(m_tot)
         call patch(m_tot)

         call print(m)
         call print(m_tot)

         call init(D,m,m_tot)
         call print(D,'domain')

         call init_CC(sig_tot,m_tot)
         call init_CC(sig,m)

         call assign(sig,1.0_cp)
         call assign(sig_tot,0.0_cp)
         ! call export_1C_SF(m_tot,sig_tot,dir,'sigma_tot',0)
         ! call export_1C_SF(m,sig,dir,'sigma',0)

         call embedCC(sig_tot,sig,D)
         ! call export_1C_SF(m_tot,sig_tot,dir,'sigma_tot_new',0)

         call delete(g2)
         ! call print(m)

         call flow_past_square(m_temp)
         call export_mesh(m_temp,dir,'flow_past_square',1); call delete(m_temp)

         call ins_elbow(m_temp)
         call export_mesh(m_temp,dir,'elbow',1); call delete(m_temp)

         call ins_u_bend(m_temp)
         call export_mesh(m_temp,dir,'ubend',1); call delete(m_temp)

         call ins_sudden_expansion(m_temp)
         call export_mesh(m_temp,dir,'sudden_expansion',1); call delete(m_temp)

         call ins_sep_channel(m_temp)
         call export_mesh(m_temp,dir,'sep_channel',1); call delete(m_temp)

         ! call print(m_temp)
         ! call delete(m_temp)
         stop 'Done'

         call export(m,dir,'m_base')
         call print(m)
         call init(ss)
         call setName(ss,'Lap(u) = f          ')

         ! *************************************************************
         ! ****************** PARAMETERS TO DEFINE *********************
         ! *************************************************************

         call init_CC(u,m)
         call init(temp_SF,u)
         call init_Face(sigma,m)
         call init(sig,u)
         call init(temp,sigma)

         call assign(sig,1.0_cp)
         call defineSig(sig)
         call export_1C_SF(m,sig,dir,'sigma',0)

         call cellCenter2Face(sigma,sig,m)
         ! call export_1C_SF(m,sigma%x,dir,'sigma_x',1)

         call init(e,u)
         call init(R,u)
         call init(u_exact,u)
         call init(f,u)
         call init(lapU,u)

         write(*,*) 'Before model problem'
         call get_ModelProblem(m,f,u,u_exact)
         ! call get_ModelProblem(m,f,temp_SF,u_exact)
         write(*,*) 'Model problem finished!'

         ! call lap(temp_SF,f,m)
         ! call applyAllBCs(temp_SF,m)
         ! call export_1C_SF(m,temp_SF,dir,'lap(f)',0)
         ! stop 'Done'

         call export_1C_SF(m,u_exact,dir,'u_exact',0)
         call export_1C_SF(m,f,dir,'f',0)


         ! *************************************************************
         ! *************************************************************
         ! *************************************************************
         NU = newAndOpen('out\','norm_JAC')

         name = 'JAC'
         dt = 0.01_cp
         call setMaxIterations(ss,100) ! Cell centered data
         call assign(u,0.0_cp)
         call init(JAC,u,m,sigma,dt)
         call solve(JAC,u,f,sigma,m,ss,norm_res,.true.,NU)
         call delete(JAC)
         call compute_Au(lapU,u,sigma,m,temp,R,dt)
         call subtract(e,u,u_exact)
         call subtract(R,lapU,f)
         call zeroGhostPoints(R)
         call export_1C_SF(m,R,dir,'R_'//name,0)
         call export_1C_SF(m,u,dir,'u_'//name,0)
         call export_1C_SF(m,e,dir,'e_'//name,0)
         call subtract(u,u_exact)
         call compute(norm_e,u,m)
         call print(norm_e,'u_'//name//' vs u_exact')


         ! ! Steady State
         ! name = 'SOR'
         ! call setMaxIterations(ss,100) ! Cell centered data
         ! call assign(u,0.0_cp)
         ! call init(SOR,u,m)
         ! call solve(SOR,u,f,m,ss,norm_res,.true.)
         ! call delete(SOR)
         ! call lap(lapU,u,m)
         ! call subtract(e,u,u_exact)
         ! call subtract(R,lapU,f)
         ! call zeroGhostPoints(R)
         ! call export_1C_SF(m,R,dir,'R_'//name,0)
         ! call export_1C_SF(m,u,dir,'u_'//name,0)
         ! call export_1C_SF(m,e,dir,'e_'//name,0)
         ! call subtract(u,u_exact)
         ! call compute(norm_e,u,m)
         ! call print(norm_e,'u_'//name//' vs u_exact')

         call delete(m)
         call delete(g)
         call delete(g2)
         call delete(e)
         call delete(u)
         call delete(temp_SF)
         call delete(sigma)
         call delete(sig)
         call delete(f)
         call delete(temp)
         call delete(u_exact)
         call delete(lapU)
         call delete(R)
       end subroutine

       end module
