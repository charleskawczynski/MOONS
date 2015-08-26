       module initializeUBCs_mod
       use grid_mod
       use BCs_mod
       use VF_mod
       use SF_mod
       use vectorBCs_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private
       public :: initUBCs

       integer,dimension(3),parameter :: periodic_dir = (/0,0,0/) ! 1 = true, else false

       integer,parameter :: preDefinedU_BCs = 1
       !                                      0 : User-defined case in initUserUBCs() (no override)
       !                                      1 : Lid Driven Cavity (3D)
       !                                      2 : No Slip Cavity
       !                                      3 : Duct Flow (Uniform Inlet)
       !                                      4 : Duct Flow (Fully Developed Inlet)
       !                                      5 : Duct Flow (Neumann Inlet/Outlet)
       !                                      6 : Duct Flow (Neumann Inlet / Periodic Outlet)
       !                                      7 : Cylinder Driven Cavity Flow (tornado)
       !                                      8 : Lid Driven Cavity (2D)
       !                                      9 : ??..
       !                                      10 : Periodic Duct (Bandaru)

       ! Lid Driven Cavity parameters:
       integer,parameter :: drivenFace      = 4 ! (1,2,3,4,5,6) = (x_min,x_max,y_min,y_max,z_min,z_max)

       ! integer,parameter :: drivenFace      = 4 
       !                                      1 {x_min}
       !                                      2 {x_max}
       !                                      3 {y_min}
       !                                      4 {y_max}
       !                                      5 {z_min}
       !                                      6 {z_max}

       integer,parameter :: drivenDirection = 1 ! (1,2,3) = (x,y,z)
       integer,parameter :: drivenSign      = 1 ! (-1,1) = {(-x,-y,-z),(x,y,z)}
       ! Duct Flow parameters: 
       integer,parameter :: ductDirection   = 1 ! (1,2,3) = (x,y,z)
       ! ductSign may or may not work. Look into implementation
       integer,parameter :: ductSign        = 1 ! (-1,1) = {(-x,-y,-z),(x,y,z)}
       ! Cylinder Driven Cavity parameters: 
       ! (not yet developed/used)
       integer,parameter :: cylinderFace    = 1 ! (1,2,3,4,5,6) = (x_min,x_max,y_min,y_max,z_min,z_max)
       integer,parameter :: cylinderSign    = 1 ! (-1,1) = {clockwise from +, clockwise from -}


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: PI = 3.14159265358979_cp
       
       contains

       subroutine initUBCs(U,p,g)
         implicit none
         ! Auxiliary data types
         type(VF),intent(inout) :: U
         type(SF),intent(inout) :: p
         type(grid),intent(in) :: g
         if (preDefinedU_BCs.ne.0) then
           call initPredefinedUBCs(U%x%RF(1)%b,U%y%RF(1)%b,U%z%RF(1)%b,p%RF(1)%b,g)
         else
           call initUserUBCs(U%x%RF(1)%b,U%y%RF(1)%b,U%z%RF(1)%b,p%RF(1)%b,g)
         endif
         call setGrid(U%x%RF(1)%b,g)
         call setGrid(U%y%RF(1)%b,g)
         call setGrid(U%z%RF(1)%b,g)
         call setGrid(p%RF(1)%b,g)

         call checkBCs(U%x%RF(1)%b)
         call checkBCs(U%y%RF(1)%b)
         call checkBCs(U%z%RF(1)%b)
         call checkBCs(p%RF(1)%b)
       end subroutine

       subroutine initPredefinedUBCs(u_bcs,v_bcs,w_bcs,p_bcs,g)
         implicit none
         ! Auxiliary data types
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: p_bcs
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer :: Nx,Ny,Nz,i

         ! Default P-Field BCs = neumann (zero slope)
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         call setAllZero(p_bcs,Nx,Ny,Nz,5)

         ! Default U-Field BCs = no slip
         call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,g)

         select case (preDefinedU_BCs)
         case (1); call lidDrivenBCs(u_bcs,v_bcs,w_bcs,g,drivenFace,drivenDirection,drivenSign)

         case (2); ! Leave default

         case (3); call ductFlow_Uniform_IO(u_bcs,v_bcs,w_bcs,g,ductDirection,-1)
                   call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)
                   call ductFlow_dirichletP_IO(p_bcs,ductDirection,1)

         case (4); call ductFlow_fullyDevelopedProfile(u_bcs,v_bcs,w_bcs,g,ductDirection,ductSign)

         case (5); call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

         case (6); ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

                   call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)
                   call ductFlow_periodicP_IO(p_bcs,ductDirection,-1)
                   call ductFlow_periodicP_IO(p_bcs,ductDirection,1)

         ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
         ! call ductFlow_Periodic_IO(u_bcs,v_bcs,w_bcs,g,ductDirection,1)

         case (7); call cylinderDrivenBCs(u_bcs,v_bcs,w_bcs,g,1)
         case (8); call lidDrivenBCs(u_bcs,v_bcs,w_bcs,g,drivenFace,drivenDirection,drivenSign)
         case (9); 
         case (10)
                   ! call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   ! call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)
                   ! call ductFlow_periodicP_IO(p_bcs,ductDirection,-1)
                   ! call ductFlow_periodicP_IO(p_bcs,ductDirection,1)
         case default
           stop 'Error: preDefinedU_BCs must = 1:5 in initPredefinedUBCs.'
         end select

         do i=1,3
           select case (periodic_dir(i))
           case (0)
           case (1); call makePeriodic(u_bcs,v_bcs,w_bcs,p_bcs,i)
           case default
           stop 'Error: periodic_dir must = 1,0 in initPredefinedUBCs in initializeUBCs.f90'
           end select
         enddo
       end subroutine

       subroutine initUserUBCs(u_bcs,v_bcs,w_bcs,p_bcs,g)
         implicit none
         ! Auxiliary data types
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: p_bcs
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer :: Nx,Ny,Nz
         ! U-field boundary conditions
         call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,g)

         call lidDrivenBCs(u_bcs,v_bcs,w_bcs,g,&
          drivenFace,drivenDirection,drivenSign)

         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         call setAllZero(p_bcs,Nx,Ny,Nz,5)
       end subroutine

       subroutine lidDrivenBCs(u_bcs,v_bcs,w_bcs,g,face,dir,posNeg)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: face,dir,posNeg
         real(cp),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz

         select case(dir)
         case (1); Nx = g%c(1)%sn; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         case (2); Nx = g%c(1)%sc; Ny = g%c(2)%sn; Nz = g%c(3)%sc
         case (3); Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sn
         case default
         stop 'Error: dir must = 1,2,3 in lidDrivenBCs.'
         end select

         select case (face)
         case (1) ! xmin
           allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); stop 'Lid driven BCs is violating flow through.'
           case (2); call setXminVals(v_bcs,bvals)
           case (3); call setXminVals(w_bcs,bvals)
           end select

         case (2) ! xmax
           allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); stop 'Lid driven BCs is violating flow through.'
           case (3); call setXmaxVals(v_bcs,bvals)
           case (2); call setXmaxVals(w_bcs,bvals)
           end select

         case (3) ! ymin
           allocate(bvals(Nx,Nz)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); call setYminVals(u_bcs,bvals)
           case (2); stop 'Lid driven BCs is violating flow through.'
           case (3); call setYminVals(w_bcs,bvals)
           end select

         case (4) ! ymax
           allocate(bvals(Nx,Nz)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); call setYmaxVals(u_bcs,bvals)
           case (2); stop 'Lid driven BCs is violating flow through.'
           case (3); call setYmaxVals(w_bcs,bvals)
           end select

         case (5) ! zmin
           allocate(bvals(Nx,Ny)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); call setZminVals(u_bcs,bvals)
           case (2); call setZminVals(v_bcs,bvals)
           case (3); stop 'Lid driven BCs is violating flow through.'
           end select

         case (6) ! zmax
           allocate(bvals(Nx,Ny)); bvals = sign(1.0_cp,real(posNeg,cp));
           select case (dir)
           case (1); call setZmaxVals(u_bcs,bvals)
           case (2); call setZmaxVals(v_bcs,bvals)
           case (3); stop 'Lid driven BCs is violating flow through.'
           end select

         end select
         deallocate(bvals)
       end subroutine

       subroutine makePeriodic(u_bcs,v_bcs,w_bcs,p_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs,p_bcs
         integer,intent(in) :: dir
         integer :: periodic_c,periodic_i
         periodic_c = 6 ! Wall coincident
         periodic_i = 7 ! Wall incoincident

         select case (dir)
         case (1)
         call setXminType(u_bcs,periodic_c)
         call setXminType(v_bcs,periodic_i)
         call setXminType(w_bcs,periodic_i)
         call setXminType(p_bcs,periodic_i)
         call setXmaxType(u_bcs,periodic_c)
         call setXmaxType(v_bcs,periodic_i)
         call setXmaxType(w_bcs,periodic_i)
         call setXmaxType(p_bcs,periodic_i)
         case (2)
         call setYminType(u_bcs,periodic_i)
         call setYminType(v_bcs,periodic_c)
         call setYminType(w_bcs,periodic_i)
         call setYminType(p_bcs,periodic_i)
         call setYmaxType(u_bcs,periodic_i)
         call setYmaxType(v_bcs,periodic_c)
         call setYmaxType(w_bcs,periodic_i)
         call setYmaxType(p_bcs,periodic_i)
         case (3)
         call setZminType(u_bcs,periodic_i)
         call setZminType(v_bcs,periodic_i)
         call setZminType(w_bcs,periodic_c)
         call setZminType(p_bcs,periodic_i)
         call setZmaxType(u_bcs,periodic_i)
         call setZmaxType(v_bcs,periodic_i)
         call setZmaxType(w_bcs,periodic_c)
         call setZmaxType(p_bcs,periodic_i)
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeUBCs.f90'
         end select
       end subroutine

       subroutine ductFlow_Uniform_IO(u_bcs,v_bcs,w_bcs,g,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: ductDir,IO
         real(cp),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz

         select case(ductDir)
         case (1); Nx = g%c(1)%sn; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         case (2); Nx = g%c(1)%sc; Ny = g%c(2)%sn; Nz = g%c(3)%sc
         case (3); Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sn
         case default
         stop 'Error: ductDir must = 1,2,3 in ductFlow_Uniform_IO.'
         end select

         select case (ductDir)
         case (1)
           select case (IO)
           case (-1)
             call setXminType(u_bcs,1) ! Dirichlet
             allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setXminVals(u_bcs,bvals); deallocate(bvals)
           case (1)
             call setXmaxType(u_bcs,1) ! Dirichlet
             allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setXmaxVals(u_bcs,bvals); deallocate(bvals)
           case default; stop 'IO must = 1,-1 in ductFlow_Uniform_IO'
           end select
         case (2)
           select case (IO)
           case (-1)
             call setYminType(v_bcs,1) ! Dirichlet
             allocate(bvals(Nx,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setYminVals(v_bcs,bvals); deallocate(bvals)
           case (1)
             call setYmaxType(v_bcs,1) ! Dirichlet
             allocate(bvals(Nx,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setYmaxVals(v_bcs,bvals); deallocate(bvals)
           case default; stop 'IO must = 1,-1 in ductFlow_Uniform_IO'
           end select
         case (3)
           select case (IO)
           case (-1)
             call setZminType(w_bcs,1) ! Dirichlet
             allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setZminVals(w_bcs,bvals); deallocate(bvals)
           case (1)
             call setZmaxType(w_bcs,1) ! Dirichlet
             allocate(bvals(Ny,Nz)); bvals = sign(1.0_cp,real(IO,cp))
             call setZmaxVals(w_bcs,bvals); deallocate(bvals)
           case default; stop 'IO must = 1,-1 in ductFlow_Uniform_IO'
           end select
         end select
       end subroutine

       subroutine ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: ductDir,IO
         integer :: neumann_n,neumann_t
         neumann_n = 3
         neumann_t = 5

         select case (ductDir)
         case (1)
           select case (IO)
           case (-1)
             call setXminType(u_bcs,neumann_n)
             call setXminType(v_bcs,neumann_t)
             call setXminType(w_bcs,neumann_t)
           case (1)
             call setXmaxType(u_bcs,neumann_n)
             call setXmaxType(v_bcs,neumann_t)
             call setXmaxType(w_bcs,neumann_t)
           case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
           end select
         case (2)
           select case (IO)
           case (-1)
             call setYminType(u_bcs,neumann_t)
             call setYminType(v_bcs,neumann_n)
             call setYminType(w_bcs,neumann_t)
           case (1)
             call setYmaxType(u_bcs,neumann_t)
             call setYmaxType(v_bcs,neumann_n)
             call setYmaxType(w_bcs,neumann_t)
           case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
           end select
         case (3)
           select case (IO)
           case (-1)
             call setZminType(u_bcs,neumann_t)
             call setZminType(v_bcs,neumann_t)
             call setZminType(w_bcs,neumann_n)
           case (1)
             call setZmaxType(u_bcs,neumann_t)
             call setZmaxType(v_bcs,neumann_t)
             call setZmaxType(w_bcs,neumann_n)
           case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
           end select
         end select
       end subroutine

       subroutine ductFlow_Periodic_IO(u_bcs,v_bcs,w_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: ductDir,IO
         integer :: periodic_c,periodic_i
         periodic_c = 6 ! Wall coincident
         periodic_i = 7 ! Wall incoincident

         select case (ductDir)
         case (1)
           select case (IO)
           case (-1)
             call setXminType(u_bcs,periodic_c)
             call setXminType(v_bcs,periodic_i)
             call setXminType(w_bcs,periodic_i)
           case (1)
             call setXmaxType(u_bcs,periodic_c)
             call setXmaxType(v_bcs,periodic_i)
             call setXmaxType(w_bcs,periodic_i)
           case default; stop 'IO must = 1,-1 in ductFlow_Periodic_IO'
           end select
         case (2)
           select case (IO)
           case (-1)
             call setYminType(u_bcs,periodic_i)
             call setYminType(v_bcs,periodic_c)
             call setYminType(w_bcs,periodic_i)
           case (1)
             call setYmaxType(u_bcs,periodic_i)
             call setYmaxType(v_bcs,periodic_c)
             call setYmaxType(w_bcs,periodic_i)
           case default; stop 'IO must = 1,-1 in ductFlow_Periodic_IO'
           end select
         case (3)
           select case (IO)
           case (-1)
             call setZminType(u_bcs,periodic_i)
             call setZminType(v_bcs,periodic_i)
             call setZminType(w_bcs,periodic_c)
           case (1)
             call setZmaxType(u_bcs,periodic_i)
             call setZmaxType(v_bcs,periodic_i)
             call setZmaxType(w_bcs,periodic_c)
           case default; stop 'IO must = 1,-1 in ductFlow_Periodic_IO'
           end select
         end select
       end subroutine

       subroutine noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs,g)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer :: Nx,Ny,Nz

         Nx = g%c(1)%sn; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         call setAllZero(u_bcs,Nx,Ny,Nz,2)
         call setXminType(u_bcs,1)
         call setXmaxType(u_bcs,1)

         Nx = g%c(1)%sc; Ny = g%c(2)%sn; Nz = g%c(3)%sc
         call setAllZero(v_bcs,Nx,Ny,Nz,2)
         call setYminType(v_bcs,1)
         call setYmaxType(v_bcs,1)

         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sn
         call setAllZero(w_bcs,Nx,Ny,Nz,2)
         call setZminType(w_bcs,1)
         call setZmaxType(w_bcs,1)
       end subroutine

       subroutine ductFlow_dirichletP_IO(p_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: ductDir,IO
         integer :: dirichlet
         dirichlet = 2

         select case (IO)
         case (-1)
           select case (ductDir)
           case (1); call setXminType(p_bcs,dirichlet)
           case (2); call setYminType(p_bcs,dirichlet)
           case (3); call setZminType(p_bcs,dirichlet)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_dirichletP_IO'
           end select
         case (1)
           select case (ductDir)
           case (1); call setXmaxType(p_bcs,dirichlet)
           case (2); call setYmaxType(p_bcs,dirichlet)
           case (3); call setZmaxType(p_bcs,dirichlet)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_dirichletP_IO'
           end select
         case default; stop 'Error: IO must = -1,1 in ductFlow_dirichletP_IO'
         end select
       end subroutine

       subroutine ductFlow_periodicP_IO(p_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: ductDir,IO
         integer :: periodic_i
         periodic_i = 7 ! Wall incoincident

         select case (IO)
         case (-1)
           select case (ductDir)
           case (1); call setXminType(p_bcs,periodic_i)
           case (2); call setYminType(p_bcs,periodic_i)
           case (3); call setZminType(p_bcs,periodic_i)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_periodicP_IO'
           end select
         case (1)
           select case (ductDir)
           case (1); call setXmaxType(p_bcs,periodic_i)
           case (2); call setYmaxType(p_bcs,periodic_i)
           case (3); call setZmaxType(p_bcs,periodic_i)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_periodicP_IO'
           end select
         case default; stop 'Error: IO must = -1,1 in ductFlow_periodicP_IO'
         end select
       end subroutine

       subroutine cylinderDrivenBCs(u_bcs,v_bcs,w_bcs,g,dir)
         implicit none
         ! Auxiliary data types
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         real(cp),dimension(:,:),allocatable :: bvals
         ! Coordinates
         real(cp),dimension(:),allocatable :: xc,yc,zc
         real(cp),dimension(:),allocatable :: xn,yn,zn
         real(cp) :: x_0,y_0,z_0
         real(cp) :: x_N,y_N,z_N
         real(cp) :: x0,y0,z0
         real(cp) :: r,omega0,theta,r0
         integer :: Nx,Ny,Nz,i,j,k
         real(cp),dimension(3) :: hmin,hmax

         hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
         hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)

         x_0 = hmin(1); y_0 = hmin(2); z_0 = hmin(3)
         x_N = hmax(1); y_N = hmax(2); z_N = hmax(3)

         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         allocate(xn(g%c(1)%sn),yn(g%c(2)%sn),zn(g%c(3)%sn))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         xn = g%c(1)%hn; yn = g%c(2)%hn; zn = g%c(3)%hn

         omega0 = 1.0_cp
         r0 = 0.05_cp
         x0 = (x_N + x_0)/2.0_cp
         y0 = (y_N + y_0)/2.0_cp
         z0 = (z_N + z_0)/2.0_cp

         select case (dir)
         case (1) ! u = 0
           ! v_bcs
           Nx = g%c(1)%sc; Ny = g%c(2)%sn; Nz = g%c(3)%sc
           call setAllZero(v_bcs,Nx,Ny,Nz,2)
           allocate(bvals(Ny,Nz));
           do j=1,Ny
             do k=1,Nz
               r = sqrt((yn(j)-y0)**2.0_cp + (zc(k)-z0)**2.0_cp)
               if (r.lt.r0) then
                 theta = atan2(zc(k),yn(j))
                 bvals(j,k) = -omega0*r*sin(theta)
               endif
             enddo
           enddo
           call setXmaxVals(v_bcs,bvals); deallocate(bvals)
           call setYminType(v_bcs,1)
           call setYmaxType(v_bcs,1)
           
           ! w_bcs
           Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sn
           call setAllZero(w_bcs,Nx,Ny,Nz,2)
           allocate(bvals(Nx,Nz));
           do j=1,Ny
             do k=1,Nz
               r = sqrt((yc(j)-y0)**2.0_cp + (zn(k)-z0)**2.0_cp)
               if (r.lt.r0) then
                 theta = atan2(zn(k),yc(j))
                 bvals(j,k) = omega0*r*cos(theta)
               endif
             enddo
           enddo
           call setXmaxVals(w_bcs,bvals); deallocate(bvals)
           call setZminType(w_bcs,1)
           call setZmaxType(w_bcs,1)

         case (2) ! v = 0
           ! v_bcs
           Nx = g%c(1)%sc; Ny = g%c(2)%sn; Nz = g%c(3)%sc
           call setAllZero(v_bcs,Nx,Ny,Nz,2)
           call setYminType(v_bcs,1)
           call setYmaxType(v_bcs,1)
           i = 1

         case (3) ! w = 0
           ! v_bcs
           Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sn
           call setAllZero(w_bcs,Nx,Ny,Nz,2)
           call setYminType(w_bcs,1)
           call setYmaxType(w_bcs,1)

         end select
       end subroutine

       subroutine ductFlow_fullyDevelopedProfile(u_bcs,v_bcs,w_bcs,g,dir,posNeg)
         ! This routine initializes a fully developed duct flow
         ! profile along direction dir.
         ! 
         ! There is a copy of this routine inside initUBCs.
         ! THE MASTER COPY OF THIS ROUTINE SHOULD
         ! RESIDE IN INITIALIZE UFIELD, NOT
         ! INITIALIZE UBCs
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,posNeg

         integer :: i,j,imax,jmax
         real(cp),dimension(:),allocatable :: hx,hy
         real(cp) :: alpha,height,width,F,A,A1,A2,A3
         real(cp),dimension(:,:),allocatable :: u_temp
         real(cp),dimension(3) :: hmin,hmax
         integer :: n,m,nMax,mMax
         integer,dimension(3) :: s,Ni

         hmin = (/g%c(1)%hmin,g%c(2)%hmin,g%c(3)%hmin/)
         hmax = (/g%c(1)%hmax,g%c(2)%hmax,g%c(3)%hmax/)
         Ni = (/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)

         ! For max number of iterations in 
         ! infinite series solution:
         nMax = 100; mMax = 100
         F = 1.0_cp

         select case (dir)
         case (1) ! u(y,z)
           s = g%c(1)%sn
           width = (hmax(2) - hmin(2))/2.0_cp
           height = (hmax(3) - hmin(3))/2.0_cp
           imax = Ni(2); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(2)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(2),s(3)))
         case (2) ! v(x,z)
           s = g%c(2)%sn
           width = (hmax(1) - hmin(1))/2.0_cp
           height = (hmax(3) - hmin(3))/2.0_cp
           imax = Ni(1); jmax = Ni(3)
           allocate(hx(imax)); hx = g%c(1)%hc
           allocate(hy(jmax)); hy = g%c(3)%hc
           allocate(u_temp(s(1),s(3)))
         case (3) ! w(x,y)
           s = g%c(3)%sn
           width = (hmax(1) - hmin(1))/2.0_cp
           height = (hmax(2) - hmin(2))/2.0_cp
           imax = Ni(1); jmax = Ni(2)
           allocate(hx(imax)); hx = g%c(1)%hc
           allocate(hy(jmax)); hy = g%c(2)%hc
           allocate(u_temp(s(1),s(2)))
         case default
         stop 'Error: dir must = 1,2,3 in ductFlow_fullyDevelopedProfile.'
         end select
         alpha = width/height

         do i=1,imax
           do j=1,jmax
             do m=1,mMax
               do n=1,nMax
               A1 = real(16.0,cp)*F*alpha**2.0_cp*height**2.0_cp/&
               ((real(m,cp)*PI)**2.0_cp+(alpha*real(n,cp)*PI)**2.0_cp)
               A2 = 1.0_cp/(real(m,cp)*PI)*1.0_cp/(real(n,cp)*PI)
               A3 = (1.0_cp-cos(real(m,cp)*PI))*(1.0_cp-cos(real(n,cp)*PI))
               A = A1*A2*A3
               u_temp(i,j) = u_temp(i,j) + A*sin(real(m,cp)*PI*(hx(i)-hmin(1))/(2.0_cp*width))*&
                                             sin(real(n,cp)*PI*(hy(j)-hmin(2))/(2.0_cp*height))
               enddo
             enddo
           enddo
         enddo

         select case (posNeg)
         case (1)
           select case (dir)
           case (1); call setXminType(u_bcs,1); call setXminVals(u_bcs,u_temp)
           case (2); call setYminType(v_bcs,1); call setXminVals(v_bcs,u_temp)
           case (3); call setZminType(w_bcs,1); call setXminVals(w_bcs,u_temp)
           end select
         case (-1)
           select case (dir)
           case (1); call setXmaxType(u_bcs,1); call setXmaxVals(u_bcs,-u_temp)
           case (2); call setYmaxType(v_bcs,1); call setXmaxVals(v_bcs,-u_temp)
           case (3); call setZmaxType(w_bcs,1); call setXmaxVals(w_bcs,-u_temp)
           end select
         case default
         stop 'Error: posNeg must = 1,-1 in initFullyDevelopedDuctFlow.'
         end select

         deallocate(u_temp)
         deallocate(hx,hy)
       end subroutine

       end module