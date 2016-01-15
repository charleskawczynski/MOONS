       module init_UBCs_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use VF_mod
       use SF_mod
       use vectorBCs_mod
       use profile_funcs_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private
       public :: init_UBCs

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

       contains

       subroutine init_UBCs(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i

         do i=1,m%s
           call init(U%x%RF(i)%b,m%g(i),U%x%RF(i)%s)
           call init(U%y%RF(i)%b,m%g(i),U%y%RF(i)%s)
           call init(U%z%RF(i)%b,m%g(i),U%z%RF(i)%s)
           if (preDefinedU_BCs.ne.0) then
             call initPredefinedUBCs(U%x%RF(i)%b,U%y%RF(i)%b,U%z%RF(i)%b,m%g(i))
           else
             call initUserUBCs(U%x%RF(i)%b,U%y%RF(i)%b,U%z%RF(i)%b)
           endif
         enddo
         call init_BC_mesh(U%x,m)
         call init_BC_mesh(U%y,m)
         call init_BC_mesh(U%z,m)
         ! call init(U%x%RF(1)%b%e(5),0.4_cp)
         ! call init(U%x%RF(1)%b%e(6),0.5_cp)
         ! call init(U%x%RF(1)%b%e(7),0.6_cp)
         ! call init(U%x%RF(1)%b%e(8),0.7_cp)

         ! call init(U%x%RF(1)%b,1.0_cp,1) ! Inlet (Dirichlet 0 by default)
         ! call init(U%x%RF(2)%b,1.0_cp,1) ! Inlet (Dirichlet 0 by default)
         ! call init(U%x%RF(3)%b,1.0_cp,1) ! Inlet (Dirichlet 0 by default)

         ! call init_Neumann(U%x%RF(2)%b,4) ! L1
         ! call init_Neumann(U%x%RF(3)%b,3) ! R1
         ! call init_Neumann(U%x%RF(4)%b,4) ! L2
         ! call init_Neumann(U%x%RF(5)%b,3) ! R2
         ! call init_Neumann(U%x%RF(6)%b,4) ! L3
         ! call init_Neumann(U%x%RF(7)%b,3) ! R3

         ! call init_Neumann(U%x%RF(6)%b,2);call init_Neumann(U%y%RF(6)%b,2);call init_Neumann(U%z%RF(6)%b,2) ! exit
         ! call init_Neumann(U%x%RF(7)%b,2);call init_Neumann(U%y%RF(7)%b,2);call init_Neumann(U%z%RF(7)%b,2) ! exit
         ! call init_Neumann(U%x%RF(8)%b,2);call init_Neumann(U%y%RF(8)%b,2);call init_Neumann(U%z%RF(8)%b,2) ! exit


         ! call init_Neumann(U%x%RF(1)%b,6)
         ! call init_Neumann(U%y%RF(1)%b,6)
         ! call init_Neumann(U%z%RF(1)%b,6)

         ! call init(U%x%RF(1)%b,1.0_cp,1)
         ! call init_Neumann(U%x%RF(m%s)%b,1)
         ! call init_Neumann(U%y%RF(m%s)%b,1)
         ! call init_Neumann(U%z%RF(m%s)%b,1)
       end subroutine

       subroutine initPredefinedUBCs(u_bcs,v_bcs,w_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer :: i

         ! Default U-Field BCs = no slip
         call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs)

         select case (preDefinedU_BCs)
         case (1); call lidDrivenBCs(u_bcs,v_bcs,w_bcs,drivenFace,drivenDirection,drivenSign)

         case (2); ! Leave default

         case (3); call ductFlow_Uniform_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

         case (4); call ductFlow_FD_Profile(u_bcs,v_bcs,w_bcs,g,ductDirection,ductSign)

         case (5); call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

         case (6); ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

                   call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)

         ! call ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
         ! call ductFlow_Periodic_IO(u_bcs,v_bcs,w_bcs,g,ductDirection,1)

         case (7); call cylinderDrivenBCs(u_bcs,v_bcs,w_bcs,g,1)
         case (8); call lidDrivenBCs(u_bcs,v_bcs,w_bcs,drivenFace,drivenDirection,drivenSign)
         case (9); 
         case (10)
                   ! call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,-1)
                   ! call ductFlow_periodic_IO(u_bcs,v_bcs,w_bcs,ductDirection,1)
         case default
           stop 'Error: preDefinedU_BCs must = 1:5 in initPredefinedUBCs.'
         end select

         do i=1,3
           select case (periodic_dir(i))
           case (0)
           case (1); call makePeriodic(u_bcs,v_bcs,w_bcs,i)
           case default
           stop 'Error: periodic_dir must = 1,0 in initPredefinedUBCs in initializeUBCs.f90'
           end select
         enddo
       end subroutine

       subroutine initUserUBCs(u_bcs,v_bcs,w_bcs)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         call noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs)
         call lidDrivenBCs(u_bcs,v_bcs,w_bcs,&
          drivenFace,drivenDirection,drivenSign)
       end subroutine

       subroutine lidDrivenBCs(u_bcs,v_bcs,w_bcs,face,dir,posNeg)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: face,dir,posNeg
         real(cp) :: bval
         select case (face)
         case (1); if (dir.eq.1) stop 'Lid driven BCs is violating flow through.'
         case (2); if (dir.eq.1) stop 'Lid driven BCs is violating flow through.'
         case (3); if (dir.eq.2) stop 'Lid driven BCs is violating flow through.'
         case (4); if (dir.eq.2) stop 'Lid driven BCs is violating flow through.'
         case (5); if (dir.eq.3) stop 'Lid driven BCs is violating flow through.'
         case (6); if (dir.eq.3) stop 'Lid driven BCs is violating flow through.'
         end select
         bval = sign(1.0_cp,real(posNeg,cp));
         select case (dir)
         case (1); call init(u_bcs,bval,face)
         case (2); call init(v_bcs,bval,face)
         case (3); call init(w_bcs,bval,face)
         end select
       end subroutine

       subroutine makePeriodic(u_bcs,v_bcs,w_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: dir
         integer :: face1,face2
         select case (dir)
         case (1); face1 = 1; face2 = 2
         case (2); face1 = 3; face2 = 4
         case (3); face1 = 5; face2 = 6
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeUBCs.f90'
         end select
         call init_periodic(u_bcs,face1); call init(u_bcs,0.0_cp,face1)
         call init_periodic(v_bcs,face1); call init(v_bcs,0.0_cp,face1)
         call init_periodic(w_bcs,face1); call init(w_bcs,0.0_cp,face1)

         call init_periodic(u_bcs,face2); call init(u_bcs,0.0_cp,face2)
         call init_periodic(v_bcs,face2); call init(v_bcs,0.0_cp,face2)
         call init_periodic(w_bcs,face2); call init(w_bcs,0.0_cp,face2)
       end subroutine

       subroutine ductFlow_Uniform_IO(u_bcs,v_bcs,w_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: ductDir,IO
         real(cp) :: bval
         integer :: face
         face = getFace(ductDir,IO)
         bval = sign(1.0_cp,real(IO,cp))
         select case (ductDir)
         case (1); call init_Dirichlet(u_bcs,face); call init(u_bcs,bval,face)
         case (2); call init_Dirichlet(v_bcs,face); call init(v_bcs,bval,face)
         case (3); call init_Dirichlet(w_bcs,face); call init(w_bcs,bval,face)
         case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_Uniform_IO'
         end select
       end subroutine

       subroutine ductFlow_neumann_IO(u_bcs,v_bcs,w_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: ductDir,IO
         integer :: face
         face = getFace(ductDir,IO)
         call init_Neumann(u_bcs,face)
         call init_Neumann(v_bcs,face)
         call init_Neumann(w_bcs,face)
         call init(u_bcs,0.0_cp,face)
         call init(v_bcs,0.0_cp,face)
         call init(w_bcs,0.0_cp,face)
       end subroutine

       subroutine ductFlow_Periodic_IO(u_bcs,v_bcs,w_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: ductDir,IO
         integer :: face
         face = getFace(ductDir,IO)
         call init_periodic(u_bcs,face)
         call init_periodic(v_bcs,face)
         call init_periodic(w_bcs,face)
         call init(u_bcs,0.0_cp,face)
         call init(v_bcs,0.0_cp,face)
         call init(w_bcs,0.0_cp,face)
       end subroutine

       subroutine noSlipNoFlowThroughBCs(u_bcs,v_bcs,w_bcs)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         call init_Dirichlet(u_bcs)
         call init_Dirichlet(v_bcs)
         call init_Dirichlet(w_bcs)
         call init(u_bcs,0.0_cp)
         call init(v_bcs,0.0_cp)
         call init(w_bcs,0.0_cp)
       end subroutine

       subroutine cylinderDrivenBCs(u_bcs,v_bcs,w_bcs,g,dir)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init(v_bcs,rotatingCylinder(g%c(2),g%c(3),&
          v_bcs%f(1)%s(1),v_bcs%f(1)%s(2),0.1_cp,1.0_cp,1),1)
                   call init(w_bcs,rotatingCylinder(g%c(2),g%c(3),&
          w_bcs%f(1)%s(1),w_bcs%f(1)%s(2),0.1_cp,1.0_cp,2),1)
         case (2); call init(u_bcs,rotatingCylinder(g%c(1),g%c(3),&
          u_bcs%f(2)%s(1),u_bcs%f(2)%s(2),0.1_cp,1.0_cp,1),2)
                   call init(w_bcs,rotatingCylinder(g%c(1),g%c(3),&
          w_bcs%f(2)%s(1),w_bcs%f(2)%s(2),0.1_cp,1.0_cp,2),2)
         case (3); call init(u_bcs,rotatingCylinder(g%c(1),g%c(2),&
          u_bcs%f(3)%s(1),u_bcs%f(3)%s(2),0.1_cp,1.0_cp,1),3)
                   call init(v_bcs,rotatingCylinder(g%c(1),g%c(2),&
          v_bcs%f(3)%s(1),v_bcs%f(3)%s(2),0.1_cp,1.0_cp,2),3)
         end select
       end subroutine

       subroutine ductFlow_FD_Profile(u_bcs,v_bcs,w_bcs,g,dir,posNeg)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,posNeg
         integer :: face
         face = getFace(dir,posNeg)
         select case (dir)
         case (1); call init(u_bcs,init_FD_DuctFlow(g%c(2),g%c(3),&
                    u_bcs%f(face)%s(1),u_bcs%f(face)%s(2)),1)
         case (2); call init(v_bcs,init_FD_DuctFlow(g%c(1),g%c(3),&
                    v_bcs%f(face)%s(1),v_bcs%f(face)%s(2)),2)
         case (3); call init(w_bcs,init_FD_DuctFlow(g%c(1),g%c(2),&
                    w_bcs%f(face)%s(1),w_bcs%f(face)%s(2)),3)
         end select
       end subroutine

       function getFace(ductDir,IO) result(face)
         implicit none
         integer,intent(in) :: ductDir,IO
         integer :: face
         select case (ductDir)
         case (1); select case (IO)
                   case (-1); face = 1
                   case (1);  face = 2
                   case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
                   end select
         case (2); select case (IO)
                   case (-1); face = 3
                   case (1);  face = 4
                   case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
                   end select
         case (3); select case (IO)
                   case (-1); face = 5
                   case (1);  face = 6
                   case default; stop 'IO must = 1,-1 in ductFlow_neumann_IO'
                   end select
         end select
       end function

       end module