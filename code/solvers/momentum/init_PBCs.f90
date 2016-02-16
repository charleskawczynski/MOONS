       module init_PBCs_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       implicit none

       private
       public :: init_PBCs

       integer,dimension(3) :: periodic_dir = (/0,0,1/) ! 1 = true, else false

       integer :: preDefinedP_BCs = 1
       !                                      0 : User-defined case in initUserPBCs() (no override)
       !                                      1 : All Neumann
       !                                      2 : Duct Flow (Neumann with dirichlet at exit)

       ! Duct Flow parameters: 
       integer :: ductDirection   = 1 ! (1,2,3) = (x,y,z)
       integer :: ductSign        = 1 ! (-1,1) = {(-x,-y,-z),(x,y,z)}


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

       subroutine init_PBCs(p,m)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         integer :: i
         call init_BC_mesh(p,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         do i=1,m%s
           if (preDefinedP_BCs.ne.0) then
                 call initPredefinedPBCs(p%RF(i)%b)
           else; call initUserPBCs(p%RF(i)%b)
           endif
         enddo

         do i=1,m%s
           call init(p%RF(i)%b,0.0_cp)
         enddo
         ! p%all_Neumann = .true. ! Needs to be adjusted manually
         p%all_Neumann = .false. ! Needs to be adjusted manually

         call init_Dirichlet(p%RF(5)%b,2)
         call init_Dirichlet(p%RF(7)%b,2)
         call init_Dirichlet(p%RF(8)%b,2)

         ! For Tyler's geometry:
         ! call init_Dirichlet(p%RF(4)%b,2)
         ! call init_Dirichlet(p%RF(5)%b,2)
         ! call init_Dirichlet(p%RF(6)%b,2)
         ! call init_Dirichlet(p%RF(10)%b,2)
         ! call init_Dirichlet(p%RF(14)%b,2)

         ! call define_Edges(p%RF(i)%b)
         ! call init_Neumann(p%RF(1)%b,6)
         ! call init_Dirichlet(p%RF(m%s)%b,1) ! Not suree if 1 or 2
       end subroutine

       subroutine initPredefinedPBCs(p_bcs)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer :: i
         ! Default P-Field BCs = neumann
         call init_Neumann(p_bcs)

         select case (preDefinedP_BCs)
         case (1); 
         case (2); call ductFlow_dirichletP_IO(p_bcs,ductDirection,ductSign)
         case (3); call ductFlow_periodicP_IO(p_bcs,ductDirection,-1)
         case default
           stop 'Error: preDefinedP_BCs must = 1:5 in initPredefinedPBCs.'
         end select
         do i=1,3
           select case (periodic_dir(i))
           case (0)
           case (1); call makePeriodic(p_bcs,i)
           case default
           stop 'Error: periodic_dir must = 1,0 in initPredefinedPBCs in initializeUBCs.f90'
           end select
         enddo
       end subroutine

       subroutine initUserPBCs(p_bcs)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         call init_Neumann(p_bcs)
       end subroutine

       subroutine makePeriodic(p_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(p_bcs,1)
                   call init_periodic(p_bcs,2)
         case (2); call init_periodic(p_bcs,3)
                   call init_periodic(p_bcs,4)
         case (3); call init_periodic(p_bcs,5)
                   call init_periodic(p_bcs,6)
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeUBCs.f90'
         end select
       end subroutine

       subroutine ductFlow_dirichletP_IO(p_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: ductDir,IO
         select case (IO)
         case (-1)
           select case (ductDir)
           case (1); call init_Dirichlet(p_bcs,1)
           case (2); call init_Dirichlet(p_bcs,3)
           case (3); call init_Dirichlet(p_bcs,5)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_dirichletP_IO'
           end select
         case (1)
           select case (ductDir)
           case (1); call init_Dirichlet(p_bcs,2)
           case (2); call init_Dirichlet(p_bcs,4)
           case (3); call init_Dirichlet(p_bcs,6)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_dirichletP_IO'
           end select
         case default; stop 'Error: IO must = -1,1 in ductFlow_dirichletP_IO'
         end select
       end subroutine

       subroutine ductFlow_periodicP_IO(p_bcs,ductDir,IO)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: ductDir,IO
         select case (IO)
         case (-1)
           select case (ductDir)
           case (1); call init_periodic(p_bcs,1)
           case (2); call init_periodic(p_bcs,3)
           case (3); call init_periodic(p_bcs,5)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_periodicP_IO'
           end select
         case (1)
           select case (ductDir)
           case (1); call init_periodic(p_bcs,2)
           case (2); call init_periodic(p_bcs,4)
           case (3); call init_periodic(p_bcs,6)
           case default; stop 'Error: ductDir must = 1,2,3 in ductFlow_periodicP_IO'
           end select
         case default; stop 'Error: IO must = -1,1 in ductFlow_periodicP_IO'
         end select
       end subroutine

       end module