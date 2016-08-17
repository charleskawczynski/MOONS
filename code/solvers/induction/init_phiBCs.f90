       module init_phiBCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       implicit none

       private
       public :: init_phiBCs

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       ! Default = dirichlet on all sides
       integer :: preDefinedphi_BCs = 0 ! see cases in init_phiBCs
       
       contains

       subroutine init_phiBCs(phi,m)
         implicit none
         type(SF),intent(inout) :: phi
         type(mesh),intent(in) :: m
         integer :: i,k,pd
         call init_BC_mesh(phi,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         do i=1,m%s
           call init_Dirichlet(phi%RF(i)%b)
           call init(phi%RF(i)%b,0.0_cp)
           do k=1,3
             pd = periodic_dir(k)
             if ((pd.ne.1).and.(pd.ne.0)) stop 'Error: periodic_dir must = 1,0 in init_phiBCs in init_phiBCs.f90'
             if (pd.eq.1) call makePeriodic(phi%RF(i)%b,k)
           enddo
         enddo
         phi%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preDefinedphi_BCs)
         case (0)
         case (1); call periodic_duct_flow(phi)
         case default; stop 'Error: preDefinedphi_BCs must = 1:5 in init_phiBCs in init_phiBCs.f90.'
         end select
       end subroutine

       subroutine periodic_duct_flow(phi)
         implicit none
         type(SF),intent(inout) :: phi
         call init_periodic(phi%RF(1)%b,1)
         call init_periodic(phi%RF(1)%b,2)
       end subroutine

       subroutine makePeriodic(phi_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: phi_bcs
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(phi_bcs,1)
                   call init_periodic(phi_bcs,2)
         case (2); call init_periodic(phi_bcs,3)
                   call init_periodic(phi_bcs,4)
         case (3); call init_periodic(phi_bcs,5)
                   call init_periodic(phi_bcs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic in init_phiBCs.f90'
         end select
       end subroutine

       end module