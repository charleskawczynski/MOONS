       module init_BBCs_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       use VF_mod
       implicit none

       private

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       integer :: preDefinedB_BCs = 0
       !                                      0 : B = 0
       !                                      1 : Psuedo-vaccuum BCs (dBn/dn = 0, B_tangential = 0)
       !                                      2 : Bandaru


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: init_BBCs

       contains

       subroutine init_BBCs(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i,k,pd

         call init_BC_mesh(B%x,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%y,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%z,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         do i=1,m%s
           call init_Dirichlet(B%x%RF(i)%b)
           call init_Dirichlet(B%y%RF(i)%b)
           call init_Dirichlet(B%z%RF(i)%b)
           call init(B%x%RF(i)%b,0.0_cp)
           call init(B%y%RF(i)%b,0.0_cp)
           call init(B%z%RF(i)%b,0.0_cp)
           do k=1,3
             pd = periodic_dir(k)
             if ((pd.ne.1).and.(pd.ne.0)) stop 'Error: periodic_dir must = 1,0 in init_BBCs in init_BBCs.f90'
             if (pd.eq.1) call makePeriodic(B%x%RF(i)%b,B%y%RF(i)%b,B%z%RF(i)%b,k)
           enddo
         enddo

         select case (preDefinedB_BCs)
         case (0); 
         case (1); call pseudo_vacuum(B,m)
         case (2); call initBandaru(B)
         case default; stop 'Error: preDefinedU_BCs must = 1:5 in init_UBCs in init_UBCs.f90'
         end select
       end subroutine

       subroutine pseudo_vacuum(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,m%s
           call init_Neumann(B%x%RF(i)%b,1)
           call init_Neumann(B%x%RF(i)%b,2)
           call init_Neumann(B%y%RF(i)%b,3)
           call init_Neumann(B%y%RF(i)%b,4)
           call init_Neumann(B%z%RF(i)%b,5)
           call init_Neumann(B%z%RF(i)%b,6)
         enddo
       end subroutine

       subroutine initBandaru(B)
         implicit none
         type(VF),intent(inout) :: B
         call init_Neumann(B%x%RF(1)%b,5)
         call init_Neumann(B%x%RF(1)%b,6)
         call init(B%x%RF(1)%b,0.0_cp,5)
         call init(B%x%RF(1)%b,0.0_cp,6)
       end subroutine

       subroutine makePeriodic(Bx_BCs,By_BCs,Bz_BCs,dir)
         implicit none
         type(BCs),intent(inout) :: Bx_BCs,By_BCs,Bz_BCs
         integer,intent(in) :: dir
         select case (dir)
         case (1);call init_periodic(Bx_BCs,1)
                  call init_periodic(By_BCs,1)
                  call init_periodic(Bz_BCs,1)
                  call init_periodic(Bx_BCs,2)
                  call init_periodic(By_BCs,2)
                  call init_periodic(Bz_BCs,2)
         case (2);call init_periodic(Bx_BCs,3)
                  call init_periodic(By_BCs,3)
                  call init_periodic(Bz_BCs,3)
                  call init_periodic(Bx_BCs,4)
                  call init_periodic(By_BCs,4)
                  call init_periodic(Bz_BCs,4)
         case (3);call init_periodic(Bx_BCs,5)
                  call init_periodic(By_BCs,5)
                  call init_periodic(Bz_BCs,5)
                  call init_periodic(Bx_BCs,6)
                  call init_periodic(By_BCs,6)
                  call init_periodic(Bz_BCs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic in ini_BBCs.f90'
         end select
       end subroutine

       end module