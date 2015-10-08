       module convergenceRate_mod
       ! Very good tutorial for convergence rates:
       ! http://www.grc.nasa.gov/WWW/wind/valid/tutorial/spatconv.html
       use simParams_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
       use IO_auxiliary_mod
       use ops_aux_mod

       use grid_mod
       use mesh_mod
       use norms_mod
       use VF_mod
       use richardsonExtrapolation_mod

       use MOONS_mod

       implicit none
       private
       public :: convergenceRateTest
       public :: computeCRFromExisting

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

       subroutine convergenceRateTest(directory)
         ! Convergence rate tests begin at the finest grid
         ! and progress towards coarser grids. This way,
         ! the longest simulation time is known shortly after
         ! the simulation starts.
         ! 
         ! The following analysis closely followed work by
         ! 
         !      Roache, P. J. Quantification of Uncertainty in Computational 
         !      Fluid Dynamics. Annu. Rev. Fluid Mech. 29, 123–160 (1997).
         ! and
         !      De Vahl Davis, G. Natural convection of air in a square cavity: a 
         !      benchmark solution. Int. J. Num. Methods Fluids 3, 249–264 (1983).
         ! 
         ! Index 1 indicates finest grid.
         ! Index Nsims indicates coarsest grid.
         ! 
         ! For convenience
         !                            2^2  = 4
         !                            2^3  = 8
         !                            2^4  = 16
         !                            2^5  = 32
         !                            2^6  = 64
         !                            2^7  = 128
         !                            2^8  = 256
         !                            2^9  = 512
         !                            2^10 = 1024
         ! 
         ! For 2D and 3D simulations, the refinement factor must be changed accordingly.
         ! 
         implicit none
         character(len=*),intent(in) :: directory
         integer,parameter :: Nstart = 4
         integer,parameter :: Nsims = 4
         integer,parameter :: r0 = 2 ! Magnetiude of refinement factor
         integer,dimension(3),parameter :: r = (/r0,r0,r0/) ! Refinement factor
         integer :: i
         integer,dimension(Nsims) :: Ni = (/(r0**i,i=Nstart,Nstart+Nsims-1)/)
         ! integer,dimension(Nsims) :: Ni = (/(r0**i,i=Nstart+Nsims-1)/)
         type(richardsonExtrapolation),dimension(Nsims-2) :: RE
         type(VF),dimension(Nsims) :: U,B
         type(mesh),dimension(Nsims) :: mesh_mom,mesh_ind
         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'parametric Ni = ',Ni
         Ni = Ni(Nsims:1:-1)
         write(*,*) 'parametric Ni = ',Ni

         if (Nsims.lt.3) stop 'Error: size(f) must > 3 for convergence rate test.'

         do i=1,Nsims ! Start with finest grid
           call MOONS_Parametric(U(i),B(i),mesh_mom(i),mesh_ind(i),Ni(i),&
           directory//'N_'//trim(adjustl(int2str2(Ni(i))))//'\')
         enddo

         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'parametric Ni = ',Ni

         RE = computeRe(U,mesh_mom,Nsims,r,1,directory,'U')
         call reportResults(RE,'U',directory,Nsims,Ni)
         RE = computeRe(U,mesh_mom,Nsims,r,2,directory,'V')
         call reportResults(RE,'V',directory,Nsims,Ni)
         RE = computeRe(U,mesh_mom,Nsims,r,3,directory,'W')
         call reportResults(RE,'W',directory,Nsims,Ni)
         do i=1,Nsims; call delete(U(i)); enddo
          
         if (solveInduction) then
           RE = computeRe(B,mesh_ind,Nsims,r,1,directory,'Bx')
           call reportResults(RE,'Bx',directory,Nsims,Ni)
           RE = computeRe(B,mesh_ind,Nsims,r,2,directory,'By')
           call reportResults(RE,'By',directory,Nsims,Ni)
           RE = computeRe(B,mesh_ind,Nsims,r,3,directory,'Bz')
           call reportResults(RE,'Bz',directory,Nsims,Ni)
           do i=1,Nsims; call delete(B(i)); enddo
         endif
          
         write(*,*) '***************************************************'
         write(*,*) '********** CONVERGENCE TEST COMPLETE **************'
         write(*,*) '***************************************************'
       end subroutine

       subroutine computeCRFromExisting(directory)
         implicit none
         character(len=*),intent(in) :: directory
         integer,parameter :: Nstart = 4
         integer,parameter :: Nsims = 4
         integer,parameter :: r0 = 2 ! Magnetiude of refinement factor
         integer,dimension(3),parameter :: r = (/r0,r0,r0/) ! Refinement factor
         integer :: i
         integer,dimension(Nsims) :: Ni = (/(r0**i,i=Nstart,Nstart+Nsims-1)/)
         type(richardsonExtrapolation),dimension(Nsims-2) :: RE
         type(VF),dimension(Nsims) :: U,B
         type(mesh),dimension(Nsims) :: mesh_mom,mesh_ind
         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'parametric Ni = ',Ni
         Ni = Ni(Nsims:1:-1)
         write(*,*) 'parametric Ni = ',Ni

         do i=1,Nsims ! Start with finest grid
           call MOONS_Parametric(U(i),B(i),mesh_mom(i),mesh_ind(i),Ni(i),&
           directory//'N_'//trim(adjustl(int2str2(Ni(i))))//'\')
         enddo

         do i=1,Nsims ! Start with finest grid
           call loadData(U(i),mesh_mom(i),&
            directory//'N_'//trim(adjustl(int2str2(Ni(i))))//'\Ufield\',&
            'uni,vni,wni',1)
         enddo

         RE = computeRe(U,mesh_mom,Nsims,r,1,directory,'U')
         call reportResults(RE,'U',directory,Nsims,Ni)
         RE = computeRe(U,mesh_mom,Nsims,r,2,directory,'V')
         call reportResults(RE,'V',directory,Nsims,Ni)
         RE = computeRe(U,mesh_mom,Nsims,r,3,directory,'W')
         call reportResults(RE,'W',directory,Nsims,Ni)

         ! Clean up
         do i=1,Nsims
           call delete(U(i))
           call delete(mesh_mom(i))
         enddo
       end subroutine

       end module
