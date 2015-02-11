       module momentumSolver_mod
       use simParams_mod
       use constants_mod
       use myAllocate_mod
       use scalarField_mod
       use vectorField_mod

       use initializeUBCs_mod
       use initializeUField_mod

       use myIO_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use vectorOps_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use mySOR_mod
       use myADI_mod
       use myPoisson_mod
       
       implicit none
       private
       
       public :: momentum,initialize,delete,solve
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeDivergence

       type momentum
         character(len=8) :: name = 'momentum'
         type(vectorField) :: U,Ustar,F,TempVF
         type(scalarField) :: p,Temp,divU

         type(BCs) :: u_bcs,v_bcs,w_bcs,p_bcs
         type(solverSettings) :: ss_mom,ss_ppe
         type(myError) :: err_PPE,err_DivU
         type(mySOR) :: SOR_p
         type(myADI) :: ADI_p
         integer :: nstep
       end type

       interface initialize;         module procedure initializeMomentum          ; end interface
       interface delete;             module procedure deleteMomentum              ; end interface
       interface solve;              module procedure solveMomentumEquation       ; end interface
       interface export;             module procedure momentumExport              ; end interface
       interface exportRaw;          module procedure momentumExportRaw           ; end interface
       interface exportTransient;    module procedure momentumExportTransient     ; end interface
       interface printExportBCs;     module procedure printExportMomentumBCs      ; end interface
       interface computeDivergence;  module procedure computeDivergenceMomentum   ; end interface

       contains

       subroutine initializeMomentum(mom,gd,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing momentum:'
         ! Initialize temp fields
         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%U,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%U,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%U,Nx,Ny,Nz)


         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%F,Nx,Ny,Nz)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%TempVF,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%TempVF,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%TempVF,Nx,Ny,Nz)
         ! allocate P-Fields
         call myAllocate(Nx,Ny,Nz,gd,PLoc)
         call allocateField(mom%p,Nx,Ny,Nz)
         call allocateField(mom%divU,Nx,Ny,Nz)
         call allocateField(mom%temp,Nx,Ny,Nz)
         
         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initializeUBCs(mom%u_bcs,mom%v_bcs,mom%w_bcs,mom%p_bcs,gd)
         write(*,*) '     BCs initialized'

         call initializeUfield(mom%U%x,mom%U%y,mom%U%z,mom%p%phi,gd,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)
         call applyAllBCs(mom%p_bcs,mom%p%phi,gd)
         write(*,*) '     BCs applied'
         ! Initialize solver settings
         call initializeSolverSettings(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,5)
         call setSubtractMean(mom%ss_ppe)
         write(*,*) '     Solver settings initialized'
         mom%nstep = 0
         !   call setMinTolerance(ss_ppe,dble(10.0**-5.0))
         !   call setMixedConditions(ss_ppe)
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteMomentum(mom,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         character(len=*),intent(in) :: dir
         call delete(mom%U)
         call delete(mom%Ustar)
         call delete(mom%F)
         call delete(mom%TempVF)
         call delete(mom%p)
         call delete(mom%Temp)

         call delete(mom%u_bcs)
         call delete(mom%v_bcs)
         call delete(mom%w_bcs)
         call delete(mom%p_bcs)
         call delete(mom%divU)
         call closeTransientUnits(dir)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine solveMomentumEquation(mom,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(dpn) :: Re,dt

         dt = getDtime(rd)
         Re = getRe(rd)

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           ! call myFaceAdvectDonor(mom%TempVF,U,U,gd,1)
           ! call myFaceAdvectDonor(mom%TempVF%x,U,U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         end select

         ! mom%Ustar = (-dt)*mom%TempVF
         call multiply(mom%TempVF,(-dt))
         call assign(mom%Ustar,mom%TempVF)

         ! Laplacian Terms -----------------------------------------
         ! call myFaceLap(mom%TempVF,U,gd)

         call myFaceLap(mom%TempVF%x,mom%U%x,gd,1)
         call myFaceLap(mom%TempVF%y,mom%U%y,gd,2)
         call myFaceLap(mom%TempVF%z,mom%U%z,gd,3)

         ! mom%Ustar = mom%Ustar + (dt/Re)*mom%TempVF
         call multiply(mom%TempVF,(dt/Re))
         call add(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + dt*mom%F
         call multiply(mom%F,dt)
         call add(mom%Ustar,mom%F)

         ! Velocity at Previous Time Step Terms --------------------
         ! mom%Ustar = mom%Ustar + mom%U
         call add(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.1) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,gd)
           ! call myFaceDiv(mom%Temp,mom%Ustar,gd)
           ! mom%Temp = (one/dt)*mom%Temp
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,gd,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,gd)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         ! call applyVectorBCs(mom%u_bcs,U,gd)
         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)

         mom%nstep = mom%nstep + 1
       end subroutine

       subroutine printExportMomentumBCs(mom,dir)
         implicit none
         type(momentum),intent(in) :: mom
         character(len=*),intent(in) :: dir
         call printAllBoundaries(mom%u_bcs,'u')
         call printAllBoundaries(mom%v_bcs,'v')
         call printAllBoundaries(mom%w_bcs,'w')
         call printAllBoundaries(mom%p_bcs,'w')
         call writeAllBoundaries(mom%u_bcs,dir//'parameters/','u')
         call writeAllBoundaries(mom%v_bcs,dir//'parameters/','v')
         call writeAllBoundaries(mom%w_bcs,dir//'parameters/','w')
         call writeAllBoundaries(mom%p_bcs,dir//'parameters/','p')
       end subroutine

       subroutine momentumExportTransient(mom,gd,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir

         select case (transientExportXYZ)
         case (1); call exportTransientU(mom,mom%U%x,gd,ss_MHD,dir,1)
         case (2); call exportTransientU(mom,mom%U%y,gd,ss_MHD,dir,2)
         case (3); call exportTransientU(mom,mom%U%z,gd,ss_MHD,dir,3)
         end select

         select case (transientExportXYZ)
         case (1); call exportTransientU(mom,mom%U%x,gd,ss_MHD,dir,1)
         case (2); call exportTransientU(mom,mom%U%y,gd,ss_MHD,dir,2)
         case (3); call exportTransientU(mom,mom%U%z,gd,ss_MHD,dir,3)
         end select

         select case (symmetryPlane)
         case (1); call exportTransientSymmetryU(mom%U%x,gd,ss_MHD,dir,'u')
         case (2); call exportTransientSymmetryU(mom%U%y,gd,ss_MHD,dir,'v')
         case (3); call exportTransientSymmetryU(mom%U%z,gd,ss_MHD,dir,'w')
         end select
       end subroutine

       subroutine exportTransientU(mom,u,gd,ss_MHD,dir,component)
         implicit none
         type(momentum),intent(inout) :: mom
         real(dpn),dimension(:,:,:),intent(in) :: u
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: component
         ! Locals
         integer :: x,y,z,n_mhd
         integer,dimension(3) :: Ni,p,k,s
         real(dpn) :: xc,yc,zc,uc
         character(len=1) :: c
         logical :: TF

         n_mhd = getIteration(ss_MHD)
         TF = (n_mhd.eq.0).and.(solveMomentum)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! U-FIELD
           if (solveMomentum) then
             call getNi(gd,Ni)

             select case (component)
             case (1); c = 'u'; x=1;y=0;z=0
             case (2); c = 'v'; x=0;y=1;z=0
             case (3); c = 'w'; x=0;y=0;z=1
             end select
             s = shape(u)
             k = mod(Ni,2) ! 0 if Ni(component) is even, 1 if odd
             p = (s+1-k)/2
             uc = (u(p(1),p(2),p(3))+dble(k(component))*u(p(1)+x,p(2)+y,p(3)+z))/(1.0d0+dble(k(component)))

             xc = (gd%xni(p(1))+dble(k(component))*gd%xni(p(1)+1))/(1.0d0+dble(k(component)))
             yc = (gd%yni(p(2))+dble(k(component))*gd%yni(p(2)+1))/(1.0d0+dble(k(component)))
             zc = (gd%zni(p(3))+dble(k(component))*gd%zni(p(3)+1))/(1.0d0+dble(k(component)))
             call writeTransientToFile(xc,yc,zc,n_mhd,uc,dir//'Ufield/','transient_'//c,TF)
           endif
         endif

         if (TF.or.(getExportErrors(ss_MHD))) then
           ! EXPORT ERRORS
           if (solveMomentum) then
             call computeError(mom%err_DivU,real(0.0,dpn),mom%divU%phi)
             call writeTransientToFile(n_mhd,getL2(mom%err_DivU),&
                                       dir//'Ufield/','transient_divU',TF)

             ! For some reason, this first L2 error of the PPE is
             ! 15 E-324 or something. Maybe this is a compiler problem
             ! since compaq doesn't do this... Try to fix this...
             ! write(*,*) 'L2PPE',getL2(errPPE)
             if (n_mhd.lt.3) then
             call writeTransientToFile(n_mhd,zero,dir//'Ufield/','transient_ppe',TF)
             else
             call writeTransientToFile(n_mhd,getL2(mom%err_PPE),dir//'Ufield/','transient_ppe',TF)
             endif
           endif
         endif
       end subroutine

       subroutine exportTransientSymmetryU(u,gd,ss_MHD,dir,component)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         character(len=1),intent(in) :: component
         integer :: n_mhd
         logical :: TF
         integer,dimension(3) :: s,Ni
         real(dpn) :: symmetry
         call getNi(gd,Ni)
         n_mhd = getIteration(ss_MHD)

         TF = (n_mhd.eq.0).and.(solveMomentum)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! U-FIELD
           if (solveMomentum) then
             s = shape(u)
             if (mod(Ni(symmetryPlane),1).eq.1) then ! N is odd
               select case (symmetryPlane)
               case (1); symmetry = sum(abs((u(Nin_probe(1),:,:)+u(Nin_probe(1)+1,:,:))/two))/dble(s(2)*s(3))
               case (2); symmetry = sum(abs((u(:,Nin_probe(2),:)+u(:,Nin_probe(2)+1,:))/two))/dble(s(1)*s(3))
               case (3); symmetry = sum(abs((u(:,:,Nin_probe(3))+u(:,:,Nin_probe(3)+1))/two))/dble(s(1)*s(2))
               end select
             else                        ! N is even
               select case (symmetryPlane)
               case (1); symmetry = sum(abs(u(Nic_probe(1),:,:)))/dble(s(2)*s(3))
               case (2); symmetry = sum(abs(u(:,Nic_probe(2),:)))/dble(s(1)*s(3))
               case (3); symmetry = sum(abs(u(:,:,Nic_probe(3))))/dble(s(1)*s(2))
               end select
             endif
             call writeTransientToFile(n_mhd,symmetry,dir//'Ufield/','transient_symmetry_'//component,TF)
           endif
         endif
       end subroutine

       subroutine closeTransientUnits(dir)
         implicit none
         character(len=*),intent(in) :: dir
         integer :: utemp
         if (solveMomentum) then
           select case (transientExportXYZ)
           case (1); utemp = getUnit(dir//'Ufield/','transient_u')
           case (2); utemp = getUnit(dir//'Ufield/','transient_v')
           case (3); utemp = getUnit(dir//'Ufield/','transient_w')
           end select
           close(utemp)
           utemp = getUnit(dir//'Ufield/','transient_divU')
           close(utemp)
           utemp = getUnit(dir//'Ufield/','transient_ppe')
           close(utemp)
         endif
       end subroutine

       subroutine momentumExportRaw(mom,gd,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! ************************* EXPORT IN RAW FORMAT *************************
         call writeToFile(gd%xni,gd%yci,gd%zci,mom%U%x,dir//'Ufield/','ufi')
         call writeToFile(gd%xci,gd%yni,gd%zci,mom%U%y,dir//'Ufield/','vfi')
         call writeToFile(gd%xci,gd%yci,gd%zni,mom%U%z,dir//'Ufield/','wfi')
         call writeToFile(gd%xci,gd%yci,gd%zci,mom%p%phi,dir//'Ufield/','pci')
         ! ----------------------- DIVERGENCE QUANTITIES --------------------------
         call writeToFile(gd%xci,gd%yci,gd%zci,mom%divU%phi,dir//'Ufield/','divUci')
         write(*,*) 'Exported Raw Solutions for U'
       end subroutine

       subroutine momentumExport(mom,gd,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         ! Interior
         ! real(dpn),dimension(:,:,:),allocatable :: tempx,tempy,tempz
         real(dpn),dimension(:,:,:),allocatable :: tempccx,tempccy,tempccz
         real(dpn),dimension(:,:,:),allocatable :: tempnx,tempny,tempnz,tempn

         ! ************************ EXPORT IN OTHER FORMATS ***********************

         ! --------------------------- UFIELD AT NODES ----------------------------
         call myAllocate(Nx,Ny,Nz,gd,dom_n_in); allocate(tempnx(Nx,Ny,Nz))
                                                allocate(tempny(Nx,Ny,Nz))
                                                allocate(tempnz(Nx,Ny,Nz))
                                                allocate(tempn (Nx,Ny,Nz))
         call myFace2Node(tempnx,mom%U%x,gd,1)
         call myFace2Node(tempny,mom%U%y,gd,2)
         call myFace2Node(tempnz,mom%U%z,gd,3)

         call myNodeDiv(tempn,tempnx,tempny,tempnz,gd)
         call writeToFile(gd%xni,gd%yni,gd%zni,tempnx,tempny,tempnz,dir//'Ufield/','uni','vni','wni')
         call writeToFile(gd%xni,gd%yni,gd%zni,tempn,dir//'Ufield/','divUni')
         deallocate(tempnx,tempny,tempnz,tempn)

         ! ****************** EXPORT IN CELL CENTERS ************************
         if (outputAlternativeFormats) then
           ! Velocities at cell centers:
           call myAllocate(Nx,Ny,Nz,gd,dom_cc_in); allocate(tempccx(Nx,Ny,Nz))
                                                   allocate(tempccy(Nx,Ny,Nz))
                                                   allocate(tempccz(Nx,Ny,Nz))
           call myFace2CellCenter(tempccx,mom%U%x,gd,1)
           call myFace2CellCenter(tempccy,mom%U%y,gd,2)
           call myFace2CellCenter(tempccz,mom%U%z,gd,3)
           call writeToFile(gd%xci,gd%yci,gd%zci,tempccx,tempccy,tempccz,dir//'Ufield/','uci','vci','wci')
           deallocate(tempccx,tempccy,tempccz)
         endif
       end subroutine

       subroutine computeDivergenceMomentum(mom,gd)
         implicit none
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         call myFaceDiv(mom%divU%phi,mom%U%x,mom%U%y,mom%U%z,gd)
       end subroutine

       end module