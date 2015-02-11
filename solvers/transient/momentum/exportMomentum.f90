       module exportMomentum_mod
       ! The format of the output results are as follows:
       !
       !  phi  [f,c,n]     [f,c,n]           [i,t]                 [b/a]
       !          ^           ^                ^
       !      original   interpolated    interior/total         calc/interp
       !      location     location         domain         before(c->i)/after(i->c)
       ! 
       ! The interior/total domain only applies to variables from 
       ! the momentum equation, since they must be embedded for
       ! the induction equation.
       ! 
       ! If there are no interpolations, then the original location
       ! is the only suffix.
       ! 
       ! The order of calculation/interpolation should only apply to 
       ! divergence quantities.
       ! 
       use simParams_mod
       use constants_mod
       use myIO_mod
       use myDebug_mod
       use myAllocate_mod
       use griddata_mod
       use myError_mod
       use rundata_mod
       use BCs_mod
       use solverSettings_mod
       use vectorOps_mod
       implicit none

       private

       public :: printExportSimParams
       public :: exportTransient
       public :: closeTransientUnits
       public :: exportRawResults
       public :: exportResults

       contains

       subroutine printExportSimParams(u_bcs,v_bcs,w_bcs,p_bcs,&
         Bx_bcs,By_bcs,Bz_bcs,phi_bcs,gd,rd,dir)
         implicit none
         type(BCs),intent(in) :: u_bcs,v_bcs,w_bcs,p_bcs
         type(BCs),intent(in) :: Bx_bcs,By_bcs,Bz_bcs,phi_bcs
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         character(len=*),intent(in) :: dir

         ! *************** WRITE PARAMETERS TO FILE *********************
         call printGriddata(gd)
         call printRunData(rd)
         call printAllBoundaries(u_bcs,'u')
         call printAllBoundaries(v_bcs,'v')
         call printAllBoundaries(w_bcs,'w')
         if (solveInduction) call printAllBoundaries(Bx_bcs,'Bx')
         if (solveInduction) call printAllBoundaries(By_bcs,'By')
         if (solveInduction) call printAllBoundaries(Bz_bcs,'Bz')
         if (cleanB) call printAllBoundaries(phi_bcs,'phi')
         call writeGriddata(gd,dir//'parameters/')
         call writeRunData(rd,dir//'parameters/')
         call writeAllBoundaries(u_bcs,dir//'parameters/','u')
         call writeAllBoundaries(v_bcs,dir//'parameters/','v')
         call writeAllBoundaries(w_bcs,dir//'parameters/','w')
         call writeAllBoundaries(p_bcs,dir//'parameters/','p')
         if (solveInduction) call writeAllBoundaries(Bx_bcs,dir//'parameters/','Bx')
         if (solveInduction) call writeAllBoundaries(By_bcs,dir//'parameters/','By')
         if (solveInduction) call writeAllBoundaries(Bz_bcs,dir//'parameters/','Bz')
         if (cleanB) call writeAllBoundaries(phi_bcs,dir//'parameters/','phi')
       end subroutine

       subroutine exportTransient(u,v,w,Bx,By,Bz,jx,jy,jz,&
         errPPE,divU,divB,divJ,gd,ss_MHD,dir,n_ind,n_mhd)
         ! exportTransient exports the probed transient data
         ! and residuals divergence quantities with a frequency
         ! defined by nskip, as specified in the simParams file.
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),intent(in) :: Bx,By,Bz
         real(dpn),dimension(:,:,:),intent(in) :: jx,jy,jz
         real(dpn),dimension(:,:,:),intent(in) :: divU,divB,divJ
         type(myError),intent(in) :: errPPE
         type(griddata),intent(in) :: gd
         ! type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd
         ! if (getTransientExportTime(ss_MHD)) then
         !   call writeTransientToFile(n_mhd,getTime(rd),dir//'parameters/','time',.true.)
         ! endif
         select case (transientExportXYZ)
         case (1)
           call exportTransientU(u,errPPE,divU,gd,ss_MHD,dir,n_ind,n_mhd,1)
           call exportTransientB(Bx,divB,gd,ss_MHD,dir,n_ind,n_mhd,'x')
           call exportTransientJ(jx,divJ,gd,ss_MHD,dir,n_ind,n_mhd,'x')
         case (2)
           call exportTransientU(v,errPPE,divU,gd,ss_MHD,dir,n_ind,n_mhd,2)
           call exportTransientB(By,divB,gd,ss_MHD,dir,n_ind,n_mhd,'y')
           call exportTransientJ(jy,divJ,gd,ss_MHD,dir,n_ind,n_mhd,'y')
         case (3)
           call exportTransientU(w,errPPE,divU,gd,ss_MHD,dir,n_ind,n_mhd,3)
           call exportTransientB(Bz,divB,gd,ss_MHD,dir,n_ind,n_mhd,'z')
           call exportTransientJ(jz,divJ,gd,ss_MHD,dir,n_ind,n_mhd,'z')
         end select

         ! select case (symmetryPlane)
         ! case (1)
         !   call exportTransientSymmetryU(u,ss_MHD,dir,n_ind,n_mhd,'u')
         !   call exportTransientSymmetryB(Bx,ss_MHD,dir,n_ind,n_mhd,'Bx')
         ! case (2)
         !   call exportTransientSymmetryU(v,ss_MHD,dir,n_ind,n_mhd,'v')
         !   call exportTransientSymmetryB(By,ss_MHD,dir,n_ind,n_mhd,'By')
         ! case (3)
         !   call exportTransientSymmetryU(w,ss_MHD,dir,n_ind,n_mhd,'w')
         !   call exportTransientSymmetryB(Bz,ss_MHD,dir,n_ind,n_mhd,'Bz')
         ! end select

         ! call exportTransientSymmetryU(u,ss_MHD,dir,n_ind,n_mhd,'u')
         ! call exportTransientSymmetryU(v,ss_MHD,dir,n_ind,n_mhd,'v')
         call exportTransientSymmetryU(w,gd,ss_MHD,dir,n_ind,n_mhd,'w')
         ! call exportTransientSymmetryB(Bx,ss_MHD,dir,n_ind,n_mhd,'Bx')
         ! call exportTransientSymmetryB(By,ss_MHD,dir,n_ind,n_mhd,'By')
         ! call exportTransientSymmetryB(Bz,ss_MHD,dir,n_ind,n_mhd,'Bz')
       end subroutine

       subroutine exportTransientU(u,errPPE,divU,gd,ss_MHD,dir,n_ind,n_mhd,component)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,divU
         type(myError),intent(in) :: errPPE
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd,component
         ! Locals
         integer :: Nx,Ny,Nz,x,y,z
         integer,dimension(3) :: Ni,p,k,s
         real(dpn),dimension(:),allocatable :: xci,yci,zci
         real(dpn),dimension(:),allocatable :: xni,yni,zni
         type(myError) :: errDivU
         real(dpn) :: xc,yc,zc,uc
         character(len=1) :: c
         logical :: TF,TFU

         TF = (n_mhd.eq.0).or.((n_ind.eq.0))
         TFU = (n_mhd.eq.0).and.(solveMomentum)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! U-FIELD
           if (solveMomentum) then
             call myAllocate(Nx,Ny,Nz,gd,dom_cc_in)
             allocate(xci(Nx),yci(Ny),zci(Nz))
             call myAllocate(Nx,Ny,Nz,gd,dom_n_in)
             allocate(xni(Nx),yni(Ny),zni(Nz))
             call getXYZcc(gd,xci,yci,zci)
             call getXYZn(gd,xni,yni,zni)
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

             xc = (xni(p(1))+dble(k(component))*xni(p(1)+1))/(1.0d0+dble(k(component)))
             yc = (yni(p(2))+dble(k(component))*yni(p(2)+1))/(1.0d0+dble(k(component)))
             zc = (zni(p(3))+dble(k(component))*zni(p(3)+1))/(1.0d0+dble(k(component)))
             call writeTransientToFile(xc,yc,zc,n_mhd,uc,dir//'Ufield/','transient_'//c,TFU)
             deallocate(xci,yci,zci)
             deallocate(xni,yni,zni)
           endif
         endif

         if (TF.or.(getExportErrors(ss_MHD))) then
           ! EXPORT ERRORS
           if (solveMomentum) then
             call computeError(errDivU,dble(0.0),divU)
             call writeTransientToFile(n_mhd,getL2(errDivU),&
                                       dir//'Ufield/','transient_divU',TFU)

             ! For some reason, this first L2 error of the PPE is
             ! 15 E-324 or something. Maybe this is a compiler problem
             ! since compaq doesn't do this... Try to fix this...
             ! write(*,*) 'L2PPE',getL2(errPPE)
             if ((n_mhd.eq.0).or.(n_mhd.eq.1).or.(n_mhd.eq.2)) then
             call writeTransientToFile(n_mhd,zero,&
                                       dir//'Ufield/','transient_ppe',TFU)
             else
             call writeTransientToFile(n_mhd,getL2(errPPE),&
                                       dir//'Ufield/','transient_ppe',TFU)
             endif
           endif
         endif
       end subroutine

       subroutine exportTransientB(B,divB,gd,ss_MHD,dir,n_ind,n_mhd,component)
         ! exportTransient exports the probed transient data
         ! and residuals divergence quantities with a frequency
         ! defined by nskip, as specified in the simParams file.
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: B,divB
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd
         character(len=1),intent(in) :: component

         ! Locals
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt
         type(myError) :: errDivB
         logical :: TF,TFB

         TF = (n_mhd.eq.0).or.((n_ind.eq.0))
         TFB = (n_ind.eq.0).and.(solveInduction)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! B-FIELD
           if (solveInduction) then
             call myAllocate(Nx,Ny,Nz,gd,dom_cc_tot)
             allocate(xct(Nx),yct(Ny),zct(Nz))
             call myAllocate(Nx,Ny,Nz,gd,dom_n_tot)
             allocate(xnt(Nx),ynt(Ny),znt(Nz))
             call getXYZcc(gd,xct,yct,zct)
             call getXYZn(gd,xnt,ynt,znt)
             ! Nx,Ny,Nz must be odd for this!!!
             call writeTransientToFile(xnt(N_probe(1)),ynt(N_probe(2)),znt(N_probe(3)),&
                                       n_mhd,B(N_probe(1),N_probe(2),N_probe(3)),&
                                       dir//'Bfield/','transient_B'//component,TFB)
             deallocate(xct,yct,zct)
             deallocate(xnt,ynt,znt)
           endif
         endif

         if (TF.or.(getExportErrors(ss_MHD))) then
           ! EXPORT ERRORS
           if (solveInduction) then
             call computeError(errDivB,dble(0.0),divB)
             call writeTransientToFile(n_ind,getLinf(errDivB),&
                                       dir//'Bfield/','transient_divB',TFB)
           endif
         endif
       end subroutine

       subroutine exportTransientJ(j,divJ,gd,ss_MHD,dir,n_ind,n_mhd,component)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: j,divJ
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd
         character(len=1),intent(in) :: component
         ! Locals
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt
         type(myError) :: errDivJ
         logical :: TF,TFJ

         TF = (n_mhd.eq.0).or.((n_ind.eq.0))
         TFJ = (n_mhd.eq.0).and.(solveInduction)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! J-FIELD
           if (solveInduction) then
             call myAllocate(Nx,Ny,Nz,gd,dom_cc_tot)
             allocate(xct(Nx),yct(Ny),zct(Nz))
             call myAllocate(Nx,Ny,Nz,gd,dom_n_tot)
             allocate(xnt(Nx),ynt(Ny),znt(Nz))
             call getXYZcc(gd,xct,yct,zct)
             call getXYZn(gd,xnt,ynt,znt)

             call writeTransientToFile(xnt(N_probe(1)),ynt(N_probe(2)),znt(N_probe(3)),&
                                       n_mhd,j(N_probe(1),N_probe(2),N_probe(3)),&
                                       dir//'Jfield/','transient_j'//component,TFJ)
             deallocate(xct,yct,zct)
             deallocate(xnt,ynt,znt)
           endif
         endif

         if (TF.or.(getExportErrors(ss_MHD))) then
           ! EXPORT ERRORS
           if (solveInduction) then
             call computeError(errDivJ,dble(0.0),divJ)
             call writeTransientToFile(n_ind,getLinf(errDivJ),&
                                       dir//'Jfield/','transient_divJ',TFJ)
           endif
         endif
       end subroutine

       subroutine exportTransientSymmetryU(u,gd,ss_MHD,dir,n_ind,n_mhd,component)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd
         character(len=1),intent(in) :: component
         ! Locals
         logical :: TF,TFU
         integer,dimension(3) :: s,Ni
         real(dpn) :: symmetry
         call getNi(gd,Ni)

         TF = (n_mhd.eq.0).or.((n_ind.eq.0))
         TFU = (n_mhd.eq.0).and.(solveMomentum)

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
             call writeTransientToFile(n_mhd,symmetry,dir//'Ufield/','transient_symmetry_'//component,TFU)
           endif
         endif
       end subroutine

       subroutine exportTransientSymmetryB(B,ss_MHD,dir,n_ind,n_mhd,component)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: B
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         integer,intent(in) :: n_ind,n_mhd
         character(len=1),intent(in) :: component
         ! Locals
         logical :: TF,TFB
         real(dpn) :: symmetry

         TF = (n_mhd.eq.0).or.((n_ind.eq.0))
         TFB = (n_mhd.eq.0).and.(solveInduction)

         if (TF.or.(getExportTransient(ss_MHD))) then
           ! B-FIELD
           if (solveInduction) then
             select case (symmetryPlane)
             case (1); symmetry = sum(abs(B(N_probe(1),:,:)))
               call writeTransientToFile(n_mhd,symmetry,dir//'Ufield/','transient_symmetry_'//component,TFB)
             case (2); symmetry = sum(abs(B(:,N_probe(2),:)))
               call writeTransientToFile(n_mhd,symmetry,dir//'Ufield/','transient_symmetry_'//component,TFB)
             case (3); symmetry = sum(abs(B(:,:,N_probe(3))))
               call writeTransientToFile(n_mhd,symmetry,dir//'Ufield/','transient_symmetry_'//component,TFB)
             end select

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

         if (solveInduction) then
           select case (transientExportXYZ)
           case (1); utemp = getUnit(dir//'Bfield/','transient_Bx')
           case (2); utemp = getUnit(dir//'Bfield/','transient_By')
           case (3); utemp = getUnit(dir//'Bfield/','transient_Bz')
           end select
           close(utemp)
           utemp = getUnit(dir//'Bfield/','transient_divB')
           close(utemp)

           select case (transientExportXYZ)
           case (1); utemp = getUnit(dir//'Jfield/','transient_jx')
           case (2); utemp = getUnit(dir//'Jfield/','transient_jy')
           case (3); utemp = getUnit(dir//'Jfield/','transient_jz')
           end select
           close(utemp)
           utemp = getUnit(dir//'Jfield/','transient_divJ')
           close(utemp)
         endif
       end subroutine

       subroutine exportRawResults(u,v,w,p,divU,&
         Bx,By,Bz,divB,jx,jy,jz,divJ,sigma,mu,gd,dir)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,p,divU
         real(dpn),dimension(:,:,:),intent(in) :: Bx,By,Bz,divB
         real(dpn),dimension(:,:,:),intent(in) :: jx,jy,jz,divJ
         real(dpn),dimension(:,:,:),intent(in) :: sigma,mu
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xci,yci,zci
         real(dpn),dimension(:),allocatable :: xni,yni,zni
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt

         ! ***************************** PREP GRIDS *******************************
         call myAllocate(Nx,Ny,Nz,gd,interiorCC)
         allocate(xci(Nx),yci(Ny),zci(Nz))
         call getXYZcc(gd,xci,yci,zci)

         call myAllocate(Nx,Ny,Nz,gd,interiorN)
         allocate(xni(Nx),yni(Ny),zni(Nz))
         call getXYZn(gd,xni,yni,zni)

         call myAllocate(Nx,Ny,Nz,gd,totalCC)
         allocate(xct(Nx),yct(Ny),zct(Nz))
         call getXYZcc(gd,xct,yct,zct)

         call myAllocate(Nx,Ny,Nz,gd,totalN)
         allocate(xnt(Nx),ynt(Ny),znt(Nz))
         call getXYZn(gd,xnt,ynt,znt)

         ! ************************* EXPORT IN RAW FORMAT *************************

         ! ------------------------------- U FIELD --------------------------------
         call writeToFile(xni,yci,zci,u,dir//'Ufield/','ufi')
         call writeToFile(xci,yni,zci,v,dir//'Ufield/','vfi')
         call writeToFile(xci,yci,zni,w,dir//'Ufield/','wfi')
         call writeToFile(xci,yci,zci,p,dir//'Ufield/','pci')

         ! ------------------------------- B/J FIELD ------------------------------
         if (solveInduction) then
           select case (BLoc)
           case (dom_cc_tot)
             call writeToFile(xct,yct,zct,Bx,By,Bz,dir//'Bfield/','Bxct','Byct','Bzct')
             call writeToFile(xct,yct,zct,jx,jy,jz,dir//'Jfield/','jxct','jyct','jzct')
           case (dom_n_tot)
             call writeToFile(xnt,ynt,znt,Bx,By,Bz,dir//'Bfield/','Bxnt','Bynt','Bznt')
             call writeToFile(xnt,ynt,znt,jx,jy,jz,dir//'Jfield/','jxnt','jynt','jznt')
           end select
         endif

         ! ---------------------------- SIGMA/MU FIELD ----------------------------
         select case (BLoc)
         case (dom_cc_tot)
           call writeToFile(xct,yct,zct,sigma,dir//'material/','sigmac')
           call writeToFile(xct,yct,zct,mu,dir//'material/','muc')
         case (dom_n_tot)
           call writeToFile(xnt,ynt,znt,sigma,dir//'material/','sigman')
           call writeToFile(xnt,ynt,znt,mu,dir//'material/','mun')
         end select

         ! ----------------------- DIVERGENCE QUANTITIES --------------------------
         call writeToFile(xci,yci,zci,divU,dir//'Ufield/','divUci')
         if (solveInduction) then
           select case (BLoc)
           case (dom_cc_tot)
             call writeToFile(xct,yct,zct,divB,dir//'Bfield/','divBct')
             call writeToFile(xct,yct,zct,divJ,dir//'Jfield/','divJct')
           case (dom_n_tot)
             call writeToFile(xnt,ynt,znt,divB,dir//'Bfield/','divBnt')
             call writeToFile(xnt,ynt,znt,divJ,dir//'Jfield/','divJnt')
           end select
         endif

         ! ****************** DEALLOCATE LOCALS *************************
         deallocate(xci,yci,zci)
         deallocate(xni,yni,zni)
         deallocate(xct,yct,zct)
         deallocate(xnt,ynt,znt)
         write(*,*) 'Exported Raw Solutions'
       end subroutine

       subroutine exportResults(u,v,w,ut,vt,wt,&
         Bx,By,Bz,jx,jy,jz,sigma,mu,gd,dir)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),intent(in) :: ut,vt,wt
         real(dpn),dimension(:,:,:),intent(in) :: Bx,By,Bz
         real(dpn),dimension(:,:,:),intent(in) :: jx,jy,jz
         real(dpn),dimension(:,:,:),intent(in) :: sigma,mu
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         real(dpn),dimension(:),allocatable :: xci,yci,zci
         real(dpn),dimension(:),allocatable :: xni,yni,zni
         real(dpn),dimension(:),allocatable :: xct,yct,zct
         real(dpn),dimension(:),allocatable :: xnt,ynt,znt
         ! Interior
         ! real(dpn),dimension(:,:,:),allocatable :: tempx,tempy,tempz
         real(dpn),dimension(:,:,:),allocatable :: tempccx,tempccy,tempccz
         real(dpn),dimension(:,:,:),allocatable :: tempnx,tempny,tempnz,tempn,tempcc

         ! ***************************** PREP GRIDS *******************************
         call myAllocate(Nx,Ny,Nz,gd,interiorCC)
         allocate(xci(Nx),yci(Ny),zci(Nz))
         call getXYZcc(gd,xci,yci,zci)

         call myAllocate(Nx,Ny,Nz,gd,interiorN)
         allocate(xni(Nx),yni(Ny),zni(Nz))
         call getXYZn(gd,xni,yni,zni)

         call myAllocate(Nx,Ny,Nz,gd,totalCC)
         allocate(xct(Nx),yct(Ny),zct(Nz))
         call getXYZcc(gd,xct,yct,zct)

         call myAllocate(Nx,Ny,Nz,gd,totalN)
         allocate(xnt(Nx),ynt(Ny),znt(Nz))
         call getXYZn(gd,xnt,ynt,znt)

         ! ************************ EXPORT IN OTHER FORMATS ***********************

         ! --------------------------- UFIELD AT NODES ----------------------------
         call myAllocate(Nx,Ny,Nz,gd,dom_n_in); allocate(tempnx(Nx,Ny,Nz))
                                                allocate(tempny(Nx,Ny,Nz))
                                                allocate(tempnz(Nx,Ny,Nz))
                                                allocate(tempn (Nx,Ny,Nz))
         call myFace2Node(tempnx,u,gd,1)
         call myFace2Node(tempny,v,gd,2)
         call myFace2Node(tempnz,w,gd,3)

         call myNodeDiv(tempn,tempnx,tempny,tempnz,gd)
         call writeToFile(xni,yni,zni,tempnx,tempny,tempnz,dir//'Ufield/','uni','vni','wni')
         call writeToFile(xni,yni,zni,tempn,dir//'Ufield/','divUni')
         deallocate(tempnx,tempny,tempnz,tempn)

         ! -------------------------- B/J FIELD AT NODES --------------------------

         ! Magnetic field/currents:
         if (solveInduction) then
           select case (BLoc)
           case (dom_cc_tot)
             call myAllocate(Nx,Ny,Nz,gd,dom_n_tot); allocate(tempnx(Nx,Ny,Nz))
                                                     allocate(tempny(Nx,Ny,Nz))
                                                     allocate(tempnz(Nx,Ny,Nz))
                                                     allocate(tempn (Nx,Ny,Nz))
             call myCellCenter2Node(tempnx,Bx,gd)
             call myCellCenter2Node(tempny,By,gd)
             call myCellCenter2Node(tempnz,Bz,gd)

             call writeToFile(xnt,ynt,znt,tempnx,tempny,tempnz,dir//'Bfield/','Bxnt','Bynt','Bznt')
             call myNodeDiv(tempn,tempnx,tempny,tempnz,gd)
             call writeToFile(xnt,ynt,znt,tempn,dir//'Bfield/','divBnt')
             call myCellCenter2Node(tempnx,jx,gd)
             call myCellCenter2Node(tempny,jy,gd)
             call myCellCenter2Node(tempnz,jz,gd)
             call writeToFile(xnt,ynt,znt,tempnx,tempny,tempnz,dir//'Jfield/','jxnt','jynt','jznt')
             deallocate(tempnx,tempny,tempnz,tempn)
           end select
         endif

         ! ----------------------- SIGMA/MU FIELD AT NODES ------------------------
         select case (BLoc)
         case (dom_cc_tot)
           call myAllocate(Nx,Ny,Nz,gd,dom_n_tot); allocate(tempn(Nx,Ny,Nz))
           call myCellCenter2Node(tempn,sigma,gd)
           call writeToFile(xnt,ynt,znt,tempn,dir//'material/','sigman')
           call myCellCenter2Node(tempn,mu,gd)
           call writeToFile(xnt,ynt,znt,tempn,dir//'material/','mun')
           deallocate(tempn)
         end select


         ! -------------------------- TOTAL DOMAIN VELOCITY -----------------------
         ! Total velocity
         select case (BLoc)
         case (dom_cc_tot)
           call writeToFile(xct,yct,zct,ut,vt,wt,dir//'Ufield/','uct','vct','wct')
           call myAllocate(Nx,Ny,Nz,gd,dom_cc_tot); allocate(tempcc(Nx,Ny,Nz))
           call myCC2CCDiv(tempcc,ut,vt,wt,gd)
           call writeToFile(xct,yct,zct,tempcc,dir//'Ufield/','divUct')
           deallocate(tempcc)
         case (dom_n_tot)
           call myAllocate(Nx,Ny,Nz,gd,dom_n_tot); allocate(tempn(Nx,Ny,Nz))
           call myNodeDiv(tempn,ut,vt,wt,gd)
           call writeToFile(xnt,ynt,znt,ut,vt,wt,dir//'Ufield/','unt','vnt','wnt')
           call writeToFile(xnt,ynt,znt,tempn,dir//'Ufield/','divUnt')
           deallocate(tempn)
         end select


         ! ****************** EXPORT IN CELL CENTERS ************************
         if (outputAlternativeFormats) then
           ! Velocities at cell centers:
           call myAllocate(Nx,Ny,Nz,gd,dom_cc_in); allocate(tempccx(Nx,Ny,Nz))
                                                   allocate(tempccy(Nx,Ny,Nz))
                                                   allocate(tempccz(Nx,Ny,Nz))
           call myFace2CellCenter(tempccx,u,gd,1)
           call myFace2CellCenter(tempccy,v,gd,2)
           call myFace2CellCenter(tempccz,w,gd,3)
           call writeToFile(xci,yci,zci,tempccx,tempccy,tempccz,dir//'Ufield/','uci','vci','wci')
           ! ! Velocities back to face after linear interpolation:
           ! call myAllocate(Nx,Ny,Nz,gd,ULoc,1); allocate(tempx(Nx,Ny,Nz))
           ! call myAllocate(Nx,Ny,Nz,gd,ULoc,2); allocate(tempy(Nx,Ny,Nz))
           ! call myAllocate(Nx,Ny,Nz,gd,ULoc,3); allocate(tempz(Nx,Ny,Nz))
           ! call myCellCenter2Face(tempx,tempccx,gd,1)
           ! call myCellCenter2Face(tempy,tempccy,gd,2)
           ! call myCellCenter2Face(tempz,tempccz,gd,3)
           ! call writeToFile(xni,yci,zci,tempx,dir//'Ufield/','uf_postProc')
           ! call writeToFile(xci,yni,zci,tempy,dir//'Ufield/','vf_postProc')
           ! call writeToFile(xci,yci,zni,tempz,dir//'Ufield/','wf_postProc')
           ! deallocate(tempx,tempy,tempz)
           deallocate(tempccx,tempccy,tempccz)
         endif

         ! ****************** DEALLOCATE LOCALS *************************

         deallocate(xci,yci,zci)
         deallocate(xni,yni,zni)
         deallocate(xct,yct,zct)
         deallocate(xnt,ynt,znt)
       end subroutine

       end module