       module calcOmegaPsi_mod
       use simParams_mod
       use constants_mod
       use myIO_mod
       use griddata_mod
       use vectorOps_mod
       use solverSettings_mod
       use BCs_mod
       use myError_mod
       use myPoisson_mod
       implicit none
       contains

       subroutine calcOmegaPsi(u,v,w,gd,dir)
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         integer,dimension(3) :: N
         real(dpn),dimension(:),allocatable :: xc,yc,zc
         real(dpn),dimension(:),allocatable :: xn,yn,zn
         real(dpn),dimension(:,:,:),allocatable :: omega,psi
         type(BCs) :: psi_bcs
         type(myError) :: err
         type(solverSettings) :: ss_poisson
         ! **************************************************************
         call getN(gd,N)
         Nx = N(1); Ny = N(2); Nz = N(3)
         allocate(xc(Nx+2),yc(Ny+2),zc(Nz+2))
         allocate(xn(Nx+1),yn(Ny+1),zn(Nz+1))

         call getXYZcc(gd,xc,yc,zc)
         call getXYZn(gd,xn,yn,zn)

         ! ********* CALCULATE VORTICITY AND STREAM FUNCTION *************
         if (calculateOmegaPsi) then
           ! Poisson Equation solver settings
           call setName(ss_poisson,'psi poisson         ')
           call init(ss_poisson)
           call setMaxIterations(ss_poisson,200)

           ! x - component
           allocate(omega(Nx+2,Ny+1,Nz+1))
           call myFaceCurl(omega,u,v,w,gd,1)
           call writeToFile(xc,yn,zn,omega,dir//'Ufield/','omega_x')

           allocate(psi(Nx+2,Ny+1,Nz+1))
           call setAllZero(psi_bcs,Nx+2,Ny+1,Nz+1,2)
           call checkBCs(psi_bcs)
           psi = 0.0 ! (Initial condition)
           omega = -omega
           call myPoisson(psi,omega,psi_bcs,gd,ss_poisson,err,2)
           call deallocateBCs(psi_bcs)
           call writeToFile(xc,yn,zn,psi,dir//'Ufield/','psi_x')
           deallocate(omega,psi)
           
           ! y - component
           allocate(omega(Nx+1,Ny+2,Nz+1))
           call myFaceCurl(omega,u,v,w,gd,2)
           call writeToFile(xn,yc,zn,omega,dir//'Ufield/','omega_y')

           allocate(psi(Nx+1,Ny+2,Nz+1))
           call setAllZero(psi_bcs,Nx+1,Ny+2,Nz+1,2)
           call checkBCs(psi_bcs)
           psi = 0.0 ! (Initial condition)
           omega = -omega

           call myPoisson(psi,omega,psi_bcs,gd,ss_poisson,err,2)
           call deallocateBCs(psi_bcs)
           call writeToFile(xn,yc,zn,psi,dir//'Ufield/','psi_y')
           deallocate(omega,psi)
           
           ! z - component
           allocate(omega(Nx+1,Ny+1,Nz+2))
           call myFaceCurl(omega,u,v,w,gd,3)
           call writeToFile(xn,yn,zc,omega,dir//'Ufield/','omega_z')

           allocate(psi(Nx+1,Ny+1,Nz+2))
           call setAllZero(psi_bcs,Nx+1,Ny+1,Nz+2,2)
           call checkBCs(psi_bcs)
           psi = 0.0 ! (Initial condition)
           omega = -omega
           call myPoisson(psi,omega,psi_bcs,gd,ss_poisson,err,2)
           call deallocateBCs(psi_bcs)
           call writeToFile(xn,yn,zc,psi,dir//'Ufield/','psi_z')
           deallocate(omega,psi)
         endif

         call deallocateBCs(psi_bcs)
         deallocate(xc,yc,zc)
         deallocate(xn,yn,zn)

       end subroutine
       end module