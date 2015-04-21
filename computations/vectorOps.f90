       module delOps_mod
       ! 
       ! Directions are frequently used in this code. 
       ! For clarity, some diagrams here show how the 
       ! directions are defined.
       ! 
       ! faceDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !      faceDir --->  \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       ! 
       ! 
       ! 
       ! edgeDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !                    \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !                        -------> edgeDir
       ! 
       ! 
       use del_mod
       use grid_mod
       use vectorField_mod
       use myError_mod
       use interpOps_mod
       use IO_scalarFields_mod

       implicit none

       ! Compiler Flags (_PARALLELIZE_DELOPS_,
       !                 _DEBUG_DELOPS_,
       !                 _CHECK_SYMMETRY_DELOPS_)

       private

#ifdef _CHECK_SYMMETRY_
       integer,parameter :: symmetryPlane = 2
       public :: symmetryPlane
#endif

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       ! ----------------------------------- OTHER ROUTINES ------------------------------------
       public :: zeroGhostPoints      ! call zeroGhostPoints(f)
       public :: totalEnergy          ! call totalEnergy(e,u,v,w)
       public :: collocatedMagnitude  ! call collocatedMagnitude(mag,u,v,w)
       public :: myCollocatedCross    ! call myCollocatedCross(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir)
       public :: printPhysicalMinMax  ! call printPhysicalMinMax(u,s,name)
       public :: checkSymmetry        ! call checkSymmetry(u,plane,name)

       ! --------------------------------- DERIVATIVE ROUTINES ---------------------------------
       ! General data derivatives
       public :: lap                  ! call lap(lapU,u,v,w,g)
       public :: div                  ! call div(divU,u,v,w,g)

       public :: collocatedCurl       ! call curl(divU,u,v,w,g)

       ! Face based derivatives
       ! public :: myFaceDiv            ! call myFaceDiv(divU,u,v,w,g)
       ! public :: myFaceLap            ! call myFaceLap(lapF,f,g,dir)
       public :: myFaceAdvect         ! call myFaceAdvect(psi,u,v,w,phi,g)
       public :: myFaceAdvectDonor    ! call myFaceAdvectDonor(psi,u,v,w,phi,g)
       public :: myFaceAdvectHybrid   ! call myFaceAdvectHybrid(psi,u,v,w,phi,g)
       public :: myFaceCurl           ! call myFaceCurl(curlU,u,v,w,g,dir)

       ! Cell-center based derivatives
       ! public :: myCC2CCDiv           ! call myCC2CCDiv(divU,u,v,w,g)
       ! public :: CC2CCLap             ! call CC2CCLap(lapU,u,g)
       public :: myCC2FaceGrad        ! call myCC2FaceGrad(gradx,grady,gradz,p,g)
       public :: myCC2EdgeCurl        ! call myCC2EdgeCurl(curl,u,v,w,g,dir)
       public :: myCCVaryDel          ! call myCCVaryDel(ddf,u,v,w,k,g,dir1,dir2)

       public :: myCCCurl             ! call myCCCurl(curl,u,v,w,g,dir) ! May be generalized
       public :: myCC2CCDel           ! call myCC2CCDel(gradp,p,g,dir) ! Seems useless

       ! Node-center based derivatives
       ! public :: myNodeGrad           ! call myNodeGrad(gradx,grady,gradz,f,g)
       ! public :: myNode2NodeDel       ! call myNode2NodeDel(df,f,g,dir)
       ! public :: myNodeDiv            ! call myNodeDiv(divU,u,v,w,g)
       ! public :: myNodeLap            ! call myNodeLap(lapU,u,g)
       ! public :: myNodeCurl           ! call myNodeCurl(curlU,u,v,w,g[,n])

       ! Edge-centered derivatives
       public :: myEdge2FaceCurl      ! call myEdge2FaceCurl(curl,u,v,w,g,curlDir)
       
       ! ----------------------------- SPECIAL TERMS -------------------------------------------
       public :: myCCBfieldAdvect            ! call myCCBfieldAdvect(div,u,v,w,Bx,By,Bz,g,dir)
       public :: myCCBfieldDiffuse           ! call myCCBfieldDiffuse(div,Bx,By,Bz,sigma,mu,g,dir)

       ! interface CC2CCLap;   module procedure CC2CCLapVariCoeff;   end interface
       ! interface CC2CCLap;   module procedure CC2CCLapUnifCoeff;   end interface

       interface lap;        module procedure lapUniformCoeffSF;     end interface
       interface lap;        module procedure lapVariableCoeffSF;    end interface
       interface lap;        module procedure lapUniformCoeffVF;     end interface
       interface lap;        module procedure lapVariableCoeffVF;    end interface

       interface myCollocatedCross;   module procedure myCollocatedCrossSF;  end interface
       interface myCollocatedCross;   module procedure myCollocatedCrossVF;  end interface

       contains

       ! ******************************* OTHER ROUTINES *********************************

       subroutine collocatedMagnitudeVF(mag,V)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: mag
         type(vectorfield),intent(inout) :: V
         call collocatedMagnitude(mag,V%x,V%y,V%z)
       end subroutine

       subroutine totalEnergyVF(e,VF,g)
         implicit none
         type(vectorField),intent(in) :: VF
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         call totalEnergy(e,VF%x,VF%y,VF%z,g)
       end subroutine

       subroutine printPhysicalMinMaxVF(u,name)
         implicit none
         type(vectorField),intent(in) :: u
         character(len=*),intent(in) :: name
         call printPhysicalMinMax(U%x,U%sx,name//'x')
         call printPhysicalMinMax(U%y,U%sy,name//'y')
         call printPhysicalMinMax(U%z,U%sz,name//'z')
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ***************************** DATA GENERAL DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myCollocatedCrossVF(AcrossB,A,B)
         implicit none
         type(vectorField),intent(inout) :: AcrossB
         type(vectorField),intent(in) :: A,B
         call myCollocatedCrossSF(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1)
         call myCollocatedCrossSF(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2)
         call myCollocatedCrossSF(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3)
       end subroutine

       subroutine lapUniformCoeffVF(lapU,u,g)
         implicit none
         type(vectorField),intent(inout) :: lapU
         type(vectorField),intent(in) :: u
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k,g)
         call lap(lapU%y,U%y,k,g)
         call lap(lapU%z,U%z,k,g)
       end subroutine

       subroutine lapVariableCoeffVF(lapU,u,k,g,dir)
         implicit none
         type(vectorField),intent(inout) :: lapU
         type(vectorField),intent(in) :: u,k
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k,g)
         call lap(lapU%y,U%y,k,g)
         call lap(lapU%z,U%z,k,g)
       end subroutine


       end module