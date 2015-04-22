       module vectorOps_mod
       use delOps_mod
       use grid_mod
       use vectorField_mod

       implicit none

       private

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
       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPointsVF;       end interface

       public :: collocatedMagnitude
       interface collocatedMagnitude;     module procedure collocatedMagnitudeVF;   end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergyVF;           end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxVF;   end interface

       ! --------------------------------- DERIVATIVE ROUTINES ---------------------------------

       public :: lap
       interface lap;                     module procedure lapUniformCoeffVF;       end interface
       interface lap;                     module procedure lapVariableCoeffVF;      end interface

       public :: div
       interface div;                     module procedure divVF;                   end interface

       public :: grad
       interface grad;                    module procedure gradVF;                  end interface

       public :: curl
       interface curl;                    module procedure curlVF;                  end interface

       public :: cross
       interface cross;                   module procedure collocatedCrossVF;       end interface

       contains

       ! ******************************* OTHER ROUTINES *********************************

       subroutine collocatedMagnitudeVF(mag,V)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: mag
         type(vectorfield),intent(in) :: V
         call collocatedMagnitude(mag,V%x,V%y,V%z)
       end subroutine

       subroutine totalEnergyVF(e,VF,g)
         implicit none
         type(vectorField),intent(in) :: VF
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         call totalEnergy(e,VF%x,VF%y,VF%z,g)
       end subroutine

       subroutine zeroGhostPointsVF(VF)
         implicit none
         type(vectorField),intent(inout) :: VF
         call zeroGhostPoints(VF%x)
         call zeroGhostPoints(VF%y)
         call zeroGhostPoints(VF%z)
       end subroutine

       subroutine printPhysicalMinMaxVF(u,name)
         implicit none
         type(vectorField),intent(in) :: u
         character(len=*),intent(in) :: name
         call printPhysicalMinMax(U%x,U%sx,name//'x')
         call printPhysicalMinMax(U%y,U%sy,name//'y')
         call printPhysicalMinMax(U%z,U%sz,name//'z')
       end subroutine

       subroutine collocatedCrossVF(AcrossB,A,B)
         implicit none
         type(vectorField),intent(inout) :: AcrossB
         type(vectorField),intent(in) :: A,B
         call cross(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1)
         call cross(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2)
         call cross(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3)
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ***************************** DATA GENERAL DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine lapUniformCoeffVF(lapU,u,g)
         implicit none
         type(vectorField),intent(inout) :: lapU
         type(vectorField),intent(in) :: u
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,g)
         call lap(lapU%y,U%y,g)
         call lap(lapU%z,U%z,g)
       end subroutine

       subroutine lapVariableCoeffVF(lapU,u,k,g)
         implicit none
         type(vectorField),intent(inout) :: lapU
         type(vectorField),intent(in) :: u,k
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k%x,g)
         call lap(lapU%y,U%y,k%y,g)
         call lap(lapU%z,U%z,k%z,g)
       end subroutine

       subroutine divVF(divU,U,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         call div(divU,U%x,U%y,U%z,g)
       end subroutine

       subroutine curlVF(curlU,U,g)
         implicit none
         type(vectorField),intent(inout) :: curlU
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         call curl(curlU%x,U%x,U%y,U%z,g,1)
         call curl(curlU%y,U%x,U%y,U%z,g,2)
         call curl(curlU%z,U%x,U%y,U%z,g,3)
       end subroutine

       subroutine gradVF(gradU,U,g)
         implicit none
         type(vectorField),intent(inout) :: gradU
         real(cp),dimension(:,:,:),intent(in) :: U
         type(grid),intent(in) :: g
         call grad(gradU%x,gradU%y,gradU%z,U,g)
       end subroutine

       end module