       module ops_discrete_mod
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
       use interpOps_mod
       use ops_aux_mod

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

       public :: cross
       interface cross; module procedure crossSF;            end interface
       interface cross; module procedure collocatedCrossVF;  end interface

       public :: lap
       interface lap;   module procedure lapUniformCoeffSF;  end interface
       interface lap;   module procedure lapVariableCoeffSF; end interface
       interface lap;   module procedure lapUniformCoeffVF;  end interface
       interface lap;   module procedure lapVariableCoeffVF; end interface

       public :: div
       interface div;   module procedure divSF;              end interface
       interface div;   module procedure divVF;              end interface

       public :: grad
       interface grad;  module procedure gradSF;             end interface
       interface grad;  module procedure gradVF;             end interface

       public :: curl
       interface curl;  module procedure curlSF;             end interface
       interface curl;  module procedure curlVF;             end interface

       public :: CCVaryDel
       ! interface gradGrad; module procedure gradGradVary;    end interface

       contains

       subroutine crossSF(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir) ! Finished
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: AcrossB
         real(cp),dimension(:,:,:),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,intent(in) :: dir
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(Ax)
         select case (dir)
         case (1)
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             AcrossB(i,j,k) = Ay(i,j,k)*Bz(i,j,k) - Az(i,j,k)*By(i,j,k)
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             AcrossB(i,j,k) = -(Ax(i,j,k)*Bz(i,j,k) - Az(i,j,k)*Bx(i,j,k))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             AcrossB(i,j,k) = Ax(i,j,k)*By(i,j,k) - Ay(i,j,k)*Bx(i,j,k)
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case default
           stop 'Error: dir must = 1,2,3 in collocatedCrossSF.'
         end select
       end subroutine

       subroutine lapUniformCoeffSF(lapU,u,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(lapU,u,g,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lapVariableCoeffSF(lapU,u,k,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u,k
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),allocatable :: temp1
         integer,dimension(3) :: s
         s = shape(lapU)
         allocate(temp1(s(1),s(2),s(3)))
         call CCVaryDel(temp1,u,k,g,1,1)
         lapU = temp1
         call CCVaryDel(temp1,u,k,g,2,2)
         lapU = lapU+temp1
         call CCVaryDel(temp1,u,k,g,3,3)
         lapU = lapU+temp1
         deallocate(temp1)
       end subroutine

       subroutine divSF(divU,u,v,w,g) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(divU,u,g,1,1,1) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,g,1,2,1) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,g,1,3,1) ! Padding avoids calcs on fictive cells

         ! Note that padding above does not zero wall normal values
         ! from being calculated. Removing ghost nodes is still necessary:
         call zeroGhostPoints(divU) ! padding avoids boundaries
       end subroutine

       subroutine gradSF(gradx,grady,gradz,u,g) ! Finsihed
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(gradx,u,g,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,g,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,g,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curlSF(curlU,u,v,w,gd,dir) ! Finished
         ! curlSF computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curlU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d
         select case (dir)
         case (1); call d%assign(curlU,w,gd,1,2,0)
                 call d%subtract(curlU,v,gd,1,3,0)
         case (2); call d%assign(curlU,u,gd,1,3,0)
                 call d%subtract(curlU,w,gd,1,1,0)
         case (3); call d%assign(curlU,v,gd,1,1,0)
                 call d%subtract(curlU,u,gd,1,2,0)
         case default
           stop 'Error: dir must = 1,2,3 in curlSF.'
         end select
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCrossVF(AcrossB,A,B)
         implicit none
         type(vectorField),intent(inout) :: AcrossB
         type(vectorField),intent(in) :: A,B
         call cross(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1)
         call cross(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2)
         call cross(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3)
       end subroutine

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
         type(vectorField),intent(in) :: u
         real(cp),dimension(:,:,:),intent(in) :: k
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k,g)
         call lap(lapU%y,U%y,k,g)
         call lap(lapU%z,U%z,k,g)
       end subroutine

       subroutine divVF(divU,U,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         call div(divU,U%x,U%y,U%z,g)
       end subroutine

       subroutine gradVF(gradU,U,g)
         implicit none
         type(vectorField),intent(inout) :: gradU
         real(cp),dimension(:,:,:),intent(in) :: U
         type(grid),intent(in) :: g
         call grad(gradU%x,gradU%y,gradU%z,U,g)
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

       subroutine CCVaryDel(df,f,k,gd,dir1,dir2) ! Finished, needs to be checked
         ! CCVaryDel computes
         ! 
         !     d/dxj (k df/dxi)
         !        ^         ^
         !        |         |
         !       dir2      dir1
         ! 
         ! f and df live at the cell center.
         !
         !     dir1 == dir2
         !            upwind -> interpolate coefficient -> upwind
         !     dir1 ≠ dir2
         !            CD2 -> CD2
         ! 
         ! del should be capable of computing conservative derivatives.
         ! This routine needs adjusting to use this capability since
         ! it uses multiple cores more efficiently.
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: df
         real(cp),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir1,dir2
         real(cp),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         integer :: x,y,z
         type(del) :: d
         s = shape(f)
         select case (dir1)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir1 must = 1,2,3 in CCVaryDel.'; stop
         end select

         if (dir1.eq.dir2) then
           allocate(temp1(s(1)+x,s(2)+y,s(3)+z))
           allocate(temp2(s(1)+x,s(2)+y,s(3)+z))
           call d%assign(temp1,f,gd,1,dir1,0)
           call myCellCenter2Face(temp2,k,gd,dir1)
           temp1 = temp1*temp2
           call d%assign(temp2,temp1,gd,1,dir2,0)
           df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
           deallocate(temp1,temp2)
         else
           allocate(temp1(s(1),s(2),s(3)))
           allocate(temp2(s(1),s(2),s(3)))
           call d%assign(temp1,f,gd,1,dir1,0)
           temp1 = temp1*k
           call d%assign(temp2,temp1,gd,1,dir2,0)
           df = temp2
           deallocate(temp1,temp2)
         endif
       end subroutine

       end module