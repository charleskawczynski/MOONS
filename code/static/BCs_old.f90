       module BCs_mod
       ! This is the BCs module. Here is an example of implementation
       ! of setting and applying BCs for the magnetic field using Psuedo-vacuum BCs.
       ! 
       !           type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
       !           integer :: Nx,Ny,Nz,neumann,dirichlet
       !  
       !           ! B-field boundary conditions
       !           dirichlet = 2; neumann = 5 ! See applyBCs module
       !           
       !           call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
       !           call setXminType(Bx_bcs,neumann)
       !           call setXmaxType(Bx_bcs,neumann)
       !           call checkBCs(Bx_bcs)
       !  
       !           call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
       !           call setYminType(By_bcs,neumann)
       !           call setYmaxType(By_bcs,neumann)
       !           call checkBCs(By_bcs)
       !  
       !           call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
       !           call setZminType(Bz_bcs,neumann)
       !           call setZmaxType(Bz_bcs,neumann)
       !           call checkBCs(Bz_bcs)
       !           .
       !           .
       !           .
       !           call myAdvect(tempx,Bx0,By0,Bz0,u,gd)
       !           call myPoisson(Bx,-Rem*tempx,B_bcs,gd)
       !  
       !           call myAdvect(tempy,Bx0,By0,Bz0,v,gd)
       !           call myPoisson(By,-Rem*tempy,B_bcs,gd)
       !  
       !           call myAdvect(tempz,Bx0,By0,Bz0,w,gd)
       !           call myPoisson(Bz,-Rem*tempz,B_bcs,gd)
       !           .
       !           .
       !           .

       use grid_mod
       use IO_tools_mod
       implicit none

       private

       public :: BCs
       public :: init,delete,setGrid

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       ! Setters for type and value
       public :: setXminType,setXmaxType
       public :: setYminType,setYmaxType
       public :: setZminType,setZmaxType
       public :: setXminVals,setXmaxVals
       public :: setYminVals,setYmaxVals
       public :: setZminVals,setZmaxVals

       ! Modifiers
       public :: setAllBVals
       public :: setAllZero

       ! Checkers
       public :: checkBCs,BCsReady
       public :: getAllDirichlet,getAllNeumann

       ! Print / export
       public :: printAllBoundaries
       public :: writeAllBoundaries

       type BCs
         integer :: xminType,xmaxType                               ! good
         integer :: yminType,ymaxType                               ! good
         integer :: zminType,zmaxType                               ! good
         real(cp),dimension(:,:),allocatable :: xminVals,xmaxVals   ! good
         real(cp),dimension(:,:),allocatable :: yminVals,ymaxVals   ! good
         real(cp),dimension(:,:),allocatable :: zminVals,zmaxVals   ! good
         integer,dimension(3) :: s

         real(cp),dimension(:),allocatable :: xn,yn,zn
         real(cp),dimension(:),allocatable :: xc,yc,zc
         logical,dimension(6) :: TFb
         logical :: TFs
         logical :: TFgrid,allDirichlet,allNeumann
         logical,dimension(6) :: TFvals
         logical :: BCsDefined = .false.
         type(grid) :: g
       end type

       interface init;       module procedure initBCs;               end interface
       interface init;       module procedure initBCsCopy;           end interface
       interface init;       module procedure initBCsSize;           end interface
       interface init;       module procedure initBCsSize2;          end interface
       interface delete;     module procedure deleteBCs;             end interface
       interface setGrid;    module procedure setGridBCs;            end interface

       interface setAllZero; module procedure setAllZeroGivenSize;   end interface
       interface setAllZero; module procedure setAllZeroGivenSize_s; end interface
       interface setAllZero; module procedure setAllZeroNoSize;      end interface

       contains


       ! *******************************************************************************
       ! *******************************************************************************
       ! ************************************* INIT ************************************
       ! *******************************************************************************
       ! *******************************************************************************

       subroutine initBCs(this)
         implicit none
         type(BCs),intent(inout) :: this
         this%xminType = 0; this%xmaxType = 0
         this%yminType = 0; this%ymaxType = 0
         this%zminType = 0; this%zmaxType = 0
         this%TFb = .false.
         this%TFs = .false.
         this%BCsDefined = .false.
         call setAllDirichlet_TF(this)
         call setAllNeumann_TF(this)
       end subroutine

       subroutine initBCsSize(this,Nx,Ny,Nz)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: Nx,Ny,Nz
         call initBCs(this)
         this%s = (/Nx,Ny,Nz/)
       end subroutine

       subroutine initBCsSize2(this,s)
         implicit none
         type(BCs),intent(inout) :: this
         integer,dimension(3),intent(in) :: s
         call initBCs(this)
         this%s = s
       end subroutine

       subroutine initBCsCopy(this,u_bcs)
         implicit none
         type(BCs),intent(inout) :: this
         type(BCs),intent(in) :: u_bcs
         real(cp),dimension(:,:), allocatable :: bvals
         integer :: Nx,Ny,Nz
         this%xminType = u_bcs%xminType; this%xmaxType = u_bcs%xmaxType
         this%yminType = u_bcs%yminType; this%ymaxType = u_bcs%ymaxType
         this%zminType = u_bcs%zminType; this%zmaxType = u_bcs%zmaxType
         call init(this%g,u_bcs%g)

         this%s = u_bcs%s; Nx = this%s(1); Ny = this%s(2); Nz = this%s(3)

         allocate(bvals(Ny,Nz)); bvals = u_bcs%xminVals
         call setXminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Ny,Nz)); bvals = u_bcs%xmaxVals
         call setXmaxVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Nz)); bvals = u_bcs%yminVals
         call setYminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Nz)); bvals = u_bcs%ymaxVals
         call setYmaxVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Ny)); bvals = u_bcs%zminVals
         call setZminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Ny)); bvals = u_bcs%zmaxVals
         call setZmaxVals(this,bvals); deallocate(bvals)

         this%TFb = .true.
         this%BCsDefined = .true.
         call setAllDirichlet_TF(this)
         call setAllNeumann_TF(this)
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! *********************************** DELETE ************************************
       ! *******************************************************************************
       ! *******************************************************************************

       subroutine deleteBCs(this)
         implicit none
         type(BCs),intent(inout) :: this
         if (allocated(this%xminVals)) deallocate(this%xminVals)
         if (allocated(this%xmaxVals)) deallocate(this%xmaxVals)
         if (allocated(this%yminVals)) deallocate(this%yminVals)
         if (allocated(this%ymaxVals)) deallocate(this%ymaxVals)
         if (allocated(this%zminVals)) deallocate(this%zminVals)
         if (allocated(this%zmaxVals)) deallocate(this%zmaxVals)

         if (allocated(this%xn)) deallocate(this%xn)
         if (allocated(this%yn)) deallocate(this%yn)
         if (allocated(this%zn)) deallocate(this%zn)
         if (allocated(this%xc)) deallocate(this%xc)
         if (allocated(this%yc)) deallocate(this%yc)
         if (allocated(this%zc)) deallocate(this%zc)
         this%TFb = .false.
         this%BCsDefined = .false.
         this%TFgrid = .false.
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! *********************************** MODIFY ************************************
       ! *******************************************************************************
       ! *******************************************************************************

       subroutine setAllBVals(this,u,g)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         integer,dimension(3) :: s
         integer :: i
         s = this%s
         if (all((/(s(i).eq.g%c(i)%sn,i=1,3)/))) then
           call setXminVals(this,u(2,:,:))
           call setYminVals(this,u(:,2,:))
           call setZminVals(this,u(:,:,2))
           call setXmaxVals(this,u(s(1)-1,:,:))
           call setYmaxVals(this,u(:,s(2)-1,:))
           call setZmaxVals(this,u(:,:,s(3)-1))
         elseif (all((/(s(i).eq.g%c(i)%sc,i=1,3)/))) then
           call setXminVals(this,0.5_cp*(u(1,:,:)+u(2,:,:)))
           call setYminVals(this,0.5_cp*(u(:,1,:)+u(:,2,:)))
           call setZminVals(this,0.5_cp*(u(:,:,1)+u(:,:,2)))
           call setXmaxVals(this,0.5_cp*(u(s(1),:,:)+u(s(1)-1,:,:)))
           call setYmaxVals(this,0.5_cp*(u(:,s(2),:)+u(:,s(2)-1,:)))
           call setZmaxVals(this,0.5_cp*(u(:,:,s(3))+u(:,:,s(3)-1)))
         else
           write(*,*) 's = ',s
           write(*,*) 'sn = ',g%c(1)%sn,g%c(2)%sn,g%c(3)%sn
           write(*,*) 'TF1 = ',(/(s(i).eq.g%c(i)%sn,i=1,3)/)
           stop 'Error: setAllBVals not supported for face/edge data in BCs.f90'
         endif
       end subroutine

       subroutine setGridBCs(this,g)
         implicit none
         type(BCs),intent(inout) :: this
         type(grid),intent(in) :: g
         if (allocated(this%xc)) deallocate(this%xc)
         if (allocated(this%yc)) deallocate(this%yc)
         if (allocated(this%zc)) deallocate(this%zc)
         if (allocated(this%xn)) deallocate(this%xn)
         if (allocated(this%yn)) deallocate(this%yn)
         if (allocated(this%zn)) deallocate(this%zn)
         allocate(this%xc(g%c(1)%sc),this%xn(g%c(1)%sn))
         allocate(this%yc(g%c(2)%sc),this%yn(g%c(2)%sn))
         allocate(this%zc(g%c(3)%sc),this%zn(g%c(3)%sn))
         this%xc = g%c(1)%hc; this%xn = g%c(1)%hn
         this%yc = g%c(2)%hc; this%yn = g%c(2)%hn
         this%zc = g%c(3)%hc; this%zn = g%c(3)%hn
         call init(this%g,g)
         this%TFgrid = .true.
       end subroutine

       subroutine setAllZeroGivenSize(this,Nx,Ny,Nz,bctype)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: Nx,Ny,Nz
         integer,intent(in) :: bctype
         real(cp),dimension(:,:),allocatable :: bvals

         call initBCs(this)
         this%s = (/Nx,Ny,Nz/)

         allocate(bvals(Ny,Nz)); call setXminType(this,bctype)
         bvals = 0.0_cp; call setXminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Ny,Nz)); call setXmaxType(this,bctype)
         bvals = 0.0_cp; call setXmaxVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Nz)); call setYminType(this,bctype)
         bvals = 0.0_cp; call setYminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Nz)); call setYmaxType(this,bctype)
         bvals = 0.0_cp; call setYmaxVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Ny)); call setZminType(this,bctype)
         bvals = 0.0_cp; call setZminVals(this,bvals); deallocate(bvals)
         allocate(bvals(Nx,Ny)); call setZmaxType(this,bctype)
         bvals = 0.0_cp; call setZmaxVals(this,bvals); deallocate(bvals)
         call setAllDirichlet_TF(this)
         call setAllNeumann_TF(this)
       end subroutine

       subroutine setAllZeroGivenSize_s(this,s,bctype)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: bctype
         integer,dimension(3),intent(in) :: s
         call setAllZeroGivenSize(this,s(1),s(2),s(3),bctype)
       end subroutine

       subroutine setAllZeroNoSize(this,bctype)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: bctype
         call setAllZero(this,this%s(1),this%s(2),this%s(3),bctype)
         call setAllDirichlet_TF(this)
         call setAllNeumann_TF(this)
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! ********************************* CHECKERS ************************************
       ! *******************************************************************************
       ! *******************************************************************************

       function BCsReady(this) result(TF)
         implicit none
         type(BCs),intent(in) :: this
         logical :: TF
         TF = this%BCsDefined
       end function

       subroutine checkBCs(this)
         implicit none
         type(BCs),intent(inout) :: this
         this%BCsDefined = all((/this%TFb,this%TFvals,this%TFgrid/))
         if (.not.this%BCsDefined) then
           write(*,*) 'TFb,TFvals,TFgrid = ',this%TFb,this%TFvals,this%TFgrid
           write(*,*) 'BCs have not been fully defined. Terminating'; stop
         endif
       end subroutine

       subroutine setAllDirichlet_TF(this)
         implicit none
         type(BCs),intent(inout) :: this
         logical :: TF
         TF = .true.
         if (TF.and.this%TFb(1)) TF = (this%xminType.eq.1)
         if (TF.and.this%TFb(2)) TF = (this%xmaxType.eq.1)
         if (TF.and.this%TFb(3)) TF = (this%yminType.eq.1)
         if (TF.and.this%TFb(4)) TF = (this%ymaxType.eq.1)
         if (TF.and.this%TFb(5)) TF = (this%zminType.eq.1)
         if (TF.and.this%TFb(6)) TF = (this%zmaxType.eq.1)
         this%allDirichlet = TF
       end subroutine

       subroutine setAllNeumann_TF(this)
         implicit none
         type(BCs),intent(inout) :: this
         logical,dimension(6) :: TFAll
         TFAll = .false.
         if (this%TFb(1)) TFAll(1) = (this%xminType.eq.3).or.((this%xminType.eq.4).or.(this%xminType.eq.5))
         if (this%TFb(2)) TFAll(2) = (this%xmaxType.eq.3).or.((this%xmaxType.eq.4).or.(this%xmaxType.eq.5))
         if (this%TFb(3)) TFAll(3) = (this%yminType.eq.3).or.((this%yminType.eq.4).or.(this%yminType.eq.5))
         if (this%TFb(4)) TFAll(4) = (this%ymaxType.eq.3).or.((this%ymaxType.eq.4).or.(this%ymaxType.eq.5))
         if (this%TFb(5)) TFAll(5) = (this%zminType.eq.3).or.((this%zminType.eq.4).or.(this%zminType.eq.5))
         if (this%TFb(6)) TFAll(6) = (this%zmaxType.eq.3).or.((this%zmaxType.eq.4).or.(this%zmaxType.eq.5))
         this%allNeumann = all(TFAll)
       end subroutine

       function getAllDirichlet(this) result(TF)
         implicit none
         type(BCs),intent(in) :: this
         logical :: TF
         TF = this%allDirichlet
       end function

       function getAllNeumann(this) result(TF)
         implicit none
         type(BCs),intent(in) :: this
         logical :: TF
         TF = this%allNeumann
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************
       ! *******************************************************************************

       subroutine printAllBoundaries(this,name)
         implicit none
         type(BCs), intent(in) :: this
         character(len=*),intent(in) :: name
         call writeAllBCsToFileOrScreen(this,name,6)
       end subroutine

       subroutine writeAllBoundaries(this,dir,name)
         implicit none
         type(BCs), intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: NewU
         NewU = newAndOpen(dir,name//'_BoundaryConditions')
         call writeAllBCsToFileOrScreen(this,name,newU)
         call closeAndMessage(newU,name//'_BoundaryConditions',dir)
       end subroutine

       subroutine writeAllBCsToFileOrScreen(this,name,newU)
         implicit none
         type(BCs), intent(in) :: this
         character(len=*),intent(in) :: name
         integer,intent(in) :: NewU
         write(newU,*) 'Boundary conditions for ' // trim(adjustl(name))
         ! write(*,*) 'shape(BC) = ',this%s
         if (this%TFb(1)) then; call writeBoundary(1,this%xminType,newU); endif
         if (this%TFb(2)) then; call writeBoundary(2,this%xmaxType,newU); endif
         if (this%TFb(3)) then; call writeBoundary(3,this%yminType,newU); endif
         if (this%TFb(4)) then; call writeBoundary(4,this%ymaxType,newU); endif
         if (this%TFb(5)) then; call writeBoundary(5,this%zminType,newU); endif
         if (this%TFb(6)) then; call writeBoundary(6,this%zmaxType,newU); endif
       end subroutine

       subroutine writeBoundary(face,bctype,NewU)
         implicit none
         integer,intent(in) :: NewU,face,bctype
         if (face.eq.1) then; write(newU,'(7A)',advance='no') ' xmin: '; endif
         if (face.eq.2) then; write(newU,'(7A)',advance='no') ' xmax: '; endif
         if (face.eq.3) then; write(newU,'(7A)',advance='no') ' ymin: '; endif
         if (face.eq.4) then; write(newU,'(7A)',advance='no') ' ymax: '; endif
         if (face.eq.5) then; write(newU,'(7A)',advance='no') ' zmin: '; endif
         if (face.eq.6) then; write(newU,'(7A)',advance='no') ' zmax: '; endif
         if (bctype.eq.1) then; write(newU,*) 'Dirichlet - direct - wall coincident'; endif
         if (bctype.eq.2) then; write(newU,*) 'Dirichlet - interpolated - wall incoincident'; endif
         if (bctype.eq.3) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh^2)'; endif
         if (bctype.eq.4) then; write(newU,*) 'Neumann - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.5) then; write(newU,*) 'Neumann - interpolated - wall incoincident O(dh)'; endif
         if (bctype.eq.6) then; write(newU,*) 'Periodic - direct - wall coincident ~O(dh)'; endif
         if (bctype.eq.7) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh)'; endif
         if (bctype.eq.8) then; write(newU,*) 'Periodic - interpolated - wall incoincident ~O(dh^2)'; endif
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! *********************** Setters for setVals and setType ***********************
       ! *******************************************************************************
       ! *******************************************************************************

       subroutine setXminVals(this,xminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: xminVals
         integer,dimension(2) :: s
         if (allocated(this%xminVals)) deallocate(this%xminVals)
         s = shape(xminVals); allocate(this%xminVals(s(1),s(2)))
         this%xminVals = xminVals; this%TFvals(1) = .true.
       end subroutine

       subroutine setXmaxVals(this,xmaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: xmaxVals
         integer,dimension(2) :: s
         if (allocated(this%xmaxVals)) deallocate(this%xmaxVals)
         s = shape(xmaxVals); allocate(this%xmaxVals(s(1),s(2)))
         this%xmaxVals = xmaxVals; this%TFvals(2) = .true.
       end subroutine

       subroutine setYminVals(this,yminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: yminVals
         integer,dimension(2) :: s
         if (allocated(this%yminVals)) deallocate(this%yminVals)
         s = shape(yminVals); allocate(this%yminVals(s(1),s(2)))
         this%yminVals = yminVals; this%TFvals(3) = .true.
       end subroutine

       subroutine setYmaxVals(this,ymaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: ymaxVals
         integer,dimension(2) :: s
         if (allocated(this%ymaxVals)) deallocate(this%ymaxVals)
         s = shape(ymaxVals); allocate(this%ymaxVals(s(1),s(2)))
         this%ymaxVals = ymaxVals; this%TFvals(4) = .true.
       end subroutine

       subroutine setZminVals(this,zminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: zminVals
         integer,dimension(2) :: s
         if (allocated(this%zminVals)) deallocate(this%zminVals)
         s = shape(zminVals); allocate(this%zminVals(s(1),s(2)))
         this%zminVals = zminVals; this%TFvals(5) = .true.
       end subroutine

       subroutine setZmaxVals(this,zmaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: zmaxVals
         integer,dimension(2) :: s
         if (allocated(this%zmaxVals)) deallocate(this%zmaxVals)
         s = shape(zmaxVals); allocate(this%zmaxVals(s(1),s(2)))
         this%zmaxVals = zmaxVals; this%TFvals(6) = .true.
       end subroutine

       subroutine setXminType(this,xminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: xminType
         this%xminType = xminType; this%TFb(1) = .true.
       end subroutine

       subroutine setXmaxType(this,xmaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: xmaxType
         this%xmaxType = xmaxType; this%TFb(2) = .true.
       end subroutine

       subroutine setYminType(this,yminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: yminType
         this%yminType = yminType; this%TFb(3) = .true.
       end subroutine

       subroutine setYmaxType(this,ymaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: ymaxType
         this%ymaxType = ymaxType; this%TFb(4) = .true.
       end subroutine

       subroutine setZminType(this,zminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: zminType
         this%zminType = zminType; this%TFb(5) = .true.
       end subroutine

       subroutine setZmaxType(this,zmaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: zmaxType
         this%zmaxType = zmaxType; this%TFb(6) = .true.
       end subroutine

       end module