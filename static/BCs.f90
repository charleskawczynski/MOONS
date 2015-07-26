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

       public :: setXminType,getXminType
       public :: setXmaxType,getXmaxType
       public :: setYminType,getYminType
       public :: setYmaxType,getYmaxType
       public :: setZminType,getZminType
       public :: setZmaxType,getZmaxType

       public :: getXminVals,setXminVals
       public :: getXmaxVals,setXmaxVals
       public :: getYminVals,setYminVals
       public :: getYmaxVals,setYmaxVals
       public :: getZminVals,setZminVals
       public :: getZmaxVals,setZmaxVals

       public :: checkBCs
       public :: BCsReady
       public :: setAllBVals

       public :: setAllZero
       public :: getAllDirichlet,allNeumann

       public :: printAllBoundaries
       public :: writeAllBoundaries

       type BCs
         integer :: xminType,xmaxType
         integer :: yminType,ymaxType
         integer :: zminType,zmaxType
         real(cp),dimension(:,:),allocatable :: xminVals,xmaxVals
         real(cp),dimension(:,:),allocatable :: yminVals,ymaxVals
         real(cp),dimension(:,:),allocatable :: zminVals,zmaxVals
         real(cp),dimension(:),allocatable :: xn,yn,zn
         real(cp),dimension(:),allocatable :: xc,yc,zc
         integer,dimension(3) :: s
         logical,dimension(6) :: TFb
         logical :: TFs
         logical :: TFgrid
         logical,dimension(6) :: TFvals
         logical :: BCsDefined = .false.
         type(grid) :: g
       end type

       interface init;       module procedure initBCs;             end interface
       interface init;       module procedure initBCsCopy;         end interface
       interface init;       module procedure initBCsSize;         end interface
       interface delete;     module procedure deleteBCs;           end interface
       interface setGrid;    module procedure setGridBCs;          end interface

       interface setAllZero; module procedure setAllZeroGivenSize; end interface
       interface setAllZero; module procedure setAllZeroNoSize;    end interface

       contains

       subroutine initBCs(this)
         implicit none
         type(BCs),intent(inout) :: this
         this%xminType = 0; this%xmaxType = 0
         this%yminType = 0; this%ymaxType = 0
         this%zminType = 0; this%zmaxType = 0
         this%TFb = .false.
         this%TFs = .false.
         this%BCsDefined = .false.
       end subroutine

       subroutine initBCsSize(this,Nx,Ny,Nz)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: Nx,Ny,Nz
         call initBCs(this)
         this%s = (/Nx,Ny,Nz/)
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

         this%s = u_bcs%s
         Nx = this%s(1); Ny = this%s(2); Nz = this%s(3)

         allocate(bvals(Ny,Nz));
         call getXminVals(u_bcs,bvals)
         call setXminVals(this,bvals)
         deallocate(bvals)

         allocate(bvals(Ny,Nz));
         call getXmaxVals(u_bcs,bvals)
         call setXmaxVals(this,bvals)
         deallocate(bvals)

         allocate(bvals(Nx,Nz));
         call getYminVals(u_bcs,bvals)
         call setYminVals(this,bvals)
         deallocate(bvals)

         allocate(bvals(Nx,Nz));
         call getYmaxVals(u_bcs,bvals)
         call setYmaxVals(this,bvals)
         deallocate(bvals)

         allocate(bvals(Nx,Ny));
         call getZminVals(u_bcs,bvals)
         call setZminVals(this,bvals)
         deallocate(bvals)

         allocate(bvals(Nx,Ny));
         call getZmaxVals(u_bcs,bvals)
         call setZmaxVals(this,bvals)
         deallocate(bvals)

         this%TFb = .true.
         this%BCsDefined = .true.
       end subroutine

       subroutine setAllBVals(this,u)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:,:),intent(in) :: u
         integer,dimension(3) :: s
         s = this%s
         call setXminVals(this,u(1,:,:))
         call setYminVals(this,u(:,1,:))
         call setZminVals(this,u(:,:,1))
         call setXmaxVals(this,u(s(1),:,:))
         call setYmaxVals(this,u(:,s(2),:))
         call setZmaxVals(this,u(:,:,s(3)))
       end subroutine

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
         this%xc = g%c(1)%hc
         this%xn = g%c(1)%hn
         this%yc = g%c(2)%hc
         this%yn = g%c(2)%hn
         this%zc = g%c(3)%hc
         this%zn = g%c(3)%hn
         call init(this%g,g)
         this%TFgrid = .true.
       end subroutine

       subroutine setXminVals(this,xminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: xminVals
         integer,dimension(2) :: s
         s = shape(xminVals)
         if (allocated(this%xminVals)) deallocate(this%xminVals)
         allocate(this%xminVals(s(1),s(2)))
         this%xminVals = xminVals
         this%TFvals(1) = .true.
       end subroutine

       subroutine getXminVals(this,xminVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: xminVals
         xminVals = this%xminVals
       end subroutine

       subroutine setXmaxVals(this,xmaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: xmaxVals
         integer,dimension(2) :: s
         s = shape(xmaxVals)
         if (allocated(this%xmaxVals)) deallocate(this%xmaxVals)
         allocate(this%xmaxVals(s(1),s(2)))
         this%xmaxVals = xmaxVals
         this%TFvals(2) = .true.
       end subroutine

       subroutine getXmaxVals(this,xmaxVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: xmaxVals
         xmaxVals = this%xmaxVals
       end subroutine

       subroutine setYminVals(this,yminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: yminVals
         integer,dimension(2) :: s
         s = shape(yminVals)
         if (allocated(this%yminVals)) deallocate(this%yminVals)
         allocate(this%yminVals(s(1),s(2)))
         this%yminVals = yminVals
         this%TFvals(3) = .true.
       end subroutine

       subroutine getYminVals(this,yminVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: yminVals
         yminVals = this%yminVals
       end subroutine

       subroutine setYmaxVals(this,ymaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: ymaxVals
         integer,dimension(2) :: s
         s = shape(ymaxVals)
         if (allocated(this%ymaxVals)) deallocate(this%ymaxVals)
         allocate(this%ymaxVals(s(1),s(2)))
         this%ymaxVals = ymaxVals
         this%TFvals(4) = .true.
       end subroutine

       subroutine getYmaxVals(this,ymaxVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: ymaxVals
         ymaxVals = this%ymaxVals
       end subroutine

       subroutine setZminVals(this,zminVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: zminVals
         integer,dimension(2) :: s
         s = shape(zminVals)
         if (allocated(this%zminVals)) deallocate(this%zminVals)
         allocate(this%zminVals(s(1),s(2)))
         this%zminVals = zminVals
         this%TFvals(5) = .true.
       end subroutine

       subroutine getZminVals(this,zminVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: zminVals
         zminVals = this%zminVals
       end subroutine

       subroutine setZmaxVals(this,zmaxVals)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp),dimension(:,:),intent(in) :: zmaxVals
         integer,dimension(2) :: s
         s = shape(zmaxVals)
         if (allocated(this%zmaxVals)) deallocate(this%zmaxVals)
         allocate(this%zmaxVals(s(1),s(2)))
         this%zmaxVals = zmaxVals
         this%TFvals(6) = .true.
       end subroutine

       subroutine getZmaxVals(this,zmaxVals)
         implicit none
         type(BCs),intent(in) :: this
         real(cp),dimension(:,:),intent(inout) :: zmaxVals
         zmaxVals = this%zmaxVals
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
       end subroutine

       subroutine setAllZeroNoSize(this,bctype)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: bctype
         call setAllZero(this,this%s(1),this%s(2),this%s(3),bctype)
       end subroutine

       subroutine setXminType(this,xminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: xminType
         this%xminType = xminType
         this%TFb(1) = .true.
       end subroutine

       subroutine setXmaxType(this,xmaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: xmaxType
         this%xmaxType = xmaxType
         this%TFb(2) = .true.
       end subroutine

       subroutine setYminType(this,yminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: yminType
         this%yminType = yminType
         this%TFb(3) = .true.
       end subroutine

       subroutine setYmaxType(this,ymaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: ymaxType
         this%ymaxType = ymaxType
         this%TFb(4) = .true.
       end subroutine

       subroutine setZminType(this,zminType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: zminType
         this%zminType = zminType
         this%TFb(5) = .true.
       end subroutine

       subroutine setZmaxType(this,zmaxType)
         implicit none
         type(BCs),intent(inout) :: this
         integer,intent(in) :: zmaxType
         this%zmaxType = zmaxType
         this%TFb(6) = .true.
       end subroutine

! **************************************************************

       function getXminType(this) result(xminType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: xminType
         xminType = this%xminType
       end function

       function getXmaxType(this) result(xmaxType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: xmaxType
         xmaxType = this%xmaxType
       end function

       function getYminType(this) result(yminType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: yminType
         yminType = this%yminType
       end function

       function getYmaxType(this) result(ymaxType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: ymaxType
         ymaxType = this%ymaxType
       end function

       function getZminType(this) result(zminType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: zminType
         zminType = this%zminType
       end function

       function getZmaxType(this) result(zmaxType)
         implicit none
         type(BCs),intent(in) :: this
         integer :: zmaxType
         zmaxType = this%zmaxType
       end function

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
           write(*,*) 'BCs have not been fully defined. Terminating'
           stop
         endif
       end subroutine

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

       function getAllDirichlet(this) result(TF)
         implicit none
         type(BCs),intent(in) :: this
         logical :: TF
         TF = .true.

         if (TF.and.this%TFb(1)) TF = (getXminType(this).eq.1)
         if (TF.and.this%TFb(2)) TF = (getXmaxType(this).eq.1)
         if (TF.and.this%TFb(3)) TF = (getYminType(this).eq.1)
         if (TF.and.this%TFb(4)) TF = (getYmaxType(this).eq.1)
         if (TF.and.this%TFb(5)) TF = (getZminType(this).eq.1)
         if (TF.and.this%TFb(6)) TF = (getZmaxType(this).eq.1)
       end function

       function allNeumann(this) result(TF)
         implicit none
         type(BCs),intent(in) :: this
         logical,dimension(6) :: TFAll
         logical :: TF
         TFAll = .false.

         if (this%TFb(1)) TFAll(1) = (getXminType(this).eq.3).or.((getXminType(this).eq.4).or.(getXminType(this).eq.5))
         if (this%TFb(2)) TFAll(2) = (getXmaxType(this).eq.3).or.((getXmaxType(this).eq.4).or.(getXmaxType(this).eq.5))
         if (this%TFb(3)) TFAll(3) = (getYminType(this).eq.3).or.((getYminType(this).eq.4).or.(getYminType(this).eq.5))
         if (this%TFb(4)) TFAll(4) = (getYmaxType(this).eq.3).or.((getYmaxType(this).eq.4).or.(getYmaxType(this).eq.5))
         if (this%TFb(5)) TFAll(5) = (getZminType(this).eq.3).or.((getZminType(this).eq.4).or.(getZminType(this).eq.5))
         if (this%TFb(6)) TFAll(6) = (getZmaxType(this).eq.3).or.((getZmaxType(this).eq.4).or.(getZmaxType(this).eq.5))
         TF = all(TFAll)
       end function

       end module