       module grids_mod
       use IO_tools_mod
       use IO_scalarBase_mod
       use coordinates_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private

       public :: grids
       public :: init,delete
       public :: print,export ! import
       public :: restrict

       type grids
         integer :: s
         type(grid),dimension(1) :: g
         ! type(grid),dimension(:),allocatable :: g ! grids
       end type

       interface init;       module procedure initGridCopy;   end interface
       interface init;       module procedure initGrid1;      end interface
       interface init;       module procedure initGrid2;      end interface
       interface init;       module procedure initGrid3;      end interface
       interface delete;     module procedure delete_grids;   end interface

       interface restrict;   module procedure restrictGrid1;  end interface
       interface restrict;   module procedure restrictGrid3;  end interface

       interface print;      module procedure printGrid;      end interface
       interface export;     module procedure exportGrid;     end interface
       interface addToFile;  module procedure addToFileGrid;  end interface
       ! interface export;     module procedure exportGrid1;     end interface
       
       contains

       subroutine delete_grids(g)
         implicit none
         type(grids),intent(inout) :: gs
         integer :: i
         do i = 1,3; call delete(gs%g(i)) ;enddo
         ! write(*,*) 'Grid deleted'
       end subroutine

       subroutine initGridCopy(g,f)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: f
         integer :: i
         do i = 1,3; call init(g%c(i),f%c(i)%hn,2) ;enddo
         call initProps(g)
       end subroutine

       subroutine initGrid1(g,h,dir,gridType)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir,gridType
         integer :: i
         call init(g%c(dir),h,gridType)
         if (all((/(allocated(g%c(i)%hn),i=1,3)/))) then
           call initProps(g)
#ifdef _CHECK_GRID_
           call checkGrid(g)
#endif
         endif
       end subroutine

       subroutine initGrid2(g,h1,h2,h3,gridType)
        implicit none
        type(grid),intent(inout) :: g
        real(cp),dimension(:),intent(in) :: h1,h2,h3
        integer,intent(in) :: gridType
        call init(g%c(1),h1,gridType)
        call init(g%c(2),h2,gridType)
        call init(g%c(3),h3,gridType)
        call initProps(g)
       end subroutine

       subroutine initGrid3(g,h1,h2,h3,gridType)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h1,h2,h3
         integer,dimension(3),intent(in) :: gridType
         call init(g%c(1),h1,gridType(1))
         call init(g%c(2),h2,gridType(2))
         call init(g%c(3),h3,gridType(3))
         call initProps(g)
       end subroutine

       subroutine initProps(g)
         implicit none
         type(grid),intent(inout) :: g
         g%dhMin = minval((/g%c(1)%dhMin,g%c(2)%dhMin,g%c(3)%dhMin/))
         g%dhMax = maxval((/g%c(1)%dhMax,g%c(2)%dhMax,g%c(3)%dhMax/))
         g%maxRange = maxval((/g%c(1)%maxRange,g%c(2)%maxRange,g%c(3)%maxRange/))
         g%volume = g%c(1)%maxRange*g%c(2)%maxRange*g%c(3)%maxRange
       end subroutine
        
       ! ------------------- restrict (for multigrid) --------------

       subroutine restrictGrid1(r,g,dir)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         call restrict(r%c(dir),g%c(dir))
       end subroutine

       subroutine restrictGrid3(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         integer :: i
         do i = 1,3; call restrict(r%c(i),g%c(i)) ;enddo
       end subroutine

       ! ---------------------------------------------- check grid

#ifdef _CHECK_GRID_
       subroutine checkGrid(g)
         implicit none
         type(grid),intent(in) :: g
         integer :: i
         do i=1,3; call checkCoordinates(g%c(i)); enddo
       end subroutine
#endif

       subroutine exportGrid(g,dir,name)
         implicit none
         type(grid), intent(in) :: g
         character(len=*),intent(in) :: dir,name
         integer :: newU
         if (exportLight) then
           newU = newAndOpen(dir,'gridXYZ_'//name)
           write(newU,*) 'dhMin = ',g%dhMin
           write(newU,*) 'dhMin = ',g%dhMax
           write(newU,*) 'maxRange = ',g%maxRange
           write(newU,*) 'volume = ',g%volume
           call addToFile(g,newU); close(newU)
           call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,1.0_cp,dir,name//'_n')
         else
           call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,1.0_cp,dir,name//'_n')
           ! call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,1.0_cp,dir,name//'_c')
         endif
       end subroutine

       subroutine printGrid(g)
         implicit none
         type(grid), intent(in) :: g
         integer :: i
         do i = 1,3; call print(g%c(i)) ;enddo
       end subroutine

       subroutine addToFileGrid(g,u)
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: u
         integer :: i
         do i = 1,3; call addToFile(g%c(i),u) ;enddo
       end subroutine

       end module