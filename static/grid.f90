       module grid_mod
       use myIO_mod
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

       public :: grid
       public :: init,delete
       public :: print,export
       public :: restrict

       type grid
         type(coordinates),dimension(3) :: c ! hn,hc,dhn,dhc
       end type

       interface init;       module procedure initGridCopy;   end interface
       interface init;       module procedure initGrid1;      end interface
       interface init;       module procedure initGrid2;      end interface
       interface init;       module procedure initGrid3;      end interface
       interface delete;     module procedure deleteGrid;     end interface

       interface restrict;   module procedure restrictGrid1;  end interface
       interface restrict;   module procedure restrictGrid3;  end interface

       interface print;      module procedure printGrid;      end interface
       interface export;     module procedure exportGrid;     end interface
       interface addToFile;  module procedure addToFileGrid;  end interface
       ! interface export;     module procedure exportGrid1;     end interface
       
       contains

       subroutine deleteGrid(g)
         implicit none
         type(grid),intent(inout) :: g
         integer :: i
         do i = 1,3; call delete(g%c(i)) ;enddo
         write(*,*) 'Grid deleted'
       end subroutine

       subroutine initGridCopy(g,f)
        implicit none
        type(grid),intent(inout) :: g
        type(grid),intent(in) :: f
        call init(g%c(1),f%c(1)%hn,2)
        call init(g%c(2),f%c(2)%hn,2)
        call init(g%c(3),f%c(3)%hn,2)
       end subroutine

       subroutine initGrid1(g,h,dir,gridType)
        implicit none
        type(grid),intent(inout) :: g
        real(cp),dimension(:),intent(in) :: h
        integer,intent(in) :: dir,gridType
        call init(g%c(dir),h,gridType)
       end subroutine

       subroutine initGrid2(g,h1,h2,h3,gridType)
        implicit none
        type(grid),intent(inout) :: g
        real(cp),dimension(:),intent(in) :: h1,h2,h3
        integer,intent(in) :: gridType
        call init(g%c(1),h1,gridType)
        call init(g%c(2),h2,gridType)
        call init(g%c(3),h3,gridType)
       end subroutine

       subroutine initGrid3(g,h1,h2,h3,gridType)
        implicit none
        type(grid),intent(inout) :: g
        real(cp),dimension(:),intent(in) :: h1,h2,h3
        integer,dimension(3),intent(in) :: gridType
        call init(g%c(1),h1,gridType(1))
        call init(g%c(2),h2,gridType(2))
        call init(g%c(3),h3,gridType(3))
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
        call restrict(r%c(1),g%c(1))
        call restrict(r%c(2),g%c(2))
        call restrict(r%c(3),g%c(3))
       end subroutine

       ! ----------------------------------------------

       subroutine exportGrid(g,dir,name)
         implicit none
         type(grid), intent(in) :: g
         character(len=*),intent(in) :: dir,name
         integer :: newU
         newU = newAndOpen(dir,'gridXYZ_'//name)
         call addToFile(g,newU)
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,real(1.0,cp),dir,name//'_n')
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,real(1.0,cp),dir,name//'_c')
         close(newU)
       end subroutine

       subroutine printGrid(g)
         implicit none
         type(grid), intent(in) :: g
         call print(g%c(1))
         call print(g%c(2))
         call print(g%c(3))
       end subroutine

       subroutine addToFileGrid(g,u)
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: u
         call addToFile(g%c(1),u)
         call addToFile(g%c(2),u)
         call addToFile(g%c(3),u)
       end subroutine

       end module