       module grid_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
       use current_precision_mod
       use IO_tools_mod
       use coordinates_mod
       use stitch_mod
       use stitch_face_mod
       use stitch_edge_mod
       use stitch_corner_mod
       implicit none

       private
       public :: grid
       public :: init,delete,display,print,export,import ! Essentials
       public :: restrict,restrict_x,restrict_xy
       ! public :: get_face,get_edge,get_corner
       public :: initProps,display_stitches
       public :: snip,pop

#ifdef _DEBUG_COORDINATES_
       public :: checkGrid
#endif

       type grid
         type(coordinates),dimension(3) :: c ! hn,hc,dhn,dhc / dhMin,maxRange
         type(stitch),dimension(6)   :: st_faces   ! Face-stitches
         type(stitch),dimension(12)   :: st_edges   ! Edge-stitch
         type(stitch),dimension(8)   :: st_corners   ! Corner-stitch
         ! Properties
         real(cp) :: dhMin,dhMax,maxRange,volume
         integer :: N_cells
         logical :: defined
       end type

       interface init;               module procedure initGridCopy;            end interface
       interface init;               module procedure initGrid1;               end interface
       interface init;               module procedure initGrid2;               end interface
       interface delete;             module procedure deleteGrid;              end interface
       interface display;            module procedure display_grid;            end interface
       interface print;              module procedure print_Grid;              end interface
       interface export;             module procedure export_Grid;             end interface
       interface import;             module procedure import_Grid;             end interface

       interface initProps;          module procedure initProps_grid;          end interface
       interface display_stitches;   module procedure display_grid_stitches;   end interface
       interface restrict;           module procedure restrictGrid1;           end interface
       interface restrict;           module procedure restrictGrid3;           end interface
       interface restrict_x;         module procedure restrictGrid_x;          end interface
       interface restrict_xy;        module procedure restrictGrid_xy;         end interface

       interface snip;               module procedure snip_grid;               end interface
       interface pop;                module procedure pop_grid;                end interface


       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine initGridCopy(g,f)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: f
         integer :: i
         do i=1,3;  call init(g%c(i),f%c(i));                   enddo
         do i=1,6;  call init(g%st_faces(i),f%st_faces(i));     enddo
         do i=1,12; call init(g%st_edges(i),f%st_edges(i));     enddo
         do i=1,8;  call init(g%st_corners(i),f%st_corners(i)); enddo
         call initProps(g)
         if (.not.g%defined) stop 'Error: tried copying grid that was not fully defined'
       end subroutine

       subroutine initGrid1(g,h,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         integer :: i
         call init(g%c(dir),h,size(h))
         if (all((/(allocated(g%c(i)%hn),i=1,3)/))) then
           call initProps(g)
#ifdef _DEBUG_COORDINATES_
           call checkGrid(g)
#endif
         endif
       end subroutine

       subroutine initGrid2(g,h1,h2,h3)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h1,h2,h3
         call init(g%c(1),h1,size(h1))
         call init(g%c(2),h2,size(h2))
         call init(g%c(3),h3,size(h3))
         call initProps(g)
       end subroutine

       subroutine deleteGrid(g)
         implicit none
         type(grid),intent(inout) :: g
         integer :: i
         do i=1,3;  call delete(g%c(i));          enddo
         do i=1,6;  call delete(g%st_faces(i));   enddo
         do i=1,12; call delete(g%st_edges(i));   enddo
         do i=1,8;  call delete(g%st_corners(i)); enddo
         call initProps(g)
         ! write(*,*) 'Grid deleted'
       end subroutine

       subroutine display_grid(g,un)
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: un
         write(un,*) 'volume,defined = ',g%volume,g%defined
         write(un,*) 'min/max(h)_x = ',(/g%c(1)%hmin,g%c(1)%hmax/)
         write(un,*) 'min/max(h)_y = ',(/g%c(2)%hmin,g%c(2)%hmax/)
         write(un,*) 'min/max(h)_z = ',(/g%c(3)%hmin,g%c(3)%hmax/)
         write(un,*) 'min/max(dh)_x = ',(/g%c(1)%dhMin,g%c(1)%dhMax/)
         write(un,*) 'min/max(dh)_y = ',(/g%c(2)%dhMin,g%c(2)%dhMax/)
         write(un,*) 'min/max(dh)_z = ',(/g%c(3)%dhMin,g%c(3)%dhMax/)
         write(un,*) 'N_cells_x,stretching_x = ',g%c(1)%N,g%c(1)%dhMax-g%c(1)%dhMin
         write(un,*) 'N_cells_y,stretching_y = ',g%c(2)%N,g%c(2)%dhMax-g%c(2)%dhMin
         write(un,*) 'N_cells_z,stretching_z = ',g%c(3)%N,g%c(3)%dhMax-g%c(3)%dhMin
         ! call display_grid_stitches(g,un)
       end subroutine

       subroutine print_Grid(g)
         implicit none
         type(grid), intent(in) :: g
         call display(g,6)
       end subroutine

       subroutine export_grid(g,un) ! Import / Export must mirror
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'volume,defined = ';         write(un,*) g%volume,g%defined
         write(un,*) 'min/max(h)_x = ';           write(un,*) (/g%c(1)%hmin,g%c(1)%hmax/)
         write(un,*) 'min/max(h)_y = ';           write(un,*) (/g%c(2)%hmin,g%c(2)%hmax/)
         write(un,*) 'min/max(h)_z = ';           write(un,*) (/g%c(3)%hmin,g%c(3)%hmax/)
         write(un,*) 'min/max(dh)_x = ';          write(un,*) (/g%c(1)%dhMin,g%c(1)%dhMax/)
         write(un,*) 'min/max(dh)_y = ';          write(un,*) (/g%c(2)%dhMin,g%c(2)%dhMax/)
         write(un,*) 'min/max(dh)_z = ';          write(un,*) (/g%c(3)%dhMin,g%c(3)%dhMax/)
         write(un,*) 'N_cells_x = '; write(un,*) g%c(1)%N
         write(un,*) 'N_cells_y = '; write(un,*) g%c(2)%N
         write(un,*) 'N_cells_z = '; write(un,*) g%c(3)%N
         do i = 1,3;  call export(g%c(i),un);          enddo
         do i = 1,6;  call export(g%st_faces(i),un);   enddo
         do i = 1,12; call export(g%st_edges(i),un);   enddo
         do i = 1,8;  call export(g%st_corners(i),un); enddo
       end subroutine

       subroutine import_grid(g,un) ! Import / Export must mirror
         implicit none
         type(grid), intent(inout) :: g
         integer,intent(in) :: un
         integer :: i
         read(un,*); read(un,*) g%volume,g%defined
         read(un,*); read(un,*) g%c(1)%hmin,g%c(1)%hmax
         read(un,*); read(un,*) g%c(2)%hmin,g%c(2)%hmax
         read(un,*); read(un,*) g%c(3)%hmin,g%c(3)%hmax
         read(un,*); read(un,*) g%c(1)%dhMin,g%c(1)%dhMax
         read(un,*); read(un,*) g%c(2)%dhMin,g%c(2)%dhMax
         read(un,*); read(un,*) g%c(3)%dhMin,g%c(3)%dhMax
         read(un,*); read(un,*) g%c(1)%N
         read(un,*); read(un,*) g%c(2)%N
         read(un,*); read(un,*) g%c(3)%N
         do i = 1,3;  call import(g%c(i),un);          enddo
         do i = 1,6;  call import(g%st_faces(i),un);   enddo
         do i = 1,12; call import(g%st_edges(i),un);   enddo
         do i = 1,8;  call import(g%st_corners(i),un); enddo
         call initProps(g)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine initProps_grid(g)
         implicit none
         type(grid),intent(inout) :: g
         integer :: i
         g%dhMin = minval((/g%c(1)%dhMin,g%c(2)%dhMin,g%c(3)%dhMin/))
         g%dhMax = maxval((/g%c(1)%dhMax,g%c(2)%dhMax,g%c(3)%dhMax/))
         g%maxRange = maxval((/g%c(1)%maxRange,g%c(2)%maxRange,g%c(3)%maxRange/))
         g%volume = g%c(1)%maxRange*g%c(2)%maxRange*g%c(3)%maxRange
         g%N_cells = g%c(1)%N + g%c(2)%N + g%c(3)%N
         g%defined = all((/(g%c(i)%defined,i=1,3)/))
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

       subroutine restrictGrid_x(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         call restrict(r%c(1),g%c(1))
       end subroutine

       subroutine restrictGrid_xy(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         call restrict(r%c(1),g%c(1))
         call restrict(r%c(2),g%c(2))
       end subroutine

       ! ------------------- restrict (for multigrid) --------------

       ! subroutine get_face(g,g_in,face)
       !   ! Removes all cells except for prescribed face
       !   implicit none
       !   type(grid),intent(inout) :: g
       !   type(grid),intent(in) :: g_in
       !   integer,intent(in) :: face
       !   integer :: i,dir,s
       !   call init(g,g_in)
       !   select case (face)
       !   case (1,2); dir = 1
       !   case (3,4); dir = 2
       !   case (5,6); dir = 3
       !   case default; stop 'Error: face (1) must = 1:6 in get_face in grid.f90'
       !   end select
       !   ! The number of times that snip / pop are called
       !   ! must be chosen carefully, this is just a test...
       !   s = g%c(dir)%N
       !   N = c%sc-2
       !   select case (face)
       !   case (1,3,5); do i=1,s; call snip(g,dir); enddo
       !   case (2,4,6); do i=1,s; call  pop(g,dir); enddo
       !   case default; stop 'Error: face (2) must = 1:6 in get_face in grid.f90'
       !   end select
       ! end subroutine

       subroutine pop_grid(g,dir) ! Removes the last index from the grid
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call pop(g%c(dir))
       end subroutine

       subroutine snip_grid(g,dir) ! Removes the first index from the grid
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call snip(g%c(dir))
       end subroutine

       ! ---------------------------------------------- check grid

#ifdef _DEBUG_COORDINATES_
       subroutine checkGrid(g)
         implicit none
         type(grid),intent(in) :: g
         integer :: i
         do i=1,3; call checkCoordinates(g%c(i)); enddo
       end subroutine
#endif

       subroutine display_grid_stitches(g,un)
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'st_faces = ',(/(g%st_faces(i)%TF,i=1,6)/)
         write(un,*) 'st_edges = ',(/(g%st_edges(i)%TF,i=1,12)/)
         write(un,*) 'st_corners = ',(/(g%st_edges(i)%TF,i=1,8)/)
         
         ! do i=1,6
         !   if (g%st_faces(i)%TF) write(un,*) 'st_face_id (i,ID)=',i,g%st_faces(i)%ID
         ! enddo

         do i=1,12
           if (g%st_edges(i)%TF) write(un,*) 'st_edge_id (i,ID)=',i,g%st_edges(i)%ID
         enddo

         ! write(un,*) 'stitches_edge (minmin) = ',(/(g%st_edge%minmin(i),i=1,3)/)
         ! write(un,*) 'stitches_edge (minmax) = ',(/(g%st_edge%minmax(i),i=1,3)/)
         ! write(un,*) 'stitches_edge (maxmin) = ',(/(g%st_edge%maxmin(i),i=1,3)/)
         ! write(un,*) 'stitches_edge (maxmax) = ',(/(g%st_edge%maxmax(i),i=1,3)/)

         ! write(un,*) 'stitches_corner (minmin) = ',g%st_corner%minmin
         ! write(un,*) 'stitches_corner (minmax) = ',(/(g%st_corner%minmax(i),i=1,3)/)
         ! write(un,*) 'stitches_corner (maxmin) = ',(/(g%st_corner%maxmin(i),i=1,3)/)
         ! write(un,*) 'stitches_corner (maxmax) = ',g%st_corner%maxmax
       end subroutine

       end module