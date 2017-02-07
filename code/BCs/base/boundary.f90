       module boundary_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use data_location_mod
       use grid_mod
       use block_mod
       use string_mod
       use GF_mod
       use bctype_mod
       use BC_logicals_mod
       use IO_tools_mod
       use table_mod
       implicit none

       private
       public :: boundary
       public :: init,delete,display,print,export,import ! Essentials

       ! Setters for type
       public :: init_Dirichlet
       public :: init_Neumann
       public :: init_Robin
       public :: init_periodic
       public :: init_symmetric
       public :: init_antisymmetric

       public :: get_All_Neumann
       public :: get_all_Dirichlet
       public :: get_All_Robin

       public :: prolongate
       public :: restrict

       type boundary
         integer :: n = 0
         type(grid_field),dimension(:),allocatable :: b      ! B values size = 6
         type(bctype),dimension(:),allocatable :: bct        ! for self-documenting output
         type(string) :: name                                ! Face,edge,corner
         type(BC_logicals) :: BCL                            ! logicals
       end type

       interface init;                module procedure init_GFs_boundary_DL;      end interface
       interface init;                module procedure init_boundary_copy;        end interface

       interface init;                module procedure init_vals_all_S;           end interface
       interface init;                module procedure init_vals_ID_GF;           end interface
       interface init;                module procedure init_val_ID_S;             end interface

       interface delete;              module procedure delete_boundary;           end interface
       interface display;             module procedure display_boundary;          end interface
       interface print;               module procedure print_boundary;            end interface
       interface export;              module procedure export_boundary;           end interface
       interface import;              module procedure import_boundary;           end interface

       interface export;              module procedure export_boundary_wrapper;   end interface
       interface import;              module procedure import_boundary_wrapper;   end interface

       interface init_Dirichlet;      module procedure init_Dirichlet_all;        end interface
       interface init_Dirichlet;      module procedure init_Dirichlet_ID;         end interface
       interface init_Neumann;        module procedure init_Neumann_all;          end interface
       interface init_Neumann;        module procedure init_Neumann_ID;           end interface
       interface init_Robin;          module procedure init_Robin_all;            end interface
       interface init_Robin;          module procedure init_Robin_ID;             end interface
       interface init_periodic;       module procedure init_periodic_all;         end interface
       interface init_periodic;       module procedure init_periodic_ID;          end interface
       interface init_symmetric;      module procedure init_symmetric_all;        end interface
       interface init_symmetric;      module procedure init_symmetric_ID;         end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_all;    end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_ID;     end interface

       interface define_logicals;     module procedure define_logicals_boundary;  end interface
       interface insist_allocated;    module procedure insist_allocated_boundary; end interface

       interface get_All_Neumann;     module procedure get_All_Neumann_B;         end interface
       interface get_all_Dirichlet;   module procedure get_all_Dirichlet_B;       end interface
       interface get_All_Robin;       module procedure get_All_Robin_B;           end interface

       interface prolongate;          module procedure prolongate_B;              end interface
       interface restrict;            module procedure restrict_B;                end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_GFs_boundary_DL(B,BL,DL,n,name)
         implicit none
         type(boundary),intent(inout) :: B
         type(block),intent(in) :: BL
         type(data_location),intent(in) :: DL
         integer,intent(in) :: n
         character(len=*),intent(in) :: name
         integer :: i
         call delete(B)
         allocate(B%b(n))
         allocate(B%bct(n))
         call init(B%name,name)
         B%n = n
         if (n.eq.6) then
               if (is_CC(DL)) then; do i=1,n; call init_CC(  B%b(i),BL%fb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,n; call init_Node(B%b(i),BL%fb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,n; call init_Face(B%b(i),BL%fb(i),get_Face(DL)); enddo
         elseif (is_Edge(DL)) then; do i=1,n; call init_Edge(B%b(i),BL%fb(i),get_Edge(DL)); enddo
         else; stop 'Error: bad DL f in init_GFs_boundary_DL in boundary.f90'
         endif
         elseif (n.eq.12) then
               if (is_CC(DL)) then; do i=1,n; call init_CC(  B%b(i),BL%eb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,n; call init_Node(B%b(i),BL%eb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,n; call init_Face(B%b(i),BL%eb(i),get_Face(DL)); enddo
         elseif (is_Edge(DL)) then; do i=1,n; call init_Edge(B%b(i),BL%eb(i),get_Edge(DL)); enddo
         else; stop 'Error: bad DL e in init_GFs_boundary_DL in boundary.f90'
         endif
         elseif (n.eq.8) then
               if (is_CC(DL)) then; do i=1,n; call init_CC(  B%b(i),BL%cb(i)); enddo
         elseif (is_Node(DL)) then; do i=1,n; call init_Node(B%b(i),BL%cb(i)); enddo
         elseif (is_Face(DL)) then; do i=1,n; call init_Face(B%b(i),BL%cb(i),get_Face(DL)); enddo
         elseif (is_Edge(DL)) then; do i=1,n; call init_Edge(B%b(i),BL%cb(i),get_Edge(DL)); enddo
         else; stop 'Error: bad DL c in init_GFs_boundary_DL in boundary.f90'
         endif
         else; stop 'Error: bad input to init_GFs_boundary_DL in B.f90'
         endif
         call init_vals_all_S(B,0.0_cp)
         B%BCL%GFs_defined = .true.
         call define_logicals(B)
       end subroutine

       subroutine init_boundary_copy(B,B_in)
         implicit none
         type(boundary),intent(inout) :: B
         type(boundary),intent(in) :: B_in
         integer :: i
#ifdef _DEBUG_boundary_
         call insist_allocated(B_in,'init_boundary_copy')
#endif

         call delete(B)
         B%n = B_in%n
         call init(B%name,B_in%name)
         call init(B%BCL,B_in%BCL)
         allocate(B%b(B_in%n))
         allocate(B%bct(B_in%n))
         do i=1,B_in%n;  call init(B%b(i),B_in%b(i)); call assign(B%b(i),B_in%b(i)); enddo
         do i=1,B_in%n;  call init(B%bct(i),B_in%bct(i)); enddo
       end subroutine

       subroutine delete_boundary(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         if (allocated(B%b)) then
           do i=1,size(B%b); call delete(B%b(i)); enddo
           deallocate(B%b)
         endif
         if (allocated(B%bct)) then
           do i=1,size(B%bct); call delete(B%bct(i)); enddo
           deallocate(B%bct)
         endif
         call delete(B%BCL)
         call delete(B%name)
         B%n = 0
       end subroutine

       subroutine init_vals_all_S(B,val)
         implicit none
         type(boundary),intent(inout) :: B
         real(cp),intent(in) :: val
         integer :: i
         do i=1,B%n;  call assign(B%b(i),val); enddo
         B%BCL%vals_defined = .true.
         call define_logicals(B)
       end subroutine

       subroutine init_vals_ID_GF(B,vals,ID)
         implicit none
         type(boundary),intent(inout) :: B
         type(grid_field),intent(in) :: vals
         integer,intent(in) :: ID
         call assign(B%b(ID),vals)
         B%BCL%vals_defined = .true.
         call define_logicals(B)
       end subroutine

       subroutine init_val_ID_S(B,val,ID)
         implicit none
         type(boundary),intent(inout) :: B
         real(cp),intent(in) :: val
         integer,intent(in) :: ID
         call assign(B%b(ID),val)
         B%BCL%vals_defined = .true.
         call define_logicals(B)
       end subroutine

       subroutine display_boundary(B,un)
         implicit none
         type(boundary),intent(in) :: B
         integer,intent(in) :: un
         integer :: i,col_width,precision
         if (B%BCL%defined) then
           precision = 4; col_width = 10
           call export_table('ID         :',(/(i,i=1,B%n)/),col_width,un)
           call export_table('Type       :',(/(get_bctype(B%bct(i)),i=1,B%n)/),col_width,un)
           call export_table('meanVal    :',(/(get_mean_value(B%bct(i)),i=1,B%n)/),col_width,precision,un)
           call export_table('prescribed :',(/(is_prescribed(B%bct(i)),i=1,B%n)/),col_width,un)
         endif
       end subroutine

       subroutine print_boundary(B)
         implicit none
         type(boundary),intent(in) :: B
         call display(B,6)
       end subroutine

       subroutine export_boundary(B,un)
         implicit none
         type(boundary),intent(in) :: B
         integer,intent(in) :: un
         integer :: i
         call insist_allocated(B,'export_boundary')
         write(un,*) 'defined'
         write(un,*) B%BCL%defined
         if (B%BCL%defined) then
           do i=1,B%n;  call export(B%b(i),un);   enddo
           do i=1,B%n;  call export(B%bct(i),un); enddo
           call export(B%BCL,un)
           write(un,*) 'all_Dirichlet,all_Neumann,all_Robin'
           write(un,*) B%BCL%all_Dirichlet,B%BCL%all_Neumann,B%BCL%all_Robin
         endif
       end subroutine

       subroutine import_boundary(B,un)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: un
         integer :: i
         read(un,*)
         read(un,*) B%BCL%defined
         if (B%BCL%defined) then
           do i=1,B%n; call import(B%b(i),un); enddo
           do i=1,B%n; call import(B%bct(i),un); enddo
           call import(B%BCL,un)
           read(un,*)
           read(un,*) B%BCL%all_Dirichlet,B%BCL%all_Neumann,B%BCL%all_Robin
         endif
       end subroutine

       subroutine export_boundary_wrapper(B,dir,name)
         implicit none
         type(boundary),intent(in) :: B
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(B,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_boundary_wrapper(B,dir,name)
         implicit none
         type(boundary),intent(inout) :: B
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(B,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! *******************************************************************************
       ! ********************************* INIT FACES **********************************
       ! *******************************************************************************

       subroutine init_Dirichlet_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_Dirichlet(B,i); enddo
       end subroutine
       subroutine init_Dirichlet_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_Dirichlet(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_Neumann_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_Neumann(B,i); enddo
       end subroutine
       subroutine init_Neumann_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_Neumann(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_Robin_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_Robin(B,i); enddo
       end subroutine
       subroutine init_Robin_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_Robin(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_periodic_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_periodic(B,i); enddo
       end subroutine
       subroutine init_periodic_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_periodic(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_symmetric_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_symmetric(B,i); enddo
       end subroutine
       subroutine init_symmetric_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_symmetric(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       subroutine init_antisymmetric_all(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         do i=1,B%n; call init_antisymmetric(B,i); enddo
       end subroutine
       subroutine init_antisymmetric_ID(B,ID)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: ID
         call init_antisymmetric(B%bct(ID))
         call define_logicals(B)
         B%BCL%BCT_defined = .true.
       end subroutine

       ! *******************************************************************************
       ! ********************************* AUXILIARY ***********************************
       ! *******************************************************************************

       subroutine insist_allocated_boundary(B,caller)
         implicit none
         type(boundary),intent(in) :: B
         character(len=*),intent(in) :: caller
         if (.not.(allocated(B%b).and.(size(B%b).eq.B%n))) then
           write(*,*) 'Error: trying to copy unallocated BCs in '//caller//'in boundary.f90'
           write(*,*) 'size(B%b) = ',size(B%b)
           stop 'Done'
         endif
       end subroutine

       subroutine define_logicals_boundary(B)
         implicit none
         type(boundary),intent(inout) :: B
         integer :: i
         B%BCL%defined = B%BCL%GFs_defined.and.B%BCL%BCT_defined.and.B%BCL%vals_defined
         B%BCL%all_Dirichlet = all((/(is_Dirichlet(B%bct(i)),i=1,B%n)/))
         B%BCL%all_Robin = all((/(is_Robin(B%bct(i)),i=1,B%n)/))
         B%BCL%all_Neumann = all((/(is_Neumann(B%bct(i)),i=1,B%n)/))
       end subroutine

       function get_All_Neumann_B(B) result(L)
         implicit none
         type(boundary),intent(inout) :: B
         logical :: L
         L = B%BCL%all_Neumann
       end function

       function get_all_Dirichlet_B(B) result(L)
         implicit none
         type(boundary),intent(inout) :: B
         logical :: L
         L = B%BCL%all_Dirichlet
       end function

       function get_All_Robin_B(B) result(L)
         implicit none
         type(boundary),intent(inout) :: B
         logical :: L
         L = B%BCL%all_Robin
       end function

       subroutine restrict_B(B,g,DL,dir,x,y,z,n)
         implicit none
         integer,intent(in) :: n,dir,x,y,z
         type(boundary),intent(inout) :: B
         type(grid),dimension(n),intent(in) :: g
         type(data_location),intent(in) :: DL
         integer :: i
         if (CC_along(DL,dir)) then
           do i=1,B%n; call restrict_C(B%b(i),g(i),dir,x,y,z); enddo
         elseif ( N_along(DL,dir)) then
           do i=1,B%n; call restrict_N(B%b(i),g(i),dir,x,y,z); enddo
         else; stop 'Error: bad DL in restrict_B in boundary.f90'
         endif
       end subroutine

       subroutine prolongate_B(B,g,DL,dir,x,y,z,n)
         implicit none
         type(boundary),intent(inout) :: B
         integer,intent(in) :: n,dir,x,y,z
         type(grid),dimension(n),intent(in) :: g
         type(data_location),intent(in) :: DL
         integer :: i
         if (CC_along(DL,dir)) then
           do i=1,B%n; call prolongate_C(B%b(i),g(i),dir,x,y,z); enddo
         elseif ( N_along(DL,dir)) then
           do i=1,B%n; call prolongate_N(B%b(i),dir,x,y,z); enddo
         else; stop 'Error: bad DL in prolongate_B in boundary.f90'
         endif
       end subroutine

       end module