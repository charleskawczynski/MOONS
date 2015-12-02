      module VF_obj_mod
      use mesh_mod
      use SF_mod
      implicit none
      private

      ! Initialization / Deletion (allocate/deallocate)
      public :: VF
      public :: init,delete
      ! Grid initialization
      public :: init_CC
      public :: init_Face
      public :: init_Edge
      public :: init_Node

      ! Monitoring
      public :: print
      public :: print_BCs
      public :: export_BCs


#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      type VF
        integer :: s = 3  ! number of components
        type(SF) :: x,y,z ! components
        logical :: is_Face,is_Edge,is_Node,is_CC
      end type

      interface init;              module procedure init_VF_copy_VF;          end interface
      interface init;              module procedure init_VF_copy_SF;          end interface

      interface init_CC;           module procedure init_VF_CC;               end interface
      interface init_Face;         module procedure init_VF_Face;             end interface
      interface init_Edge;         module procedure init_VF_Edge;             end interface
      interface init_Node;         module procedure init_VF_Node;             end interface

      interface delete;            module procedure delete_VF;                end interface
      interface print;             module procedure print_VF;                 end interface
      interface print_BCs;         module procedure print_BCs_VF;             end interface
      interface export_BCs;        module procedure export_BCs_VF;            end interface


      contains

    ! ------------------- INIT COPY --------------------

      subroutine init_VF_copy_VF(f1,f2)
        implicit none
        type(VF),intent(inout) :: f1
        type(VF),intent(in) :: f2
        call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
        f1%is_Face = f2%is_Face; f1%is_Edge = f2%is_Edge
        f1%is_Node = f2%is_Node; f1%is_CC = f2%is_CC
      end subroutine

      subroutine init_VF_copy_SF(f1,f2)
        implicit none
        type(VF),intent(inout) :: f1
        type(SF),intent(in) :: f2
        call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
        f1%is_Face = f2%is_Face; f1%is_Edge = f2%is_Edge
        f1%is_Node = f2%is_Node; f1%is_CC = f2%is_CC
      end subroutine

    ! ------------------- INIT BY LOCATION --------------------

      subroutine init_VF_CC(f,m)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
        call delete_locations(f); f%is_CC = .true.
      end subroutine

      subroutine init_VF_Edge(f,m)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        call init_Edge(f%x,m,1); call init_Edge(f%y,m,2); call init_Edge(f%z,m,3)
        call delete_locations(f); f%is_Edge = .true.
      end subroutine

      subroutine init_VF_Face(f,m)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        call init_Face(f%x,m,1); call init_Face(f%y,m,2); call init_Face(f%z,m,3)
        call delete_locations(f); f%is_Face = .true.
      end subroutine

      subroutine init_VF_Node(f,m)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
        call delete_locations(f); f%is_Node = .true.
      end subroutine

    ! ------------------- DELETE --------------------

      subroutine delete_locations(f)
        implicit none
        type(VF),intent(inout) :: f
        f%is_Face = .false.; f%is_Edge = .false.
        f%is_Node = .false.; f%is_CC = .false.
      end subroutine

      subroutine delete_VF(f)
        implicit none
        type(VF),intent(inout) :: f
        call delete(f%x); call delete(f%y); call delete(f%z)
        call delete_locations(f)
      end subroutine

    ! ------------------- PRINT / EXPORT --------------------

      subroutine print_VF(f)
        implicit none
        type(VF),intent(in) :: f
        call print(f%x); call print(f%y); call print(f%z)
      end subroutine

      subroutine print_BCs_VF(f,name)
        implicit none
        type(VF),intent(in) :: f
        character(len=*),intent(in) :: name
        call print_BCs(f%x,name//'_x')
        call print_BCs(f%y,name//'_y')
        call print_BCs(f%z,name//'_z')
      end subroutine

      subroutine export_BCs_VF(f,dir,name)
        implicit none
        type(VF),intent(in) :: f
        character(len=*),intent(in) :: dir,name
        call export_BCs(f%x,dir,name//'_x')
        call export_BCs(f%y,dir,name//'_y')
        call export_BCs(f%z,dir,name//'_z')
      end subroutine

      end module