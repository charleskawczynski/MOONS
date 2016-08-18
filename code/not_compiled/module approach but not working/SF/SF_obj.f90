      module SF_mod
      use IO_tools_mod
      use mesh_mod
      use BCs_mod
      use RF_mod
      implicit none
      private

      ! Initialization / Deletion (allocate/deallocate)
      public :: SF
      public :: init,delete
      ! Grid initialization
      public :: init_CC
      public :: init_Node
      public :: init_Face
      public :: init_Edge
      public :: CC_along,Node_along

      public :: init_BCs,init_BC_props

      ! Monitoring
      public :: print
      public :: print_BCs
      public :: export_BCs

      ! Auxiliary
      public :: get_3D_index
      public :: get_1D_index

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      type SF
        integer :: s ! Number of subdomains in domain decomposition
        type(realField),dimension(:),allocatable :: RF
        logical :: is_CC,is_Node,is_face,is_edge
        logical :: all_neumann ! If necessary to subtract mean
        integer :: face = 0 ! Direction of face data
        integer :: edge = 0 ! Direction of edge data
        integer :: numEl
      end type

      interface init;             module procedure init_SF_copy;           end interface
      interface init_CC;          module procedure init_SF_CC;             end interface
      interface init_Node;        module procedure init_SF_Node;           end interface
      interface init_Face;        module procedure init_SF_Face;           end interface
      interface init_Edge;        module procedure init_SF_Edge;           end interface

      interface init_BCs;         module procedure init_BCs_SF;            end interface
      interface init_BC_props;    module procedure init_BC_props_SF;       end interface

      interface delete;           module procedure delete_SF;              end interface

      interface print;            module procedure print_SF;               end interface
      interface print_BCs;        module procedure print_BCs_SF;           end interface
      interface export_BCs;       module procedure export_BCs_SF;          end interface

      interface CC_along;         module procedure CC_along_SF;            end interface
      interface Node_along;       module procedure Node_along_SF;          end interface

      contains

      subroutine init_SF_copy(f1,f2)
        implicit none
        type(SF),intent(inout) :: f1
        type(SF),intent(in) :: f2
        integer :: i
        call delete(f1)
        allocate(f1%RF(f2%s)); f1%s = f2%s
        do i=1,f1%s; call init(f1%RF(i),f2%RF(i)); enddo
        f1%numEl = f2%numEl
        f1%is_CC = f2%is_CC
        f1%is_node = f2%is_node
        f1%is_face = f2%is_face
        f1%is_edge = f2%is_edge

        f1%face = f2%face
        f1%edge = f2%edge
      end subroutine

      subroutine init_SF_CC(f,m)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        integer :: i
        call delete(f)
        allocate(f%RF(m%s)); f%s = m%s
        do i=1,f%s; call init_CC(f%RF(i),m%g(i)); enddo
        call deleteDataLocation(f)
        call computeNumEl(f)
        f%is_CC = .true.
      end subroutine

      subroutine init_SF_Face(f,m,dir)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir
        integer :: i
        call delete(f)
        allocate(f%RF(m%s)); f%s = m%s
        do i=1,f%s; call init_Face(f%RF(i),m%g(i),dir); enddo
        call deleteDataLocation(f)
        call computeNumEl(f)
        f%is_face = .true.
        f%face = dir
      end subroutine

      subroutine init_SF_Edge(f,m,dir)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir
        integer :: i
        allocate(f%RF(m%s)); f%s = m%s
        do i=1,f%s; call init_Edge(f%RF(i),m%g(i),dir); enddo
        call deleteDataLocation(f)
        call computeNumEl(f)
        f%is_edge = .true.
        f%edge = dir
      end subroutine

      subroutine init_SF_Node(f,m)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        integer :: i
        allocate(f%RF(m%s)); f%s = m%s
        do i=1,f%s; call init_Node(f%RF(i),m%g(i)); enddo
        call deleteDataLocation(f)
        call computeNumEl(f)
        f%is_node = .true.
      end subroutine

      function get_1D_index(i,j,k,t,U) result(m)
        ! Notes:
        !     For i=1,j=1,k=1 we have
        !     m = 1 + im*(0 + 0) = 1
        !     For i=im,j=jm,k=km we have
        !     m = im + im*((jm-1) + jm*(km-1))
        !       = im + im*jm - im + im*jm*(km-1)
        !       =      im*jm      + im*jm*km-im*jm
        !       =                 + im*jm*km
        !     Which should equal
        !     m = im*jm*km
        implicit none
        type(SF),intent(in) :: U
        integer,intent(in) :: i,j,k,t
        integer :: im,jm,km
        real(cp) :: m
        if (U%s.gt.1) stop 'Error: get_1D_index not developed for U%s>1 in SF.f90'
        if (t.gt.1) stop 'Error: get_1D_index not developed for t>1 in SF.f90'
        im = U%RF(1)%s(1)
        jm = U%RF(1)%s(2)
        km = U%RF(1)%s(3)
        m = i + im*( (j-1) + jm*(k-1) )
      end function

      subroutine get_3D_index(i_3D,j_3D,k_3D,t_3D,U,index_1D)
        implicit none
        integer,intent(inout) :: i_3D,j_3D,k_3D,t_3D
        type(SF),intent(in) :: U
        integer,intent(in) :: index_1D
        integer :: im,jm,km
        if (U%s.gt.1) stop 'Error: get_1D_index not developed for U%s>1 in SF.f90'
        im = U%RF(1)%s(1)
        jm = U%RF(1)%s(2)
        km = U%RF(1)%s(3)
        t_3D = 1
        k_3D = (index_1D-1)/(im*jm)+1
        j_3D = ((index_1D-1) - ((k_3D-1)*im*jm))/im+1
        i_3D = index_1D - (j_3D-1)*im - (k_3D-1)*im*jm
      end subroutine

    ! ------------------- ALLOCATE / DEALLOCATE --------------------

      subroutine deleteDataLocation(a)
        implicit none
        type(SF),intent(inout) :: a
        a%is_CC = .false.
        a%is_node = .false.
        a%is_face = .false.
        a%is_edge = .false.
      end subroutine

      subroutine computeNumEl(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        f%numEl = 0
        do i=1,f%s
        f%numEl = f%numEl + f%RF(i)%s(1)*f%RF(i)%s(2)*f%RF(i)%s(3)
        enddo
      end subroutine

      function CC_along_SF(f,dir) result(TF)
        implicit none
        type(SF),intent(in) :: f
        integer,intent(in) :: dir
        logical :: TF
        TF = any((/f%is_CC,f%is_Face.and.(f%face.ne.dir),f%is_Edge.and.(f%edge.eq.dir)/))
      end function

      function Node_along_SF(f,dir) result(TF)
        implicit none
        type(SF),intent(in) :: f
        integer,intent(in) :: dir
        logical :: TF
        TF = any((/f%is_Node,f%is_Face.and.(f%face.eq.dir),f%is_Edge.and.(f%edge.ne.dir)/))
      end function

      subroutine init_BCs_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        if (f%is_CC) then
          do i=1,f%s; call init_BCs(f%RF(i),.true.,.false.); enddo
        elseif (f%is_Node) then
          do i=1,f%s; call init_BCs(f%RF(i),.false.,.true.); enddo
        else
          stop 'Error: no datatype found in init_BCs_SF in SF.f90'
        endif
        f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
        call init_BC_props(f)
      end subroutine

      subroutine init_BC_props_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
      end subroutine

      subroutine delete_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        if (allocated(f%RF)) then
          do i=1,f%s; call delete(f%RF(i)); enddo
          deallocate(f%RF)
        endif
        f%s = 0
        f%numEl = 0
      end subroutine

      subroutine print_SF(f)
        implicit none
        type(SF),intent(in) :: f
        integer :: i
        do i=1,f%s; call print(f%RF(i)); enddo
      end subroutine

      subroutine print_BCs_SF(f,name)
        implicit none
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: name
        call exp_BCs_SF(f,name,6)
      end subroutine

      subroutine export_BCs_SF(f,dir,name)
        implicit none
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: dir,name
        integer :: un
        un = newAndOpen(dir,name//'_BoundaryConditions')
        call exp_BCs_SF(f,name,un)
        call closeAndMessage(un,dir,name//'_BoundaryConditions')
      end subroutine

      subroutine exp_BCs_SF(f,name,un)
        implicit none
        type(SF),intent(in) :: f
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        integer :: i
        write(un,*) ' ------ BCs for ' // name // ' ------ '
        do i=1,f%s
          call export(f%RF(i)%b,un)
        enddo
        write(un,*) ' ------------------------------------ '
      end subroutine

      end module