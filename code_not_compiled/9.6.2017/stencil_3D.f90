      module stencil_3D_mod
      use stencil_1D_mod
      use data_location_mod
      use grid_mod
      use GF_mod
      use face_edge_corner_indexing_mod
      use current_precision_mod
      implicit none
      private
      public :: stencil_3D
      public :: init,delete,display,print,export,import

      public :: clean
      public :: assign_consecutive
      public :: add_diagonals

      public :: assign_mixed

      public :: add_to_diag
      public :: multiply_diag
      public :: add
      public :: multiply

      public :: insist_allocated

      type stencil_3D
        type(stencil_1D),dimension(3) :: S
        type(grid_field) :: D_3D
        type(grid_field) :: U1_U2
        type(grid_field) :: D1_U2
        type(grid_field) :: U1_D2
        type(grid_field) :: D1_D2
      end type

      interface init;               module procedure init_S3D;                     end interface
      interface init;               module procedure init_Copy_S3D;                end interface
      interface delete;             module procedure delete_S3D;                   end interface
      interface print;              module procedure print_S3D;                    end interface
      interface display;            module procedure display_S3D;                  end interface
      interface import;             module procedure import_S3D;                   end interface
      interface export;             module procedure export_S3D;                   end interface

      interface clean;              module procedure clean_S3D;                    end interface

      interface assign_mixed;       module procedure assign_mixed_S3D;             end interface
      interface assign_mixed;       module procedure assign_mixed_S3D_VP;          end interface

      interface assign_consecutive; module procedure assign_consecutive_S3D_dir;   end interface
      interface assign_consecutive; module procedure assign_consecutive_S3D;       end interface
      interface assign_consecutive; module procedure assign_consecutive_S3D_dir_VP;end interface
      interface assign_consecutive; module procedure assign_consecutive_S3D_VP;    end interface
      interface add_diagonals;      module procedure add_diagonals_S3D;            end interface

      interface add_to_diag;        module procedure add_to_diag_S3D;              end interface
      interface multiply_diag;      module procedure multiply_diag_S3D;            end interface

      interface add;                module procedure add_S3D;                      end interface
      interface multiply;           module procedure multiply_S3D;                 end interface


      interface insist_allocated;   module procedure insist_allocated_S3D;         end interface

      contains

      subroutine init_S3D(ST,g,DL)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3; call init(ST%S(i),g,DL,i); enddo
        call init(ST%D_3D,g,DL)
        call init(ST%U1_U2,g,DL)
        call init(ST%D1_U2,g,DL)
        call init(ST%U1_D2,g,DL)
        call init(ST%D1_D2,g,DL)
      end subroutine

      subroutine init_Copy_S3D(ST,ST_in)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        type(stencil_3D),intent(in) :: ST_in
        integer :: i
        call delete(ST)
        do i=1,3
          call init(ST%S(i),ST_in%S(i))
        enddo
        call init(ST%D_3D,ST_in%D_3D)
        call init(ST%U1_U2,ST_in%U1_U2)
        call init(ST%D1_U2,ST_in%D1_U2)
        call init(ST%U1_D2,ST_in%U1_D2)
        call init(ST%D1_D2,ST_in%D1_D2)
      end subroutine

      subroutine delete_S3D(ST)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer :: i
        do i=1,3; call delete(ST%S(i)); enddo
        call delete(ST%D_3D)
        call delete(ST%U1_U2)
        call delete(ST%D1_U2)
        call delete(ST%U1_D2)
        call delete(ST%D1_D2)
      end subroutine

      subroutine print_S3D(ST)
        implicit none
        type(stencil_3D),intent(in) :: ST
        call display(ST,6)
      end subroutine

      subroutine display_S3D(ST,un)
        implicit none
        type(stencil_3D),intent(in) :: ST
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call display(ST%S(i),un); enddo
        call display(ST%D_3D,un)
        call display(ST%U1_U2,un)
        call display(ST%D1_U2,un)
        call display(ST%U1_D2,un)
        call display(ST%D1_D2,un)
      end subroutine

      subroutine export_S3D(ST,un)
        implicit none
        type(stencil_3D),intent(in) :: ST
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call export(ST%S(i),un); enddo
        call export(ST%D_3D,un)
        call export(ST%U1_U2,un)
        call export(ST%D1_U2,un)
        call export(ST%U1_D2,un)
        call export(ST%D1_D2,un)
      end subroutine

      subroutine import_S3D(ST,un)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call import(ST%S(i),un); enddo
        call import(ST%D_3D,un)
        call import(ST%U1_U2,un)
        call import(ST%D1_U2,un)
        call import(ST%U1_D2,un)
        call import(ST%D1_D2,un)
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine clean_S3D(ST)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer :: i
        do i=1,3; call clean(ST%S(i)); enddo
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine add_diagonals_S3D(ST)
        implicit none
        type(stencil_3D),intent(inout) :: ST
#ifdef _DEBUG_STENCIL_3D_
        call insist_allocated(ST,'add_diagonals_S3D')
#endif
        call assign(ST%D_3D,ST%S(1)%SF%D)
        call    add(ST%D_3D,ST%S(2)%SF%D)
        call    add(ST%D_3D,ST%S(3)%SF%D)
      end subroutine

      subroutine assign_consecutive_S3D(ST)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer :: i
        do i=1,3; call assign_consecutive(ST%S(i)); enddo
      end subroutine

      subroutine assign_consecutive_S3D_dir(ST,dir)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer,intent(in) :: dir
        call assign_consecutive(ST%S(dir))
      end subroutine

      subroutine assign_consecutive_S3D_VP(ST,sig)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        type(grid_field),dimension(3),intent(in) :: sig
        integer :: i
        do i=1,3; call assign_consecutive(ST%S(i),sig); enddo
      end subroutine

      subroutine assign_consecutive_S3D_dir_VP(ST,dir,sig)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        type(grid_field),dimension(3),intent(in) :: sig
        integer,intent(in) :: dir
        call assign_consecutive(ST%S(dir),sig)
      end subroutine

      subroutine assign_mixed_S3D(ST,dir) ! verified for non-uniform mesh on 11/8/2016
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer,dimension(2),intent(in) :: dir
        integer :: i,d
        real(cp) :: D1,D2,U1,U2
        integer,dimension(3) :: s
#ifdef _DEBUG_STENCIL_3D_
        call insist_allocated(ST,'assign_mixed_S3D')
#endif
        s = ST%D_3D%s
        d = dir(1)
        do i=1,s(d)-1
          D1 = ST%S(d)%stag_CC2N%D%f(i)
          call assign_plane(ST%D1_U2,D1,i+1,d)
          call assign_plane(ST%D1_D2,D1,i+1,d)
        enddo
        do i=1,s(d)-1
          U1 = ST%S(d)%stag_CC2N%U%f(i)
          call assign_plane(ST%U1_U2,U1,i+1,d)
          call assign_plane(ST%U1_D2,U1,i+1,d)
        enddo
        d = dir(2)
        do i=1,s(d)-1
          D2 = ST%S(d)%stag_N2CC%D%f(i)
          call multiply_plane(ST%D1_D2,D2,i,d)
          call multiply_plane(ST%U1_D2,D2,i,d)
        enddo
        do i=1,s(d)-1
          U2 = ST%S(d)%stag_N2CC%U%f(i)
          call multiply_plane(ST%D1_U2,U2,i,d)
          call multiply_plane(ST%U1_U2,U2,i,d)
        enddo
      end subroutine

      subroutine assign_mixed_S3D_VP(ST,dir,sig)
        ! Verified on 11/10/2016 to vary within machine accuracy of mixed terms in curl-curl operator
        implicit none
        type(stencil_3D),intent(inout) :: ST
        integer,dimension(2),intent(in) :: dir
        type(grid_field),dimension(3),intent(in) :: sig
        integer :: i,d,d_sig,N
        real(cp) :: D1,D2,U1,U2
        integer,dimension(3) :: s
#ifdef _DEBUG_STENCIL_3D_
        call insist_allocated(ST,'assign_mixed_S3D_VP')
#endif
        s = ST%D_3D%s
        d = dir(1); N = ST%S(d)%stag_CC2N%D%N
        do i=1,N
          D1 = ST%S(d)%stag_CC2N%D%f(i)
          call assign_plane(ST%D1_D2,D1,i+1,d)
          call assign_plane(ST%D1_U2,D1,i+1,d)
        enddo
        do i=1,N
          U1 = ST%S(d)%stag_CC2N%U%f(i)
          call assign_plane(ST%U1_D2,U1,i+1,d)
          call assign_plane(ST%U1_U2,U1,i+1,d)
        enddo
        d = dir(2); N = ST%S(d)%stag_N2CC%D%N
        do i=1,N
          D2 = ST%S(d)%stag_N2CC%D%f(i)
          call multiply_plane(ST%D1_D2,D2,i,d)
          call multiply_plane(ST%U1_D2,D2,i,d)
        enddo
        do i=1,N
          U2 = ST%S(d)%stag_N2CC%U%f(i)
          call multiply_plane(ST%D1_U2,U2,i,d)
          call multiply_plane(ST%U1_U2,U2,i,d)
        enddo
        d_sig = orth_dir(dir)
        d = dir(2)
        do i=2,s(d)-1
          call multiply_plane(ST%D1_D2,sig(d_sig),i,i,d)
          call multiply_plane(ST%U1_D2,sig(d_sig),i,i,d)
          call multiply_plane(ST%D1_U2,sig(d_sig),i,i+1,d)
          call multiply_plane(ST%U1_U2,sig(d_sig),i,i+1,d)
        enddo
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine add_to_diag_S3D(ST,v)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        real(cp),intent(in) :: v
        call add(ST%D_3D,v)
      end subroutine

      subroutine multiply_diag_S3D(ST,v)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        real(cp),intent(in) :: v
        call multiply(ST%D_3D,v)
      end subroutine

      subroutine add_S3D(ST,v)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        real(cp),intent(in) :: v
        integer :: i
        do i=1,3; call add(ST%S(i),v); enddo
        call add(ST%D_3D,v)
        call add(ST%U1_U2,v)
        call add(ST%D1_U2,v)
        call add(ST%U1_D2,v)
        call add(ST%D1_D2,v)
      end subroutine

      subroutine multiply_S3D(ST,v)
        implicit none
        type(stencil_3D),intent(inout) :: ST
        real(cp),intent(in) :: v
        integer :: i
        do i=1,3; call multiply(ST%S(i),v); enddo
        call multiply(ST%D_3D,v)
        call multiply(ST%U1_U2,v)
        call multiply(ST%D1_U2,v)
        call multiply(ST%U1_D2,v)
        call multiply(ST%D1_D2,v)
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine insist_allocated_S3D(ST,caller)
        implicit none
        type(stencil_3D),intent(in) :: ST
        character(len=*),intent(in) :: caller
        call insist_allocated(ST%S(1),caller//' S(1)')
        call insist_allocated(ST%S(2),caller//' S(2)')
        call insist_allocated(ST%S(3),caller//' S(3)')
        call insist_allocated(ST%D_3D,caller//' D_3D')
      end subroutine

      end module