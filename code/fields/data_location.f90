       module data_location_mod
       ! Compiler flags: (_DEBUG_DATA_LOCATION_)
       implicit none

       private
       public :: data_location
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_CC,   is_CC
       public :: init_Node, is_Node
       public :: init_Face, is_Face
       public :: init_Edge, is_Edge

       public :: CC_along,N_along

       public :: vol_ID

       type data_location
         logical :: C,N,E,F = .false.        ! cell center, cell corner, cell edge, cell face
         integer :: face,edge = 0            ! face direction, edge direction
         logical,dimension(3) :: CC_along = .false.
         logical,dimension(3) :: N_along = .false.
         logical :: defined = .false.        ! 
         integer :: volume_ID = 0            ! face direction, edge direction
       end type

       interface init;                module procedure init_copy_DL;            end interface
       interface init_CC;             module procedure init_CC_DL;              end interface
       interface init_Node;           module procedure init_Node_DL;            end interface
       interface init_Face;           module procedure init_Face_DL;            end interface
       interface init_Edge;           module procedure init_Edge_DL;            end interface

       interface is_CC;               module procedure is_CC_DL;                end interface
       interface is_Node;             module procedure is_Node_DL;              end interface
       interface is_Face;             module procedure is_Face_DL;              end interface
       interface is_Edge;             module procedure is_Edge_DL;              end interface

       interface CC_along;            module procedure CC_along_DL;             end interface
       interface N_along;             module procedure N_along_DL;              end interface

       interface vol_ID;              module procedure volume_ID_DL;            end interface

       interface delete;              module procedure delete_DL;               end interface
       interface display;             module procedure display_DL;              end interface
       interface print;               module procedure print_DL;                end interface
       interface export;              module procedure export_DL;               end interface
       interface import;              module procedure import_DL;               end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_copy_DL(DL,DL_in)
         implicit none
         type(data_location),intent(inout) :: DL
         type(data_location),intent(in) :: DL_in
         call delete(DL)
         call insist_defined(DL_in,'init_copy_DL')
         DL%C = DL_in%C
         DL%N = DL_in%N
         DL%E = DL_in%E
         DL%F = DL_in%F
         DL%face = DL_in%face
         DL%edge = DL_in%edge
         DL%defined = DL_in%defined
         DL%CC_along = DL_in%CC_along
         DL%N_along = DL_in%N_along
         DL%volume_ID = DL_in%volume_ID
       end subroutine

       subroutine init_CC_DL(DL)
         implicit none
         type(data_location),intent(inout) :: DL
         call delete(DL)
         DL%C = .true.
         DL%defined = .true.
         DL%volume_ID = 1
         call init_CC_N_along(DL,'init_CC_DL')
       end subroutine

       subroutine init_Node_DL(DL)
         implicit none
         type(data_location),intent(inout) :: DL
         call delete(DL)
         DL%N = .true.
         DL%defined = .true.
         DL%volume_ID = 2
         call init_CC_N_along(DL,'init_Node_DL')
       end subroutine

       subroutine init_Face_DL(DL,dir)
         implicit none
         type(data_location),intent(inout) :: DL
         integer,intent(in) :: dir
         call delete(DL)
         DL%F = .true.
         call insist_valid_dir(DL,dir,'init_Face_DL')
         DL%face = dir
         DL%defined = .true.
         DL%volume_ID = 2+dir
         call init_CC_N_along(DL,'init_Face_DL')
       end subroutine

       subroutine init_Edge_DL(DL,dir)
         implicit none
         type(data_location),intent(inout) :: DL
         integer,intent(in) :: dir
         call delete(DL)
         DL%E = .true.
         call insist_valid_dir(DL,dir,'init_Edge_DL')
         DL%edge = dir
         DL%defined = .true.
         DL%volume_ID = 5+dir
         call init_CC_N_along(DL,'init_Edge_DL')
       end subroutine

       subroutine delete_DL(DL)
         implicit none
         type(data_location),intent(inout) :: DL
         DL%C = .false.
         DL%N = .false.
         DL%E = .false.
         DL%F = .false.
         DL%CC_along = .false.
         DL%N_along = .false.
         DL%face = 0
         DL%edge = 0
         DL%defined = .false.
         DL%volume_ID = 0
       end subroutine

       subroutine display_DL(DL,un)
         implicit none
         type(data_location),intent(in) :: DL
         integer,intent(in) :: un
         write(un,*) 'C = ',DL%C
         write(un,*) 'N = ',DL%N
         write(un,*) 'E = ',DL%E
         write(un,*) 'F = ',DL%F
         write(un,*) 'face = ',DL%face
         write(un,*) 'edge = ',DL%edge
         write(un,*) 'defined = ',DL%defined
         write(un,*) 'CC_along = ',DL%CC_along
         write(un,*) 'N_along = ',DL%N_along
         write(un,*) 'volume_ID = ',DL%volume_ID
       end subroutine

       subroutine print_DL(DL)
         implicit none
         type(data_location),intent(in) :: DL
         call display(DL,6)
       end subroutine

       subroutine export_DL(DL,un)
         implicit none
         type(data_location),intent(in) :: DL
         integer,intent(in) :: un
         write(un,*) 'defined = ';   write(un,*) DL%defined
         write(un,*) 'C = ';         write(un,*) DL%C
         write(un,*) 'N = ';         write(un,*) DL%N
         write(un,*) 'E = ';         write(un,*) DL%E
         write(un,*) 'F = ';         write(un,*) DL%F
         write(un,*) 'face = ';      write(un,*) DL%face
         write(un,*) 'edge = ';      write(un,*) DL%edge
         write(un,*) 'CC_along = ';  write(un,*) DL%CC_along
         write(un,*) 'N_along = ';   write(un,*) DL%N_along
         write(un,*) 'volume_ID = '; write(un,*) DL%volume_ID
       end subroutine

       subroutine import_DL(DL,un)
         implicit none
         type(data_location),intent(inout) :: DL
         integer,intent(in) :: un
         read(un,*); read(un,*) DL%defined
         read(un,*); read(un,*) DL%C
         read(un,*); read(un,*) DL%N
         read(un,*); read(un,*) DL%E
         read(un,*); read(un,*) DL%F
         read(un,*); read(un,*) DL%face
         read(un,*); read(un,*) DL%edge
         read(un,*); read(un,*) DL%CC_along
         read(un,*); read(un,*) DL%N_along
         read(un,*); read(un,*) DL%volume_ID
       end subroutine

       function volume_ID_DL(DL) result(volume_ID)
         implicit none
         type(data_location),intent(in) :: DL
         integer :: volume_ID
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'volume_ID_DL')
#endif
         volume_ID = DL%volume_ID
       end function

       function is_CC_DL(DL) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'is_CC_DL')
#endif
         L = DL%C
       end function

       function is_Node_DL(DL) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'is_Node_DL')
#endif
         L = DL%N
       end function

       function is_Edge_DL(DL) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'is_Edge_DL')
#endif
         L = DL%E
       end function

       function is_Face_DL(DL) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'is_Face_DL')
#endif
         L = DL%F
       end function

       function CC_along_DL(DL,dir) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         integer,intent(in) :: dir
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'CC_along_DL')
         call insist_valid_dir(DL,dir,'init_Edge_DL')
#endif
         L = DL%CC_along(dir)
       end function

       function N_along_DL(DL,dir) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         integer,intent(in) :: dir
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'N_along_DL')
         call insist_valid_dir(DL,dir,'init_Edge_DL')
#endif
         L = DL%N_along(dir)
       end function

        subroutine init_CC_N_along(DL,caller)
          implicit none
          type(data_location),intent(inout) :: DL
          character(len=*),intent(in) :: caller
              if (DL%C) then; DL%CC_along = .true.
          elseif (DL%N) then; DL%CC_along = .false.
          elseif (DL%F) then; DL%CC_along = .not.diag_true(DL%face,caller)
          elseif (DL%E) then; DL%CC_along =      diag_true(DL%edge,caller)
          else
            write(*,*) 'Error: bad data type in '//caller//' in data_location.f90'
            stop 'Done'
          endif
          DL%N_along = .not.DL%CC_along
        end subroutine

        function diag_true(dir,caller) result(L)
          implicit none
          character(len=*),intent(in) :: caller
          integer,intent(in) :: dir
          logical,dimension(3) :: L
          select case(dir)
          case (1);L=(/.true.,.false.,.false./)
          case (2);L=(/.false.,.true.,.false./)
          case (3);L=(/.false.,.false.,.true./)
          case default
          write(*,*) 'Error: dir must = 1,2,3 in ',caller,' in data_location.f90'
          stop 'Done'
          end select
        end function

        subroutine insist_valid_dir(DL,dir,caller)
          implicit none
          type(data_location),intent(in) :: DL
          integer,intent(in) :: dir
          character(len=*),intent(in) :: caller
          if ((dir.ne.1).and.(dir.ne.2).and.(dir.ne.3)) then
            call print(DL)
            write(*,*) 'Error: dir must = 1,2,3 in ',caller,' in data_location.f90'
            stop 'Done'
          endif
        end subroutine

       subroutine insist_defined(DL,caller)
         implicit none
         type(data_location),intent(in) :: DL
         character(len=*),intent(in) :: caller
         if (.not.DL%defined) then
           call print(DL)
           write(*,*) 'Error: undefined DL in ',caller,' in data_location.f90'
           stop 'Done'
         endif
       end subroutine

       end module