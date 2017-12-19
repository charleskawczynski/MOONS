       module data_location_extend_mod
       use data_location_mod
       ! Compiler flags: (_DEBUG_DATA_LOCATION_)
       use dir_manip_mod
       implicit none

       private
       public :: data_location
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_CC,   is_CC,   is_CC_VF,   is_CC_TF
       public :: init_Node, is_Node, is_Node_VF, is_Node_TF
       public :: init_Face, is_Face, is_Face_VF, is_Face_TF
       public :: init_Edge, is_Edge, is_Edge_VF, is_Edge_TF
       public :: is_CC_Edge_TF

       public :: DL_CC
       public :: DL_Node
       public :: DL_Face
       public :: DL_Edge

       public :: get_Face
       public :: get_Edge

       public :: CC_along,N_along

       public :: insist_collocated
       public :: is_collocated_VF
       public :: is_collocated_TF

       public :: CC_eye
       public :: N_eye

       public :: vol_ID

       public :: get_char
       public :: defined
       public :: indentical

       interface init_CC;             module procedure init_CC_DL;              end interface
       interface init_Node;           module procedure init_Node_DL;            end interface
       interface init_Face;           module procedure init_Face_DL;            end interface
       interface init_Edge;           module procedure init_Edge_DL;            end interface

       interface indentical;          module procedure indentical_DL;           end interface

       interface DL_CC;               module procedure DL_CC_DL;                end interface
       interface DL_Node;             module procedure DL_Node_DL;              end interface
       interface DL_Face;             module procedure DL_Face_DL;              end interface
       interface DL_Edge;             module procedure DL_Edge_DL;              end interface

       interface is_CC;               module procedure is_CC_DL;                end interface
       interface is_Node;             module procedure is_Node_DL;              end interface
       interface is_Face;             module procedure is_Face_DL;              end interface
       interface is_Edge;             module procedure is_Edge_DL;              end interface

       interface is_CC_VF;            module procedure is_CC_VF_DL;             end interface
       interface is_Node_VF;          module procedure is_Node_VF_DL;           end interface
       interface is_Face_VF;          module procedure is_Face_VF_DL;           end interface
       interface is_Edge_VF;          module procedure is_Edge_VF_DL;           end interface

       interface is_CC_TF;            module procedure is_CC_TF_DL;             end interface
       interface is_Node_TF;          module procedure is_Node_TF_DL;           end interface
       interface is_Face_TF;          module procedure is_Face_TF_DL;           end interface
       interface is_Edge_TF;          module procedure is_Edge_TF_DL;           end interface
       interface is_CC_Edge_TF;       module procedure is_CC_Edge_TF_DL;        end interface

       interface get_Face;            module procedure get_Face_DL;             end interface
       interface get_Edge;            module procedure get_Edge_DL;             end interface

       interface CC_along;            module procedure CC_along_DL;             end interface
       interface N_along;             module procedure N_along_DL;              end interface

       interface vol_ID;              module procedure volume_ID_DL;            end interface

       interface CC_eye;              module procedure CC_eye_DL;               end interface
       interface N_eye;               module procedure N_eye_DL;                end interface

       interface get_char;            module procedure get_char_DL;             end interface
       interface defined;             module procedure defined_DL;              end interface
       interface insist_collocated;   module procedure insist_collocated_DL;    end interface
       interface is_collocated_VF;    module procedure is_collocated_VF_DL;     end interface
       interface is_collocated_TF;    module procedure is_collocated_TF_DL;     end interface

       interface print;               module procedure print_DL3;               end interface

       contains

       function indentical_DL(A,B) result(L_all)
         implicit none
         type(data_location),intent(in) :: A,B
         logical,dimension(12) :: L
         logical :: L_all
         L(1)  = A%C         .eqv. B%C
         L(2)  = A%N         .eqv. B%N
         L(3)  = A%E         .eqv. B%E
         L(4)  = A%F         .eqv. B%F
         L(5)  = A%face      .eq. B%face
         L(6)  = A%edge      .eq. B%edge
         L(7)  = A%defined   .eqv. B%defined
         L(8)  = all(A%CC_along  .eqv. B%CC_along)
         L(9)  = all(A%N_along   .eqv. B%N_along)
         L(10) = A%volume_ID .eq. B%volume_ID
         L(11) = all(A%CC_eye    .eq. B%CC_eye)
         L(12) = all(A%N_eye     .eq. B%N_eye)
         L_all = all(L)
       end function

       subroutine init_CC_DL(DL)
         implicit none
         type(data_location),intent(inout) :: DL
         call delete(DL)
         DL%C = .true.
         DL%defined = .true.
         DL%volume_ID = 1
         call init_CC_N_along(DL,'init_CC_DL')
         DL%N_eye = (/0,0,0/)
         DL%CC_eye = (/1,1,1/)
       end subroutine

       subroutine init_Node_DL(DL)
         implicit none
         type(data_location),intent(inout) :: DL
         call delete(DL)
         DL%N = .true.
         DL%defined = .true.
         DL%volume_ID = 2
         call init_CC_N_along(DL,'init_Node_DL')
         DL%N_eye = (/1,1,1/)
         DL%CC_eye = (/0,0,0/)
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
         select case (dir)
         case (1); DL%N_eye = (/1,0,0/); DL%CC_eye = (/0,1,1/)
         case (2); DL%N_eye = (/0,1,0/); DL%CC_eye = (/1,0,1/)
         case (3); DL%N_eye = (/0,0,1/); DL%CC_eye = (/1,1,0/)
         end select
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
         select case (dir)
         case (1); DL%N_eye = (/0,1,1/); DL%CC_eye = (/1,0,0/)
         case (2); DL%N_eye = (/1,0,1/); DL%CC_eye = (/0,1,0/)
         case (3); DL%N_eye = (/1,1,0/); DL%CC_eye = (/0,0,1/)
         end select
       end subroutine

       subroutine print_DL3(DL)
         implicit none
         type(data_location),dimension(3),intent(in) :: DL
         call display(DL(1),6)
         call display(DL(2),6)
         call display(DL(3),6)
       end subroutine

       function CC_eye_DL(DL) result(I)
         implicit none
         type(data_location),intent(in) :: DL
         integer,dimension(3) :: I
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'CC_eye_DL')
#endif
         I = DL%CC_eye
       end function

       function N_eye_DL(DL) result(I)
         implicit none
         type(data_location),intent(in) :: DL
         integer,dimension(3) :: I
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'N_eye_DL')
#endif
         I = DL%N_eye
       end function

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

       function is_CC_VF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(3),intent(in) :: DL
         logical,dimension(3) :: L
         logical :: L_final
         integer :: i
         do i=1,3; L(i) = is_CC(DL(i)); enddo
         L_final = all(L)
       end function

       function is_Node_VF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(3),intent(in) :: DL
         logical,dimension(3) :: L
         logical :: L_final
         integer :: i
         do i=1,3; L(i) = is_Node(DL(i)); enddo
         L_final = all(L)
       end function

       function is_Edge_VF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(3),intent(in) :: DL
         logical,dimension(3) :: L
         logical :: L_final
         integer :: i
         do i=1,3; L(i) = is_Edge(DL(i)).and.(get_Edge(DL(i)).eq.i); enddo
         L_final = all(L)
       end function

       function is_Face_VF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(3),intent(in) :: DL
         logical,dimension(3) :: L
         logical :: L_final
         integer :: i
         do i=1,3; L(i) = is_Face(DL(i)).and.(get_Face(DL(i)).eq.i); enddo
         L_final = all(L)
       end function

       function is_CC_TF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(9),intent(in) :: DL
         logical,dimension(9) :: L
         logical :: L_final
         integer :: i
         do i=1,9; L(i) = is_CC(DL(i)); enddo
         L_final = all(L)
       end function

       function is_Node_TF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(9),intent(in) :: DL
         logical,dimension(9) :: L
         logical :: L_final
         integer :: i
         do i=1,9; L(i) = is_Node(DL(i)); enddo
         L_final = all(L)
       end function

       function is_Edge_TF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(9),intent(in) :: DL
         logical,dimension(9) :: L
         logical :: L_final
         integer :: i
         do i=1,9; L(i) = is_Edge(DL(i)).and.(get_Edge(DL(i)).eq.i); enddo
         L_final = all(L)
       end function

       function is_Face_TF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(9),intent(in) :: DL
         logical,dimension(9) :: L
         logical :: L_final
         integer :: i
         do i=1,9; L(i) = is_Face(DL(i)).and.(get_Face(DL(i)).eq.i); enddo
         L_final = all(L)
       end function

       function is_CC_Edge_TF_DL(DL) result(L_final)
         implicit none
         type(data_location),dimension(9),intent(in) :: DL
         logical,dimension(9) :: L
         logical :: L_final
         L(1) = is_CC(DL(1))
         L(2) = is_Face(DL(2)).and.(get_Face(DL(2)).eq.2)
         L(3) = is_Face(DL(3)).and.(get_Face(DL(3)).eq.3)

         L(4) = is_Face(DL(4)).and.(get_Face(DL(4)).eq.4)
         L(5) = is_CC(DL(5))
         L(6) = is_Face(DL(6)).and.(get_Face(DL(6)).eq.6)

         L(7) = is_Face(DL(7)).and.(get_Face(DL(7)).eq.7)
         L(8) = is_Face(DL(8)).and.(get_Face(DL(8)).eq.8)
         L(9) = is_CC(DL(9))
         L_final = all(L)
       end function

       function DL_CC_DL() result(DL)
         implicit none
         type(data_location) :: DL
         call init_CC(DL)
       end function

       function DL_Node_DL() result(DL)
         implicit none
         type(data_location) :: DL
         call init_Node(DL)
       end function

       function DL_Edge_DL(dir) result(DL)
         implicit none
         integer,intent(in) :: dir
         type(data_location) :: DL
         call init_Edge(DL,dir)
       end function

       function DL_Face_DL(dir) result(DL)
         implicit none
         integer,intent(in) :: dir
         type(data_location) :: DL
         call init_Face(DL,dir)
       end function

       function get_Face_DL(DL) result(dir)
         implicit none
         type(data_location),intent(in) :: DL
         integer :: dir
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'get_Face_DL')
#endif
         dir = DL%face
       end function

       function get_Edge_DL(DL) result(dir)
         implicit none
         type(data_location),intent(in) :: DL
         integer :: dir
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'get_Edge_DL')
#endif
         dir = DL%edge
       end function

       function CC_along_DL(DL,dir) result(L)
         implicit none
         type(data_location),intent(in) :: DL
         integer,intent(in) :: dir
         logical :: L
#ifdef _DEBUG_DATA_LOCATION_
         call insist_defined(DL,'CC_along_DL')
         call insist_valid_dir(DL,dir,'CC_along_DL')
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
         call insist_valid_dir(DL,dir,'N_along_DL')
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

        function defined_DL(DL) result(defined)
          implicit none
          type(data_location),intent(in) :: DL
          logical :: defined
          defined = DL%defined
        end function

        subroutine insist_collocated_DL(DL,caller)
          implicit none
          type(data_location),dimension(3),intent(in) :: DL
          character(len=*),intent(in) :: caller
          if (.not.is_collocated_VF(DL)) then
            call print(DL(1))
            call print(DL(2))
            call print(DL(3))
            write(*,*) 'Error: DLs are not collocated in '//caller//' in data_location.f90'
            stop 'Done'
          endif
        end subroutine

        function is_collocated_VF_DL(DL) result(L_final)
          implicit none
          type(data_location),dimension(3),intent(in) :: DL
          integer :: i
          logical :: L_final
          logical,dimension(4) :: L
          L(1) = all((/(DL(i)%C,i=1,3)/))
          L(2) = all((/(DL(i)%N,i=1,3)/))
          L(3) = all((/(DL(i)%F.and.(DL(i)%face.eq.DL(1)%face),i=1,3)/))
          L(4) = all((/(DL(i)%E.and.(DL(i)%edge.eq.DL(1)%edge),i=1,3)/))
          L_final = any(L)
        end function

        function is_collocated_TF_DL(DL) result(L_final)
          implicit none
          type(data_location),dimension(9),intent(in) :: DL
          integer :: i
          logical :: L_final
          logical,dimension(4) :: L
          L(1) = all((/(DL(i)%C,i=1,9)/))
          L(2) = all((/(DL(i)%N,i=1,9)/))
          L(3) = all((/(DL(i)%F.and.(DL(i)%face.eq.DL(1)%face),i=1,9)/))
          L(4) = all((/(DL(i)%E.and.(DL(i)%edge.eq.DL(1)%edge),i=1,9)/))
          L_final = any(L)
        end function

        function get_char_DL(DL) result(c)
          implicit none
          type(data_location),intent(in) :: DL
          character(len=1) :: c
              if (DL%C) then; c = 'c'
          elseif (DL%N) then; c = 'n'
          elseif (DL%F) then; c = 'f'
          elseif (DL%E) then; c = 'e'
          else; stop 'Error: bad input type in get_char_DL in data_location.f90'
          endif
        end function

        subroutine insist_valid_dir(DL,dir,caller)
          implicit none
          type(data_location),intent(in) :: DL
          integer,intent(in) :: dir
          character(len=*),intent(in) :: caller
          if ((dir.ne.1).and.(dir.ne.2).and.(dir.ne.3)) then
            write(*,*) '-------------------------------------------------------------'
            call print(DL)
            write(*,*) 'Error: dir must = 1,2,3 in ',caller,' in data_location.f90'
            write(*,*) 'dir = ',dir
            write(*,*) '-------------------------------------------------------------'
            stop 'Done'
          endif
        end subroutine

#ifdef _DEBUG_DATA_LOCATION_
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
#endif

       end module