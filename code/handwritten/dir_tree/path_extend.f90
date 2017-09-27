      module path_extend_mod
      use path_mod
      use dir_manip_mod
      use string_mod
      implicit none

      private
      public :: path
      public :: init,make,str,rel,full

      interface init;          module procedure init_P;            end interface
      interface init;          module procedure init_ext_rel_str;  end interface
      interface init;          module procedure init_ext_rel_path; end interface
      interface make;          module procedure make_P;            end interface
      interface rel;           module procedure rel_P;             end interface
      interface str;           module procedure str_P;             end interface
      interface full;          module procedure full_P;            end interface

      contains

      subroutine init_P(P,root,rel,PS)
        implicit none
        type(path),intent(inout) :: P
        character(len=*),intent(in) :: root,rel,PS
        call init(P%a,root)
        call init(P%r,rel//PS)
      end subroutine

      subroutine init_ext_rel_str(P,ext,PS)
        implicit none
        type(path),intent(inout) :: P
        character(len=*),intent(in) :: ext,PS
        type(path) :: temp
        call init(temp,P)
        call init(P,full(temp)//ext//PS,rel(temp)//ext,PS) ! calling init_P
        call delete(temp)
      end subroutine

      subroutine init_ext_rel_path(P,P_in,ext,PS)
        implicit none
        type(path),intent(inout) :: P
        type(path),intent(inout) :: P_in
        character(len=*),intent(in) :: ext,PS
        call init(P,full(P_in)//ext//PS,rel(P_in)//ext,PS) ! calling init_P
      end subroutine

      subroutine make_P(P)
        implicit none
        type(path),intent(in) :: P
        call make_dir(str(P%a))
      end subroutine

      function str_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%r)) :: s
        s = str(P%r)
      end function

      function rel_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%r)) :: s
        s = str(P%r)
      end function

      function full_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%a)) :: s
        s = str(P%a)
      end function

      end module