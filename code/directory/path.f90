      module path_mod
      use string_mod
      implicit none

      private
      public :: path
      public :: init,make,str,rel,full,delete
      public :: oldest_modified_file,latest_modified_file
      public :: make_dir,remove_dir
      
      interface init;          module procedure init_P;            end interface
      interface init;          module procedure init_ext_rel_str;  end interface
      interface init;          module procedure init_ext_rel_path; end interface
      interface init;          module procedure init_copy_P;       end interface
      interface make;          module procedure make_P;            end interface
      interface rel;           module procedure rel_P;             end interface
      interface str;           module procedure str_P;             end interface
      interface full;          module procedure full_P;            end interface
      interface delete;        module procedure delete_P;          end interface

      type path
        type(string) :: a,r ! a (Absolute), r (Relative)
      end type

      contains

      subroutine init_P(P,root,rel,PS)
        implicit none
        type(path),intent(inout) :: P
        character(len=*),intent(in) :: root,rel,PS
        call init(P%a,root)
        call init(P%r,rel//PS)
      end subroutine

      subroutine init_copy_P(P_out,P_in)
        implicit none
        type(path),intent(inout) :: P_out
        type(path),intent(in) :: P_in
        call init(P_out%a,P_in%a)
        call init(P_out%r,P_in%r)
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

      subroutine delete_P(P)
        implicit none
        type(path),intent(inout) :: P
        call delete(P%a)
        call delete(P%r)
      end subroutine

      subroutine oldest_modified_file(p,p1,p2,file_name)
        ! Using buff(10) = "last modification time"
        ! Ref: https://gcc.gnu.org/onlinedocs/gfortran/STAT.html
        implicit none
        type(path),intent(inout) :: p
        type(path),intent(in) :: p1,p2
        character(len=*),intent(in) :: file_name
        integer, dimension(13) :: buff
        integer :: status1,status2,i1,i2
        call init(p,p1)
        call stat(full(p1)//file_name,buff,status1); i1 = buff(10)
        call stat(full(p2)//file_name,buff,status2); i2 = buff(10)
        if (status1.ne.0) then;     call init(p,p1) ! restart1 empty
        elseif (status2.ne.0) then; call init(p,p2) ! restart2 empty
        else ! Re-write oldest one
          if (i2-i1.gt.0) then; call init(p,p1)
          else;                 call init(p,p2)
          endif
        endif
      end subroutine

      subroutine latest_modified_file(p,p1,p2,file_name)
        ! Using buff(10) = "last modification time"
        ! Ref: https://gcc.gnu.org/onlinedocs/gfortran/STAT.html
        implicit none
        type(path),intent(inout) :: p
        type(path),intent(in) :: p1,p2
        character(len=*),intent(in) :: file_name
        integer, dimension(13) :: buff
        integer :: status1,status2,i1,i2
        call init(p,p1)
        call stat(full(p1)//file_name,buff,status1); i1 = buff(10)
        call stat(full(p2)//file_name,buff,status2); i2 = buff(10)
        if ((status1.ne.0).and.(status2.ne.0)) then
          stop 'Error: cannot restart, no restart files in latest_modified_file in path.f90'
        endif
        if (status1.ne.0) then;     call init(p,p2) ! restart1 empty
        elseif (status2.ne.0) then; call init(p,p1) ! restart1 empty
        else ! Get latest one
          if (i2-i1.gt.0) then; call init(p,p2)
          else;                 call init(p,p1)
          endif
        endif
      end subroutine

      ! Make a buildDirectory routine:
      ! http://homepages.wmich.edu/~korista/README-fortran.html

      subroutine make_dir(d1,d2,d3,d4)
        character(len=*),intent(in) :: d1
        character(len=*),intent(in),optional :: d2,d3,d4
        type(string) :: s
        logical :: ex
        if (present(d2).and.present(d3).and.present(d4)) then
          call init(s,d1//d2//d3//d4)
        elseif (present(d3)) then
          call init(s,d1//d2//d3)
        elseif (present(d2)) then
          call init(s,d1//d2)
        else
          call init(s,d1)
        endif

        inquire (file=str(s), EXIST=ex)
        if (.not.ex) then
          call system('mkdir ' // str(s) )
          write(*,*) 'Directory ' // str(s) // ' created.'
        else 
          write(*,*) 'Directory ' // str(s) // ' already exists.'
        endif
        call delete(s)
      end subroutine

      subroutine remove_dir(d)
        character(len=*),intent(in) :: d
        call system('rm -r /' // d )
        write(*,*) 'Directory ' // d // ' removed.'
      end subroutine

      end module