      module dir_manip_mod
      use string_mod
      use IO_tools_mod
      ! Make a buildDirectory routine:
      ! http://homepages.wmich.edu/~korista/README-fortran.html
      ! Using buff(10) = "last modification time"
      ! Ref: https://gcc.gnu.org/onlinedocs/gfortran/STAT.html
      implicit none

#ifdef _OS_LINUX_
      public :: fortran_PS
      character(len=1),parameter :: fortran_PS = '/'
#else
      public :: fortran_PS
      character(len=1),parameter :: fortran_PS = '\'
#endif

      private
      public :: least_recently_modified
      public :: most_recently_modified
      public :: make_dir
      public :: make_dir_quiet
      public :: remove_dir

      interface least_recently_modified; module procedure least_recently_modified_DM;      end interface
      interface most_recently_modified;  module procedure most_recently_modified_DM;       end interface
      interface least_recently_modified; module procedure least_recently_modified_DM_wrap; end interface
      interface most_recently_modified;  module procedure most_recently_modified_DM_wrap;  end interface
      interface make_dir;                module procedure make_dir_DM;                     end interface
      interface make_dir_quiet;          module procedure make_dir_quiet_DM;               end interface
      interface remove_dir;              module procedure remove_dir_DM;                   end interface

      contains

      subroutine least_recently_modified_DM_wrap(folder,folder1,folder2,file_name)
        implicit none
        type(string),intent(inout) :: folder
        character(len=*),intent(in) :: folder1,folder2,file_name
        call least_recently_modified(folder,&
        folder1//file_name//dot_dat,&
        folder2//file_name//dot_dat)
      end subroutine

      subroutine most_recently_modified_DM_wrap(folder,folder1,folder2,file_name)
        implicit none
        type(string),intent(inout) :: folder
        character(len=*),intent(in) :: folder1,folder2,file_name
        call most_recently_modified(folder,&
        folder1//file_name//dot_dat,&
        folder2//file_name//dot_dat)
      end subroutine

      subroutine least_recently_modified_DM(f,f1,f2)
        implicit none
        type(string),intent(inout) :: f
        character(len=*),intent(in) :: f1,f2
        integer, dimension(13) :: buff
        integer :: status1,status2,i1,i2
        call init(f,f1)
        call stat(f1,buff,status1); i1 = buff(10)
        call stat(f2,buff,status2); i2 = buff(10)
        if ((status1.ne.0).and.(status2.ne.0)) then
          write(*,*) 'Error: The following files do not exist!'
          write(*,*) f1
          write(*,*) f2
          stop 'Done in least_recently_modified_DM in dir_manip.f90'
        endif
        if (status1.ne.0) then;     call init(f,f1) ! restart1 empty
        elseif (status2.ne.0) then; call init(f,f2) ! restart1 empty
        else ! Get latest one
          if (i2-i1.gt.0) then; call init(f,f1)
          else;                 call init(f,f2)
          endif
        endif
      end subroutine

      subroutine most_recently_modified_DM(f,f1,f2)
        implicit none
        type(string),intent(inout) :: f
        character(len=*),intent(in) :: f1,f2
        integer, dimension(13) :: buff
        integer :: status1,status2,i1,i2
        call init(f,f1)
        call stat(f1,buff,status1); i1 = buff(10)
        call stat(f2,buff,status2); i2 = buff(10)
        if ((status1.ne.0).and.(status2.ne.0)) then
          write(*,*) 'Error: The following files do not exist!'
          write(*,*) f1
          write(*,*) f2
          stop 'Done in most_recently_modified_DM in dir_manip.f90'
        endif
        if (status1.ne.0) then;     call init(f,f2) ! restart1 empty
        elseif (status2.ne.0) then; call init(f,f1) ! restart1 empty
        else ! Get latest one
          if (i2-i1.gt.0) then; call init(f,f2)
          else;                 call init(f,f1)
          endif
        endif
      end subroutine

      subroutine make_dir_DM(d)
        implicit none
        character(len=*),intent(in) :: d
        logical :: ex
        inquire (file=d, EXIST=ex)
        if (.not.ex) then
          call system('mkdir ' // d )
          write(*,*) 'Directory ' // d // ' created.'
        else
          write(*,*) 'Directory ' // d // ' already exists.'
        endif
      end subroutine

      subroutine make_dir_quiet_DM(d)
        implicit none
        character(len=*),intent(in) :: d
        logical :: ex
        inquire (file=d, EXIST=ex)
        if (.not.ex) call system('mkdir ' // d )
      end subroutine

      subroutine remove_dir_DM(d)
        implicit none
        character(len=*),intent(in) :: d
        logical :: ex
        inquire (file=d, EXIST=ex)
        if (ex) then
          call system('rm -r /' // d )
          write(*,*) 'Directory ' // d // ' deleted.'
        else
          write(*,*) 'Directory ' // d // ' does not exist anyway.'
        endif
      end subroutine

      end module