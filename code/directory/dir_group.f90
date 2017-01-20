      module dir_group_mod
      use string_mod
      use path_mod
      implicit none

      private
      public :: dir_group
      public :: init,delete
      public :: make_dir_group

      interface init;     module procedure init_DG;     end interface
      interface delete;   module procedure delete_DG;   end interface

      type dir_group
        type(path) :: base
        type(path) :: field
        type(path) :: restart
        type(path) :: energy
        type(path) :: residual
        type(path) :: transient
        type(path) :: BCs
      end type

      contains

      subroutine init_DG(DG,root,name,PS)
        implicit none
        type(dir_group),intent(inout) :: DG
        type(path),intent(in) :: root
        character(len=*),intent(in) :: name,PS
        type(path) :: temp
        call init(temp,root)
        call init(DG%base     ,temp   ,name       ,PS)
        call init(DG%field    ,DG%base,'field'    ,PS)
        call init(DG%restart  ,DG%base,'restart'  ,PS)
        call init(DG%energy   ,DG%base,'energy'   ,PS)
        call init(DG%residual ,DG%base,'residual' ,PS)
        call init(DG%transient,DG%base,'transient',PS)
        call init(DG%BCs      ,DG%base,'BCs'      ,PS)
        call delete(temp)
      end subroutine

      subroutine make_dir_group(DG)
        implicit none
        type(dir_group),intent(in) :: DG
        call make_dir(str(DG%base))
        call make_dir(str(DG%field))
        call make_dir(str(DG%restart))
        call make_dir(str(DG%energy))
        call make_dir(str(DG%residual))
        call make_dir(str(DG%transient))
        call make_dir(str(DG%BCs))
      end subroutine

      subroutine delete_DG(DG)
        implicit none
        type(dir_group),intent(inout) :: DG
        call delete(DG%base)
        call delete(DG%field)
        call delete(DG%restart)
        call delete(DG%energy)
        call delete(DG%residual)
        call delete(DG%transient)
        call delete(DG%BCs)
      end subroutine

      end module