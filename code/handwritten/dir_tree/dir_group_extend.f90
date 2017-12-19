      module dir_group_extend_mod
      use dir_group_mod
      use dir_manip_mod
      use string_mod
      use path_mod
      use path_extend_mod
      implicit none

      private
      public :: init
      public :: make_dir_group

      interface init;     module procedure init_DG;     end interface

      contains

      subroutine init_DG(DG,root,name,PS)
        implicit none
        type(dir_group),intent(inout) :: DG
        type(path),intent(in) :: root
        character(len=*),intent(in) :: name,PS
        type(path) :: temp
        call init(temp,root)
        call init(DG%base    ,temp   ,name      ,PS)
        call init(DG%field   ,DG%base,'field'   ,PS)
        call init(DG%debug   ,DG%base,'debug'   ,PS)
        call init(DG%energy  ,DG%base,'energy'  ,PS)
        call init(DG%residual,DG%base,'residual',PS)
        call init(DG%unsteady,DG%base,'unsteady',PS)
        call init(DG%stats   ,DG%base,'stats'   ,PS)
        call init(DG%BCs     ,DG%base,'BCs'     ,PS)
        call delete(temp)
      end subroutine

      subroutine make_dir_group(DG)
        implicit none
        type(dir_group),intent(in) :: DG
        call make_dir(str(DG%base))
        call make_dir(str(DG%field))
        call make_dir(str(DG%debug))
        call make_dir(str(DG%energy))
        call make_dir(str(DG%residual))
        call make_dir(str(DG%unsteady))
        call make_dir(str(DG%stats))
        call make_dir(str(DG%BCs))
      end subroutine

      end module