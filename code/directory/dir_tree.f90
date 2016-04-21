      module dir_tree_mod
      use IO_tools_mod
      use string_mod
      use draw_DT_mod
      implicit none

      private
      public :: dir_tree
      public :: init
      public :: make_dir_tree
      
      interface init;     module procedure init_DT;   end interface

      type dir_tree
        type(string) :: PS
        type(string) :: tar  ! target root (.exe location)
        type(string) :: root ! output root
        type(string) :: U,B,J,T
        type(string) :: U_e,B_e,J_e,T_e ! energy data
        type(string) :: U_t,B_t,J_t,T_t ! transient data
        type(string) :: mat
        type(string) :: params
      end type

      contains

      subroutine init_DT(DT,dir_tar)
        implicit none
        type(dir_tree),intent(inout) :: DT
        character(len=*),intent(in) :: dir_tar
#ifdef _OS_LINUX_
       character(len=1),parameter :: PS = '/'
#else
       character(len=1),parameter :: PS = '\'
#endif
        call init(DT%PS,PS)
        call init(DT%tar,dir_tar)
        call compress(DT%tar)
        call draw_DT()

        call init(DT%root,str(DT%tar))

        call append(DT%root,str(DT%PS)//'out'//str(DT%PS))
        call make_dir(str(DT%root))
        call append(DT%root,'LDC'//str(DT%PS))
        call make_dir(str(DT%root))

        call init(DT%mat,DT%root);    call append(DT%mat,'material'//str(DT%PS))
        call init(DT%params,DT%root); call append(DT%params,'parameters'//str(DT%PS))

        call init(DT%U,DT%root); call append(DT%U,'Ufield'//str(DT%PS))
        call init(DT%B,DT%root); call append(DT%B,'Bfield'//str(DT%PS))
        call init(DT%J,DT%root); call append(DT%J,'Jfield'//str(DT%PS))
        call init(DT%T,DT%root); call append(DT%T,'Tfield'//str(DT%PS))

        call init(DT%U_t,DT%U); call append(DT%U_t,'energy'//str(DT%PS))
        call init(DT%B_t,DT%B); call append(DT%B_t,'energy'//str(DT%PS))
        call init(DT%J_t,DT%J); call append(DT%J_t,'energy'//str(DT%PS))
        call init(DT%T_t,DT%T); call append(DT%T_t,'energy'//str(DT%PS))

        call init(DT%U_e,DT%U); call append(DT%U_e,'transient'//str(DT%PS))
        call init(DT%B_e,DT%B); call append(DT%B_e,'transient'//str(DT%PS))
        call init(DT%J_e,DT%J); call append(DT%J_e,'transient'//str(DT%PS))
        call init(DT%T_e,DT%T); call append(DT%T_e,'transient'//str(DT%PS))
        call make_dir_tree(DT)
      end subroutine

      subroutine make_dir_tree(DT)
        implicit none
        type(dir_tree),intent(in) :: DT
        call make_dir(str(DT%params))
        call make_dir(str(DT%mat))

        call make_dir(str(DT%U))
        call make_dir(str(DT%B))
        call make_dir(str(DT%J))
        call make_dir(str(DT%T))

        call make_dir(str(DT%U_t))
        call make_dir(str(DT%B_t))
        call make_dir(str(DT%J_t))
        call make_dir(str(DT%T_t))

        call make_dir(str(DT%U_e))
        call make_dir(str(DT%B_e))
        call make_dir(str(DT%J_e))
        call make_dir(str(DT%T_e))
      end subroutine

      end module