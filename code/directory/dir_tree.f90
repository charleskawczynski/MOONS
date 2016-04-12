      module dir_tree_mod
      use IO_tools_mod
      use string_mod
      implicit none

      private
      public :: dir_tree
      public :: init
      public :: make_dir_tree
      
      interface init;     module procedure init_DT;   end interface

      type dir_tree
        type(string) :: PS
        type(string) :: root ! output root
        type(string) :: tar  ! target root (.exe location)
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
#ifdef _OS_WINDOWS_
       character(len=1),parameter :: PS = '/'
#endif
#ifdef _OS_LINUX_
       character(len=1),parameter :: PS = '/'
#endif
        call init(DT%PS,PS)
        call init(DT%tar,dir_tar)
        call compress(DT%tar)

        call init(DT%root,str(DT%tar))
        call append(DT%root,str(DT%PS)//'out'//str(DT%PS)//'LDC'//str(DT%PS)) ! out\LDC\

        call init(DT%mat,DT%root);    call append(DT%mat,str(DT%PS)//'material'//str(DT%PS))
        call init(DT%params,DT%root); call append(DT%params,str(DT%PS)//'parameters'//str(DT%PS))

        call init(DT%U,DT%root); call append(DT%U,str(DT%PS)//'Ufield'//str(DT%PS))
        call init(DT%B,DT%root); call append(DT%B,str(DT%PS)//'Bfield'//str(DT%PS))
        call init(DT%J,DT%root); call append(DT%J,str(DT%PS)//'Jfield'//str(DT%PS))
        call init(DT%T,DT%root); call append(DT%T,str(DT%PS)//'Tfield'//str(DT%PS))

        call init(DT%U_t,DT%U); call append(DT%U_t,str(DT%PS)//'energy'//str(DT%PS))
        call init(DT%B_t,DT%B); call append(DT%B_t,str(DT%PS)//'energy'//str(DT%PS))
        call init(DT%J_t,DT%J); call append(DT%J_t,str(DT%PS)//'energy'//str(DT%PS))
        call init(DT%T_t,DT%T); call append(DT%T_t,str(DT%PS)//'energy'//str(DT%PS))

        call init(DT%U_e,DT%U); call append(DT%U_e,str(DT%PS)//'transient'//str(DT%PS))
        call init(DT%B_e,DT%B); call append(DT%B_e,str(DT%PS)//'transient'//str(DT%PS))
        call init(DT%J_e,DT%J); call append(DT%J_e,str(DT%PS)//'transient'//str(DT%PS))
        call init(DT%T_e,DT%T); call append(DT%T_e,str(DT%PS)//'transient'//str(DT%PS))
      end subroutine

      subroutine make_dir_tree(DT)
        implicit none
        type(dir_tree),intent(in) :: DT
        call make_dir(str(DT%root))

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