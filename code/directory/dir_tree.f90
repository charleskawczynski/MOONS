      module dir_tree_mod
      use IO_tools_mod
      use string_mod
      use draw_DT_mod
      use path_mod
      implicit none

      private
      public :: dir_tree
      public :: init,delete
      public :: make_dir_tree
      
      interface init;     module procedure init_DT;     end interface
      interface delete;   module procedure delete_DT;   end interface

      type dir_tree
        type(string) :: PS
        type(string) :: tar     ! absolute target directory (.exe location)
        type(string) :: out_dir ! relative output directory (out/LDC/)

        type(path) :: mat,params,BEM

        type(path) :: U,B,J,T
        type(path) :: U_e,B_e,J_e,T_e ! energy data
        type(path) :: U_t,B_t,J_t,T_t ! transient data
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

        call append(DT%tar,str(DT%PS))

          call init(DT%out_dir,'out'//str(DT%PS)); call make_dir(str(DT%out_dir))
        call append(DT%out_dir,'LDC'//str(DT%PS)); call make_dir(str(DT%out_dir))

        call init(DT%mat   ,str(DT%tar),str(DT%out_dir)//'material'  ,str(DT%PS))
        call init(DT%params,str(DT%tar),str(DT%out_dir)//'parameters',str(DT%PS))
        call init(DT%BEM   ,str(DT%tar),str(DT%out_dir)//'BEM'       ,str(DT%PS))

        call init(DT%U,str(DT%tar),str(DT%out_dir)//'Ufield',str(DT%PS))
        call init(DT%B,str(DT%tar),str(DT%out_dir)//'Bfield',str(DT%PS))
        call init(DT%J,str(DT%tar),str(DT%out_dir)//'Jfield',str(DT%PS))
        call init(DT%T,str(DT%tar),str(DT%out_dir)//'Tfield',str(DT%PS))

        call init(DT%U_t,str(DT%U%a),str(DT%U)//'transient',str(DT%PS))
        call init(DT%B_t,str(DT%B%a),str(DT%B)//'transient',str(DT%PS))
        call init(DT%J_t,str(DT%J%a),str(DT%J)//'transient',str(DT%PS))
        call init(DT%T_t,str(DT%T%a),str(DT%T)//'transient',str(DT%PS))

        call init(DT%U_e,str(DT%U%a),str(DT%U)//'energy',str(DT%PS))
        call init(DT%B_e,str(DT%B%a),str(DT%B)//'energy',str(DT%PS))
        call init(DT%J_e,str(DT%J%a),str(DT%J)//'energy',str(DT%PS))
        call init(DT%T_e,str(DT%T%a),str(DT%T)//'energy',str(DT%PS))
        call make_dir_tree(DT)
      end subroutine

      subroutine make_dir_tree(DT)
        implicit none
        type(dir_tree),intent(in) :: DT
        call make_dir(str(DT%params))
        call make_dir(str(DT%mat))
        call make_dir(str(DT%BEM))

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

      subroutine delete_DT(DT)
        implicit none
        type(dir_tree),intent(inout) :: DT
        call delete(DT%tar)
        call delete(DT%PS)
        call delete(DT%out_dir)

        call delete(DT%params)
        call delete(DT%mat)
        call delete(DT%BEM)

        call delete(DT%U)
        call delete(DT%B)
        call delete(DT%J)
        call delete(DT%T)

        call delete(DT%U_t)
        call delete(DT%B_t)
        call delete(DT%J_t)
        call delete(DT%T_t)

        call delete(DT%U_e)
        call delete(DT%B_e)
        call delete(DT%J_e)
        call delete(DT%T_e)
      end subroutine

      end module