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

        type(path) :: tar_p,out_dir,LDC,mat,params,BEM,e_budget,e_budget_N,e_budget_C
        type(path) :: restart_sim,restart1,restart2,restart

        type(path) :: U,B,J,T
        type(path) :: U_e,B_e,J_e,T_e ! energy data
        type(path) :: U_t,B_t,J_t,T_t ! transient data
      end type

      contains

      subroutine init_DT(DT,dir_tar)
        implicit none
        type(dir_tree),intent(inout) :: DT
        character(len=*),intent(in) :: dir_tar
        type(path) :: temp
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
        call init(DT%tar_p,str(DT%tar),'.',str(DT%PS)) ! Cannot use '' (n must > 0)

        call init(DT%out_dir     ,DT%tar_p       ,'out'        ,str(DT%PS))
        call init(DT%LDC         ,DT%out_dir     ,'LDC'        ,str(DT%PS))

        call init(DT%e_budget    ,DT%LDC         ,'e_budget'   ,str(DT%PS))
        call init(DT%mat         ,DT%LDC         ,'material'   ,str(DT%PS))
        call init(DT%params      ,DT%LDC         ,'parameters' ,str(DT%PS))
        call init(DT%BEM         ,DT%LDC         ,'BEM'        ,str(DT%PS))
        call init(DT%restart_sim ,DT%LDC         ,'restart'    ,str(DT%PS))
        call init(DT%U           ,DT%LDC         ,'Ufield'     ,str(DT%PS))
        call init(DT%B           ,DT%LDC         ,'Bfield'     ,str(DT%PS))
        call init(DT%J           ,DT%LDC         ,'Jfield'     ,str(DT%PS))
        call init(DT%T           ,DT%LDC         ,'Tfield'     ,str(DT%PS))

        call init(DT%e_budget_N  ,DT%e_budget    ,'e_budget_N' ,str(DT%PS))
        call init(DT%e_budget_C  ,DT%e_budget    ,'e_budget_C' ,str(DT%PS))
        call init(DT%restart1    ,DT%restart_sim ,'restart1'   ,str(DT%PS))
        call init(DT%restart2    ,DT%restart_sim ,'restart2'   ,str(DT%PS))
        call init(DT%U_t         ,DT%U           ,'transient'  ,str(DT%PS))
        call init(DT%B_t         ,DT%B           ,'transient'  ,str(DT%PS))
        call init(DT%J_t         ,DT%J           ,'transient'  ,str(DT%PS))
        call init(DT%T_t         ,DT%T           ,'transient'  ,str(DT%PS))
        call init(DT%U_e         ,DT%U           ,'energy'     ,str(DT%PS))
        call init(DT%B_e         ,DT%B           ,'energy'     ,str(DT%PS))
        call init(DT%J_e         ,DT%J           ,'energy'     ,str(DT%PS))
        call init(DT%T_e         ,DT%T           ,'energy'     ,str(DT%PS))

        call init(DT%restart,DT%restart1)

        call make_dir_tree(DT)
        call oldest_modified_file(temp,DT%restart1,DT%restart2,'p.dat')
        call delete(temp)
      end subroutine

      subroutine make_dir_tree(DT)
        implicit none
        type(dir_tree),intent(in) :: DT
        call make_dir(full(DT%out_dir))
        call make_dir(full(DT%LDC))

        call make_dir(str(DT%params))
        call make_dir(str(DT%e_budget))
        call make_dir(str(DT%e_budget_C))
        call make_dir(str(DT%e_budget_N))
        call make_dir(str(DT%mat))
        call make_dir(str(DT%BEM))
        call make_dir(str(DT%restart))
        call make_dir(str(DT%restart1))
        call make_dir(str(DT%restart2))

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
        call delete(DT%e_budget)
        call delete(DT%e_budget_C)
        call delete(DT%e_budget_N)
        call delete(DT%BEM)
        call delete(DT%restart)
        call delete(DT%restart1)
        call delete(DT%restart2)

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