      module dir_tree_mod
      use string_mod
      use path_mod
      use dir_group_mod
      implicit none

      private
      public :: dir_tree
      public :: init,delete
      public :: make_dir_tree

      interface init;     module procedure init_DT;     end interface
      interface delete;   module procedure delete_DT;   end interface

      type dir_tree
        type(string) :: PS
        ! type(string) :: sim_prefix
        ! type(string) :: sim_suffix
        type(string) :: tar     ! absolute target directory (.exe location)

        type(path) :: tar_p,out_dir,LDC
        type(path) :: mat,meshes,BEM,wall_clock
        type(path) :: params,ISP,TMP,PE,export_now,refine_mesh
        type(path) :: e_budget,e_budget_N,e_budget_C
        type(path) :: restart_sim,restart1,restart2,restart

        type(path) :: unknowns
        type(dir_group) :: U,B,J,T,p,phi,rho
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
        call init(DT%meshes      ,DT%LDC         ,'meshes'     ,str(DT%PS))
        call init(DT%params      ,DT%LDC         ,'parameters' ,str(DT%PS))
        call init(DT%wall_clock  ,DT%LDC         ,'wall_clock' ,str(DT%PS))
        call init(DT%TMP         ,DT%params      ,'TMP'        ,str(DT%PS))
        call init(DT%ISP         ,DT%params      ,'ISP'        ,str(DT%PS))
        call init(DT%PE          ,DT%params      ,'PE'         ,str(DT%PS))
        call init(DT%export_now  ,DT%params      ,'export_now' ,str(DT%PS))
        call init(DT%refine_mesh ,DT%params      ,'refine_mesh',str(DT%PS))
        call init(DT%BEM         ,DT%LDC         ,'BEM'        ,str(DT%PS))
        call init(DT%restart_sim ,DT%LDC         ,'restart'    ,str(DT%PS))

        call init(DT%e_budget_N  ,DT%e_budget    ,'e_budget_N' ,str(DT%PS))
        call init(DT%e_budget_C  ,DT%e_budget    ,'e_budget_C' ,str(DT%PS))
        call init(DT%restart1    ,DT%restart_sim ,'restart1'   ,str(DT%PS))
        call init(DT%restart2    ,DT%restart_sim ,'restart2'   ,str(DT%PS))

        call init(DT%restart,DT%restart1)

        call init(DT%unknowns    ,DT%LDC         ,'unknowns'   ,str(DT%PS))
        call init(DT%U  ,DT%unknowns,'U'  ,str(DT%PS))
        call init(DT%T  ,DT%unknowns,'T'  ,str(DT%PS))
        call init(DT%B  ,DT%unknowns,'B'  ,str(DT%PS))
        call init(DT%J  ,DT%unknowns,'J'  ,str(DT%PS))
        call init(DT%p  ,DT%unknowns,'p'  ,str(DT%PS))
        call init(DT%phi,DT%unknowns,'phi',str(DT%PS))
        call init(DT%rho,DT%unknowns,'rho',str(DT%PS))

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
        call make_dir(str(DT%wall_clock))
        call make_dir(str(DT%ISP))
        call make_dir(str(DT%TMP))
        call make_dir(str(DT%PE))
        call make_dir(str(DT%export_now))
        call make_dir(str(DT%refine_mesh))
        call make_dir(str(DT%e_budget))
        call make_dir(str(DT%e_budget_C))
        call make_dir(str(DT%e_budget_N))
        call make_dir(str(DT%mat))
        call make_dir(str(DT%meshes))
        call make_dir(str(DT%BEM))
        call make_dir(str(DT%restart))
        call make_dir(str(DT%restart1))
        call make_dir(str(DT%restart2))

        call make_dir(str(DT%unknowns))
        call make_dir_group(DT%U)
        call make_dir_group(DT%T)
        call make_dir_group(DT%B)
        call make_dir_group(DT%J)
        call make_dir_group(DT%p)
        call make_dir_group(DT%phi)
        call make_dir_group(DT%rho)
      end subroutine

      subroutine delete_DT(DT)
        implicit none
        type(dir_tree),intent(inout) :: DT
        call delete(DT%tar)
        call delete(DT%PS)
        call delete(DT%out_dir)

        call delete(DT%params)
        call delete(DT%wall_clock)
        call delete(DT%ISP)
        call delete(DT%TMP)
        call delete(DT%PE)
        call delete(DT%export_now)
        call delete(DT%refine_mesh)
        call delete(DT%mat)
        call delete(DT%meshes)
        call delete(DT%e_budget)
        call delete(DT%e_budget_C)
        call delete(DT%e_budget_N)
        call delete(DT%BEM)
        call delete(DT%restart)
        call delete(DT%restart1)
        call delete(DT%restart2)

        call delete(DT%unknowns)
        call delete(DT%T)
        call delete(DT%U)
        call delete(DT%B)
        call delete(DT%J)
        call delete(DT%p)
        call delete(DT%phi)
        call delete(DT%rho)
      end subroutine

      subroutine draw_DT()
        implicit none
        write(*,*) ' ------------------- Directory Tree ------------------- '
        write(*,*) '                           |                            '
        write(*,*) '                       ----------                       '
        write(*,*) '                       |        |                       '
        write(*,*) '                  -------------------                   '
        write(*,*) '                  |        |        |                   '
        write(*,*) '             -------------------------------            '
        write(*,*) '             |        |        |           |            '
        write(*,*) '        -----------------------------------------       '
        write(*,*) '        |        |        |           |         |       '
        write(*,*) ''
      end subroutine

      end module