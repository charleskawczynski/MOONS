       program parametricStudy
       use MOONS_mod
       use varStr_mod
       implicit none
       character(len=255) :: cwd
       call getcwd(cwd)
       type(varStr) :: dir,dir_full
       call init(dir_full,cwd)
       call init(dir,'out'//PS//'LDC'//PS)
       call MOONS(dir,dir_full)
       call delete(dir)
       call delete(dir_full)
       end program