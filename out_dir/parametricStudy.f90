       program parametricStudy
       use MOONS_mod
       implicit none
       character(len=255) :: cwd
       call getcwd(cwd)
       call MOONS('out\LDC\',trim(cwd))
       end program