       program main_program
       use MOONS_main_mod
       implicit none
       character(len=255) :: cwd
       call getcwd(cwd)
       call main(trim(cwd))
       end program