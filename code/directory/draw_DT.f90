      module draw_DT_mod
      implicit none

      private
      public :: draw_DT

      contains

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