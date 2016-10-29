      module interpolation_stencils_mod
      use current_precision_mod
      use sparse_mod
      implicit none

      private
      public :: interpolation_stencil
      
      contains

      subroutine interpolation_stencil(hc,hn,sc,sn)
        implicit none
        integer,intent(in) :: sc,sn
        real(cp),dimension(sc),intent(in) :: hc
        real(cp),dimension(sn),intent(in) :: hn
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i,s
        s = sc
        call check_valid_size(s,'interpolation_stencil')
        if (s.gt.1) then
          allocate(L( 1 )); L = 0.0_cp
          allocate(D(s-1)); D = 0.0_cp
          allocate(U(s-1)); U = 0.0_cp
          D = -(/( (hn(i+1) - hc(i))/(hc(i+1) - hc(i)) ,i=1,s-1)/)
          U =  (/( 1.0_cp - D(i) ,i=1,s-1)/)
          call init(SP,L,D,U,1,s-1,s-1)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end subroutine

      subroutine check_valid_size(s,caller)
        implicit none
        integer,intent(in) :: s
        character(len=*),intent(in) :: caller
        if (s.lt.0) then
          write(*,*) 'Error: s < 0 in ',caller,' in interpolation_stencils.f90'
        endif
      end subroutine

      end module