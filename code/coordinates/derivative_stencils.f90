      module derivative_stencils_mod
      use current_precision_mod
      use sparse_mod
      implicit none

      private
      public :: staggered_CC2N
      public :: collocated_Node_1
      public :: collocated_Node_2

      public :: staggered_N2CC
      public :: collocated_CC_1
      public :: collocated_CC_1_centered
      public :: collocated_CC_2
      public :: collocated_CC_2_centered
      
      contains

      ! ********************** NODE DATA STENCILS ***********************

      function staggered_CC2N(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'staggered_CC2N')
        if (s.gt.1) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(SP,L,D,U,s-1)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_Node_1(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_Node_1')
        if (s.gt.3) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          i = 2 ! Front
          L(1) = -(dh(i+1)+2.0_cp*dh(i))/(dh(i)*(dh(i+1)+dh(i)))
          D(1) = (dh(i+1)+dh(i))/(dh(i+1)*dh(i))
          U(1) = -dh(i)/(dh(i+1)*(dh(i+1)+dh(i)))
          ! Interior
          L(2:s-3) = -(/( (dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=3,s-2 )/)
          D(2:s-3) =  (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))  ,i=3,s-2 )/)
          U(2:s-3) =  (/( (dh(i-1)/(dh( i )*(dh(i-1)+dh(i)))) ,i=3,s-2 )/)
          i = s-1 ! Back
          L(s-2) = dh(i-1)/(dh(i-2)*(dh(i-1)+dh(i-2)))
          D(s-2) = -(dh(i-1)+dh(i-2))/(dh(i-1)*dh(i-2))
          U(s-2) = (2.0_cp*dh(i-1)+dh(i-2))/(dh(i-1)*(dh(i-1)+dh(i-2)))
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_Node_2(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_Node_2')
        if (s.gt.3) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          ! Front
          i = 2
          L(1) =  2.0_cp/(dh( i )*(dh(i+1)+dh(i)))
          D(1) = -2.0_cp/(dh(i+1)*dh(i))
          U(1) =  2.0_cp/(dh(i+1)*(dh(i+1)+dh(i)))
          ! Interior
          L(2:s-3) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
          D(2:s-3) =  (/(-2.0_cp/(dh(i-1)*dh(i))           ,i=3,s-2 )/)
          U(2:s-3) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
          ! Back
          i = s-1
          L(s-2) =  2.0_cp/(dh(i-2)*(dh(i-1)+dh(i-2)))
          D(s-2) = -2.0_cp/(dh(i-1)*dh(i-2))
          U(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i-2)))
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else; call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      ! *********************** CC DATA STENCILS ************************

      function staggered_N2CC(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'staggered_N2CC')
        if (s.gt.1) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(SP,L,D,U,s-1)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_CC_1(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_CC_1')
        if (s.gt.2) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          ! Front, dh(i-1) => 0.5 dh(i-1)
          i = 2
          L(1) = (-(dh(i))/((0.5_cp*dh(i-1))*((0.5_cp*dh(i-1))+dh(i))))
          D(1) = ((-(0.5_cp*dh(i-1))+dh(i))/((0.5_cp*dh(i-1))*dh(i)))
          U(1) = ((0.5_cp*dh(i-1))/(dh(i)*((0.5_cp*dh(i-1))+dh(i))))
          ! Interior
          L(2:s-3) = (/( (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=3,s-2 )/)
          D(2:s-3) = (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))   ,i=3,s-2 )/)
          U(2:s-3) = (/( (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))    ,i=3,s-2 )/)
          ! Back, dh(i) => 0.5 dh(i)
          i = s-1
          L(s-2) = (-0.5_cp*dh(i)/(dh(i-1)*(dh(i-1)+(0.5_cp*dh(i)))))
          D(s-2) = ((-dh(i-1)+(0.5_cp*dh(i)))/(dh(i-1)*(0.5_cp*dh(i))))
          U(s-2) = (dh(i-1)/((0.5_cp*dh(i))*(dh(i-1)+(0.5_cp*dh(i)))))
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_CC_1_centered(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_CC_1_centered')
        if (s.gt.2) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          ! Interior
          L(1:s-2) = (/( (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=2,s-1 )/)
          D(1:s-2) = (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))   ,i=2,s-1 )/)
          U(1:s-2) = (/( (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))    ,i=2,s-1 )/)
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_CC_2(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_CC_2')
        if (s.gt.2) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          ! Front, dh(i-1) => 0.5 dh(i-1)
          i = 2
          L(1) =  2.0_cp/((0.5_cp*dh(i-1))*((0.5_cp*dh(i-1))+dh(i)))
          D(1) = -2.0_cp/((0.5_cp*dh(i-1))*dh(i))
          U(1) =  2.0_cp/(dh(i)*((0.5_cp*dh(i-1))+dh(i)))
          ! Interior
          L(2:s-3) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
          D(2:s-3) = -(/( 2.0_cp/(dh(i-1)*dh(i))           ,i=3,s-2 )/)
          U(2:s-3) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
          ! Back, dh(i) => 0.5 dh(i)
          i = s-1
          L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+(0.5_cp*dh(i))))
          D(s-2) = -2.0_cp/(dh(i-1)*(0.5_cp*dh(i)))
          U(s-2) =  2.0_cp/((0.5_cp*dh(i))*(dh(i-1)+(0.5_cp*dh(i))))
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      function collocated_CC_2_centered(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'collocated_CC_2_centered')
        if (s.gt.2) then
          allocate(L(s-2)); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          ! Interior
          L(1:s-2) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
          D(1:s-2) = -(/( 2.0_cp/(dh(i-1)*dh(i))           ,i=2,s-1 )/)
          U(1:s-2) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
          call init(SP,L,D,U,s-2)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end function

      subroutine check_valid_size(s,caller)
        implicit none
        integer,intent(in) :: s
        character(len=*),intent(in) :: caller
        if (s.lt.0) then
          write(*,*) 'Error: s < 0 in ',caller,' in derivative_stencils.f90'
        endif
      end subroutine

      end module