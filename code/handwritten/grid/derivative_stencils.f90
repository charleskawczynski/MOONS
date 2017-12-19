      module derivative_stencils_mod
      use current_precision_mod
      use array_mod
      use array_extend_mod
      use sparse_mod
      use sparse_extend_mod
      implicit none

      private
      public :: staggered_CC2N
      public :: staggered_N2CC

      public :: collocated_Node_1 ! use modified stencils near boundary
      public :: collocated_Node_2 ! use modified stencils near boundary
      public :: collocated_CC_1   ! use modified stencils near boundary
      public :: collocated_CC_2   ! use modified stencils near boundary

      public :: consecutive_stag_CC ! consecutive staggered derivatives (central diff near boundary)
      public :: consecutive_stag_N  ! consecutive staggered derivatives (central diff near boundary)

      ! Depricated
      public :: collocated_CC_1_centered ! not used, and may be incorrectly written
      public :: collocated_CC_2_centered ! not used, and may be incorrectly written

      contains

      ! *****************************************************************
      ! *************************** STAGGERED ***************************
      ! *****************************************************************

      function staggered_CC2N(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'staggered_CC2N')
        if (s.gt.1) then
          allocate(L( 1 )); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(SP,L,D,U,1,s-1,s-1)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        SP%staggered = .true.
      end function

      function staggered_N2CC(dh,s) result(SP)
        implicit none
        integer,intent(in) :: s
        real(cp),dimension(s-1),intent(in) :: dh
        type(sparse) :: SP
        real(cp),dimension(:),allocatable :: L,D,U
        integer :: i
        call check_valid_size(s,'staggered_N2CC')
        if (s.gt.1) then
          allocate(L( 1 )); L = 0.0_cp
          allocate(D(s-2)); D = 0.0_cp
          allocate(U(s-2)); U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(SP,L,D,U,1,s-1,s-1)
          deallocate(L,D,U)
        else
          call init(SP,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        SP%staggered = .true.
      end function

      ! *****************************************************************
      ! *************************** COLLOCATED **************************
      ! *****************************************************************

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
        SP%staggered = .false.
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
        SP%staggered = .false.
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
        SP%staggered = .false.
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
        SP%staggered = .false.
      end function

      ! *****************************************************************
      ! ************************** CONSECUTIVE **************************
      ! *****************************************************************

      function consecutive_stag_CC(CC,N) result(S)
        implicit none
        type(sparse),intent(in) :: CC,N
        type(sparse) :: S
        type(array) :: L_CC,U_CC
        type(array) :: L_N,U_N
        type(array),dimension(2) :: D_CC,D_N,D_combined
        integer :: e,i
        call init(L_CC,(/CC%D%f,0.0_cp/),CC%D%N+1)
        call init(D_CC(1),(/CC%D%f,0.0_cp/),CC%D%N+1)
        call init(D_CC(2),(/0.0_cp,CC%U%f/),CC%U%N+1)
        call init(U_CC,(/0.0_cp,CC%U%f/),CC%U%N+1)
        e = N%D%N; call init(L_N   ,N%D%f,N%D%N)
        e = N%D%N; call init(D_N(1),N%D%f,N%D%N)
        e = N%U%N; call init(D_N(2),N%U%f,N%U%N)
        e = N%U%N; call init(U_N   ,N%U%f,N%U%N)
        call init(S,L_CC%N)
        call init(D_combined(1),D_CC(1)%N)
        call init(D_combined(2),D_CC(2)%N)
        call multiply(S%L,L_CC,L_N)
        call multiply(S%U,U_CC,U_N)
        call multiply(D_combined(1),D_CC(1),D_N(1))
        call multiply(D_combined(2),D_CC(2),D_N(2))
        call add(S%D,D_combined(1),D_combined(2))
        do i=1,2; call delete(D_CC(i)); enddo
        do i=1,2; call delete(D_N(i)); enddo
        do i=1,2; call delete(D_combined(i)); enddo
        call delete(L_N)
        call delete(U_N)
        call delete(L_CC)
        call delete(U_CC)
        S%staggered = .false.
      end function

      function consecutive_stag_N(CC2N,N2CC) result(S)
        implicit none
        type(sparse) :: S
        type(sparse),intent(in) :: CC2N,N2CC
        type(array) :: L_CC,U_CC
        type(array) :: L_N,U_N
        type(array),dimension(2) :: D_CC,D_N,D_combined
        integer :: i,s_result
        call init(L_CC,CC2N%D);   call insert(L_CC,0.0_cp);    call append(L_CC,0.0_cp)
        call init(D_CC(1),CC2N%D);call insert(D_CC(1),0.0_cp); call append(D_CC(1),0.0_cp)
        call init(D_CC(2),CC2N%U);call insert(D_CC(2),0.0_cp); call append(D_CC(2),0.0_cp)
        call init(U_CC,CC2N%U);   call insert(U_CC,0.0_cp);    call append(U_CC,0.0_cp)

        call init(L_N,N2CC%D);    call insert(L_N,0.0_cp)
        call init(D_N(1),N2CC%D); call append(D_N(1),0.0_cp)
        call init(D_N(2),N2CC%U); call append(D_N(2),0.0_cp)
        call init(U_N,N2CC%U);    call append(U_N,0.0_cp)

        s_result = N2CC%D%N+1
        call init(S,s_result)
        call init(D_combined(1),s_result)
        call init(D_combined(2),s_result)
        call multiply(S%L,L_CC,L_N)
        call multiply(S%U,U_CC,U_N)
        call multiply(D_combined(1),D_CC(1),D_N(1))
        call multiply(D_combined(2),D_CC(2),D_N(2))
        call add(S%D,D_combined(1),D_combined(2))

        do i=1,2; call delete(D_CC(i)); enddo
        do i=1,2; call delete(D_N(i)); enddo
        do i=1,2; call delete(D_combined(i)); enddo
        call delete(L_N)
        call delete(U_N)
        call delete(L_CC)
        call delete(U_CC)
        S%staggered = .false.
      end function

      ! *****************************************************************
      ! ***************************** DEBUG *****************************
      ! *****************************************************************

      subroutine check_valid_size(s,caller)
        implicit none
        integer,intent(in) :: s
        character(len=*),intent(in) :: caller
        if (s.lt.0) then
          write(*,*) 'Error: s < 0 in ',caller,' in derivative_stencils.f90'
        endif
      end subroutine

      ! *****************************************************************
      ! ****************************** OLD ******************************
      ! *****************************************************************

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
        SP%staggered = .false.
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
        SP%staggered = .false.
      end function
      end module