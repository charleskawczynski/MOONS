       module procedure_array_extend_mod
       use procedure_array_mod
       use IO_tools_mod
       use single_procedure_mod
       use single_procedure_extend_mod
       use apply_BCs_faces_bridge_mod
       use apply_face_BC_op_mod

       implicit none
       private
       public :: init

       public :: add,remove,check_unique,sort
       public :: add_PA
       public :: insist_allocated
       public :: insist_defined

       interface init;             module procedure init_PA;             end interface
       interface insist_defined;   module procedure insist_defined_PA;   end interface
       interface insist_allocated; module procedure insist_allocated_PA; end interface

       interface add;              module procedure add_PA;              end interface
       interface add;              module procedure add_PA_SP;           end interface
       interface remove;           module procedure remove_PA;           end interface
       interface check_unique;     module procedure check_unique_PA;     end interface
       interface sort;             module procedure sort_PA;             end interface

       contains

       subroutine init_PA(PA,N)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: N
         call delete(PA)
         if (N.lt.1) stop 'Error: N must > 1 in init_PA in procedure_array.f90'
         allocate(PA%SP(N))
         PA%N = N
         PA%defined = .false.
       end subroutine

       ! *****************************************************************
       ! ***************************** OTHER *****************************
       ! *****************************************************************

       subroutine add_PA(PA,P,ID)
         implicit none
         type(procedure_array),intent(inout) :: PA
         procedure(apply_face_BC_op) :: P
         integer,intent(in) :: ID
         type(procedure_array) :: temp
         integer :: i
         if (PA%defined) then
           if (.not.in_set_array((/(PA%SP(i)%ID,i=1,PA%N)/),ID,PA%N,'add_PA')) then
             call init(temp,PA)
             call init(PA,temp%N+1)
             do i=1,temp%N; call init(PA%SP(i),temp%SP(i)); enddo
             i = PA%N
             call init(PA%SP(i),P,ID)
             call delete(temp)
           endif
         else
           call init(PA,1)
           call init(PA%SP(1),P,ID)
         endif
         PA%defined = .true.
       end subroutine

       subroutine add_PA_SP(PA,SP)
         implicit none
         type(procedure_array),intent(inout) :: PA
         type(single_procedure),intent(in) :: SP
         call add(PA,SP%P,SP%ID)
       end subroutine

       subroutine remove_PA(PA,ID)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: ID
         type(procedure_array) :: temp
         integer :: i
         if (PA%defined) then
           call delete(temp)
           do i=1,PA%N
             if (PA%SP(i)%ID.ne.ID) call add(temp,PA%SP(i))
           enddo
           call init(PA,temp)
           call delete(temp)
         else; call delete(PA)
         endif
       end subroutine

       subroutine check_unique_PA(PA,caller)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: caller
         integer :: i,j,violating_ID
         logical :: unique_set
         unique_set = .true.; violating_ID = 0
         do i=1,PA%N; do j=i,PA%N
         if (i.ne.j) then
         if (PA%SP(i)%ID.eq.PA%SP(j)%ID) then
           unique_set = .false.
           violating_ID = i
         endif
         endif
         enddo; enddo
         if (.not.unique_set) then
           write(*,*) 'Error: too many BCs in procedure_array in ',caller,' in procedure_array.f90'
           write(*,*) 'violating_ID = ',violating_ID
           call print(PA)
           stop 'Done'
         endif
       end subroutine

       subroutine check_unique_array(a,N,caller)
         implicit none
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: a
         character(len=*),intent(in) :: caller
         if (.not.unique_array(a,N,caller)) then
           write(*,*) 'Error: non-unique array in ',caller,' in procedure_array.f90'
           write(*,*) 'N = ',N
           write(*,*) 'a = ',a
           stop 'Done'
         endif
       end subroutine

       function unique_array(a,N,caller) result(L_all)
         implicit none
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: a
         character(len=*),intent(in) :: caller
         logical,dimension(N) :: match
         logical :: L_all
         integer :: i,j
         if ((N.lt.1).or.(size(a).ne.N)) then
           write(*,*) 'Error: bad array size in ',caller,' in procedure_array.f90'; stop 'Done'
         endif
         match = .false.
         do i=1,N;do j=1,N
         if (i.ne.j) match(i) = a(j).eq.a(i)
         enddo; enddo
         L_all = .not.any(match)
       end function

       function in_set_array(a,i,N,caller) result(L_any)
         implicit none
         integer,intent(in) :: N,i
         integer,dimension(N),intent(in) :: a
         character(len=*),intent(in) :: caller
         logical,dimension(N) :: L
         logical :: L_any
         integer :: j
         if ((N.lt.1).or.(size(a).ne.N)) then
           write(*,*) 'Error: bad array size in ',caller,' in procedure_array.f90'; stop 'Done'
         endif
         do j=1,N; L(j) = a(j).eq.i; enddo
         L_any = any(L)
       end function

       subroutine sort_PA(PA,order,N)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: order
         type(procedure_array) :: temp
         integer :: i,j
         if (PA%N.lt.1) then
           call delete(PA)
         else
           call check_unique(PA,'sort_PA')
           call check_unique_array(order,N,'sort_PA')
           call init(temp,PA)
           call delete(PA)
           do i=1,N; do j=1,temp%N
             if (temp%SP(j)%ID.eq.order(i)) call add(PA,temp%SP(j))
           enddo; enddo
           call delete(temp)
         endif
       end subroutine

       subroutine insist_defined_PA(PA,caller)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: caller
         integer :: i
         do i=1,PA%N
         call insist_defined(PA%SP(i),caller)
         enddo
         if (.not.PA%defined) then
           write(*,*) 'Error: PA must be defined in ',caller,' in procedure_array.f90'
           stop 'Done'
         endif
       end subroutine

       subroutine insist_allocated_PA(PA,caller)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: caller
         if (.not.allocated(PA%SP)) then
           write(*,*) 'Error: PA must be allocated in ',caller,' in procedure_array.f90'
           stop 'Done'
         endif
       end subroutine

       end module