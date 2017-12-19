       module procedure_array_plane_op_extend_mod
       use procedure_array_plane_op_mod
       use IO_tools_mod
       use single_procedure_plane_op_mod
       use single_procedure_plane_op_extend_mod
       use GF_assign_ghost_mod

       implicit none
       private
       public :: init
       public :: add,remove,check_unique,sort

       interface init;             module procedure init_PA;             end interface
       interface add;              module procedure add_PA;              end interface
       interface add;              module procedure add_PA_SP;           end interface
       interface remove;           module procedure remove_PA;           end interface
       interface check_unique;     module procedure check_unique_PA;     end interface
       interface sort;             module procedure sort_PA;             end interface

       contains

       subroutine init_PA(PA,N)
         implicit none
         type(procedure_array_plane_op),intent(inout) :: PA
         integer,intent(in) :: N
         call delete(PA)
         if (N.lt.1) stop 'Error: N must > 1 in init_PA in procedure_array_plane_op.f90'
         allocate(PA%SP(N))
         PA%N = N
         PA%defined = .false.
       end subroutine

       ! *****************************************************************
       ! ***************************** OTHER *****************************
       ! *****************************************************************

       subroutine add_PA(PA,P,ID)
         implicit none
         type(procedure_array_plane_op),intent(inout) :: PA
         procedure(plane_op) :: P
         integer,intent(in) :: ID
         type(procedure_array_plane_op) :: temp
         integer :: i
         if (PA%defined) then
           call init(temp,PA)
           call init(PA,temp%N+1)
           do i=1,temp%N; call init(PA%SP(i),temp%SP(i)); enddo
           i = PA%N
           call init(PA%SP(i),P,ID)
           call delete(temp)
         else
           call init(PA,1)
           call init(PA%SP(1),P,ID)
         endif
         PA%defined = .true.
       end subroutine

       subroutine add_PA_SP(PA,SP)
         implicit none
         type(procedure_array_plane_op),intent(inout) :: PA
         type(single_procedure_plane_op),intent(in) :: SP
         call add(PA,SP%P,SP%ID)
       end subroutine

       subroutine remove_PA(PA,ID)
         implicit none
         type(procedure_array_plane_op),intent(inout) :: PA
         integer,intent(in) :: ID
         type(procedure_array_plane_op) :: temp
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
         type(procedure_array_plane_op),intent(in) :: PA
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
           write(*,*) 'Error: too many BCs in procedure_array_plane_op in ',caller,'in procedure_array_plane_op.f90'
           write(*,*) 'violating_ID = ',violating_ID
           stop 'Done'
         endif
       end subroutine

       subroutine check_unique_array(a,N,caller)
         implicit none
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: a
         character(len=*),intent(in) :: caller
         integer,dimension(N) :: i_order
         logical,dimension(N) :: L
         integer :: i,j,s
         s = size(a)
         if ((N.lt.1).or.(s.ne.N)) then
           write(*,*) 'Error: bad array size in ',caller,' in procedure_array_plane_op.f90'; stop 'Done'
         endif
         i_order = (/(i,i=1,N)/)
         do i=1,N; L(i) = any((/(a(j).eq.i_order(i),j=1,N)/)); enddo
         if (.not.all(L)) then
           write(*,*) 'Error: non-unique array in ',caller,' in procedure_array_plane_op.f90'
           write(*,*) 'N = ',N
           write(*,*) 'a = ',a
           stop 'Done'
         endif
       end subroutine

       subroutine sort_PA(PA,order,N)
         implicit none
         type(procedure_array_plane_op),intent(inout) :: PA
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: order
         type(procedure_array_plane_op) :: temp
         integer :: i,j
         if ((PA%N.ne.N).or.(size(PA%SP).ne.N)) then
           stop 'Error: bad array size in sort_PA in procedure_array_plane_op.f90'
         endif
         call check_unique(PA,'sort_PA')
         call check_unique_array(order,N,'sort_PA')
         call init(temp,PA)
         call delete(PA)
         do i=1,N; do j=1,N
           if (temp%SP(j)%ID.eq.order(i)) call add(PA,temp%SP(j))
         enddo; enddo
         call delete(temp)
       end subroutine

       end module