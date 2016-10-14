       module procedure_array_mod
       use IO_tools_mod
       use single_procedure_mod
       use apply_BCs_faces_bridge_mod

       implicit none
       private
       public :: procedure_array
       public :: init,delete,display,print,export,import

       public :: add,remove,check_unique,sort

       type procedure_array
         integer :: N
         type(single_procedure),dimension(:),allocatable :: SP
         logical :: defined = .false.
       end type

       interface init;             module procedure init_PA;             end interface
       interface init;             module procedure init_copy_PA;        end interface
       interface delete;           module procedure delete_PA;           end interface
       interface display;          module procedure display_PA;          end interface
       interface print;            module procedure print_PA;            end interface
       interface export;           module procedure export_PA;           end interface
       interface import;           module procedure import_PA;           end interface
       interface export;           module procedure export_PA_wrapper;   end interface
       interface import;           module procedure import_PA_wrapper;   end interface

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

       subroutine init_copy_PA(PA,PA_in)
         implicit none
         type(procedure_array),intent(inout) :: PA
         type(procedure_array),intent(in) :: PA_in
         integer :: i
         call insist_defined(PA_in,'init_copy_PA')
         ! call insist_allocated(PA_in,'init_copy_PA')
         call delete(PA)
         call init(PA,PA_in%N)
         do i=1,PA%N
          call init(PA%SP(i),PA_in%SP(i))
         enddo
         PA%defined = PA_in%defined
         PA%N = PA_in%N
       end subroutine

       subroutine delete_PA(PA)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer :: i
         if (allocated(PA%SP)) then
           do i=1,size(PA%SP); call delete(PA%SP(i)); enddo
           deallocate(PA%SP)
         endif
         PA%N = 0
         PA%defined = .false.
       end subroutine

       subroutine display_PA(PA,un)
         implicit none
         type(procedure_array),intent(in) :: PA
         integer,intent(in) :: un
         integer :: i
         write(un,*) ' ***************** PROCEDURE ARRAY ***************** '
         write(un,*) 'PA%N = ',PA%N
         do i=1,PA%N; call display(PA%SP(i),un); enddo
         write(un,*) ' *************************************************** '
       end subroutine

       subroutine print_PA(PA)
         implicit none
         type(procedure_array),intent(in) :: PA
         call display(PA,6)
       end subroutine

       subroutine export_PA(PA,un)
         implicit none
         type(procedure_array),intent(in) :: PA
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'PA%N = '
         write(un,*) PA%N
         do i=1,PA%N; call export(PA%SP(i),un); enddo
       end subroutine

       subroutine import_PA(PA,un)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: un
         integer :: i
         read(un,*) 
         read(un,*) PA%N
         do i=1,PA%N; call import(PA%SP(i),un); enddo
       end subroutine

       subroutine export_PA_wrapper(PA,dir,name)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(PA,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_PA_wrapper(PA,dir,name)
         implicit none
         type(procedure_array),intent(inout) :: PA
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(PA,un)
         call close_and_message(un,dir,name)
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
           write(*,*) 'Error: too many BCs in procedure_array in ',caller,'in procedure_array.f90'
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
           write(*,*) 'Error: bad array size in ',caller,' in procedure_array.f90'; stop 'Done'
         endif
         i_order = (/(i,i=1,N)/)
         do i=1,N; L(i) = any((/(a(j).eq.i_order(i),j=1,N)/)); enddo
         if (.not.all(L)) then
           write(*,*) 'Error: non-unique array in ',caller,' in procedure_array.f90'
           write(*,*) 'N = ',N
           write(*,*) 'a = ',a
           stop 'Done'
         endif
       end subroutine

       subroutine sort_PA(PA,order,N)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: N
         integer,dimension(N),intent(in) :: order
         type(procedure_array) :: temp
         integer :: i,j
         if ((PA%N.ne.N).or.(size(PA%SP).ne.N)) then
           stop 'Error: bad array size in sort_PA in procedure_array.f90'
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

       ! subroutine apply_PA(PA,BF,B)
       !   implicit none
       !   type(procedure_array),intent(in) :: PA
       !   type(block_field),intent(inout) :: BF
       !   type(block),intent(in) :: B
       !   integer :: i
       !   do i=1,PA%N
       !   call PA%SP(i)%P(BF,B)
       !   enddo
       ! end subroutine

       subroutine insist_defined_PA(PA,caller)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: caller
         integer :: i
         do i=1,PA%N
         call insist_defined(PA%SP(i),caller)
         enddo
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