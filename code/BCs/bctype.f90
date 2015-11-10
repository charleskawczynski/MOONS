       module bctype_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: bctype
       public :: init_Dirichlet,init_Neumann,init_Periodic,init_symmetric,init_antisymmetric
       public :: init,delete
       public :: print,export

       type bctype
         logical :: Dirichlet
         logical :: Neumann
         logical :: Periodic
         logical :: symmetric
         logical :: antisymmetric
         logical :: defined
         real(cp) :: meanVal
       end type

       interface init_Dirichlet;      module procedure init_Dirichlet_b;      end interface
       interface init_Neumann;        module procedure init_Neumann_b;        end interface
       interface init_Periodic;       module procedure init_Periodic_b;       end interface
       interface init_symmetric;      module procedure init_symmetric_b;      end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_b;  end interface

       interface init;                module procedure init_copy_b;           end interface

       interface init;                 module procedure init_val0;            end interface
       interface init;                 module procedure init_val1;            end interface
       interface init;                 module procedure init_val2;            end interface

       interface delete;              module procedure delete_bctype;         end interface
       interface print;               module procedure print_bctype;          end interface
       interface export;              module procedure export_bctype;         end interface

       contains

       ! *******************************************************************************
       ! *********************************** TYPE **************************************
       ! *******************************************************************************

       subroutine delete_bctype(b)
         implicit none
         type(bctype),intent(inout) :: b
         b%Dirichlet = .false.
         b%Neumann = .false.
         b%Periodic = .false.
         b%symmetric = .false.
         b%antisymmetric = .false.
         b%defined = .false.
       end subroutine

       subroutine init_Dirichlet_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Dirichlet = .true.; b%defined = .true.
       end subroutine
       subroutine init_Neumann_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Neumann = .true.; b%defined = .true.
       end subroutine
       subroutine init_Periodic_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Periodic = .true.; b%defined = .true.
       end subroutine
       subroutine init_symmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%symmetric = .true.; b%defined = .true.
       end subroutine
       subroutine init_antisymmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%antisymmetric = .true.; b%defined = .true.
       end subroutine

       subroutine init_copy_b(b_out,b_in)
         implicit none
         type(bctype),intent(inout) :: b_out
         type(bctype),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy BC that has not been fully defined'
         b_out%meanVal = b_in%meanVal
         b_out%Dirichlet = b_in%Dirichlet
         b_out%Neumann = b_in%Neumann
         b_out%Periodic = b_in%Periodic
         b_out%symmetric = b_in%symmetric
         b_out%antisymmetric = b_in%antisymmetric
         b_out%defined = b_in%defined
       end subroutine

       ! *******************************************************************************
       ! ********************************** VAL ****************************************
       ! *******************************************************************************

       subroutine init_val0(b,val)
         implicit none
         type(bctype),intent(inout) :: b
         real(cp),intent(in) :: val
         b%meanVal = val
       end subroutine

       subroutine init_val1(b,val)
         implicit none
         type(bctype),intent(inout) :: b
         real(cp),dimension(:),intent(in) :: val
         b%meanVal = sum(val)/real(size(val),cp)
       end subroutine

       subroutine init_val2(b,val)
         implicit none
         type(bctype),intent(inout) :: b
         real(cp),dimension(:,:),intent(in) :: val
         b%meanVal = sum(val)/real(size(val),cp)
       end subroutine

       ! *******************************************************************************
       ! ******************************** PRINT/EXPORT *********************************
       ! *******************************************************************************

       subroutine print_bctype(b)
         implicit none
         type(bctype), intent(in) :: b
         call export(b,6)
       end subroutine

       subroutine export_bctype(b,NewU)
         implicit none
         type(bctype),intent(in) :: b
         integer,intent(in) :: NewU
         logical :: TF
         TF = .false. ! show all regardless

         if (.not.b%defined) then 
           write(newU,*) 'defined = ',b%defined
           stop 'Error: trying to export bctype before fully defined'
         endif
         if (TF) then
           write(newU,*) 'Dirichlet = ',b%Dirichlet
           write(newU,*) 'Neumann = ',b%Neumann
           write(newU,*) 'Periodic = ',b%Periodic
           write(newU,*) 'Symmetric = ',b%symmetric
           write(newU,*) 'Antisymmetric = ',b%antisymmetric
         else
           ! if (b%Dirichlet)     write(newU,*) 'Dirichlet'
           ! if (b%Neumann)       write(newU,*) 'Neumann'
           ! if (b%Periodic)      write(newU,*) 'Periodic'
           ! if (b%symmetric)     write(newU,*) 'Symmetric'
           ! if (b%antisymmetric) write(newU,*) 'Antisymmetric'

           if (b%Dirichlet)     write(newU,'(A,T1)',advance='no') 'D'
           if (b%Neumann)       write(newU,'(A,T1)',advance='no') 'N'
           if (b%Periodic)      write(newU,'(A,T1)',advance='no') 'P'
           if (b%symmetric)     write(newU,'(A,T1)',advance='no') 'S'
           if (b%antisymmetric) write(newU,'(A,T1)',advance='no') 'A'
         endif

         write(newU,*) 'meanVal = ',b%meanVal
         ! write(newU,*) 'defined = ',b%defined
       end subroutine

       end module