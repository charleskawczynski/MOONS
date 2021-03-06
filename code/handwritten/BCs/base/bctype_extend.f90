       module bctype_extend_mod
       use bctype_mod
       use current_precision_mod
       implicit none

       private
       public :: bctype
       public :: init,delete,display,print,export,import ! Essentials
       public :: display_info

       public :: init_Dirichlet,     is_Dirichlet
       public :: init_Neumann,       is_Neumann
       public :: init_Robin,         is_Robin
       public :: init_Periodic,      is_Periodic
       public :: init_symmetric,     is_symmetric
       public :: init_antisymmetric, is_antisymmetric
       public :: is_defined
       public :: is_prescribed
       public :: set_prescribed

       public :: get_mean_value
       public :: display_type,display_meanVal,get_bctype

       interface init;                module procedure init_val0;              end interface
       interface init;                module procedure init_val1;              end interface
       interface init;                module procedure init_val2;              end interface
       interface display_info;        module procedure display_info_bctype;    end interface

       interface init_Dirichlet;      module procedure init_Dirichlet_b;       end interface
       interface init_Neumann;        module procedure init_Neumann_b;         end interface
       interface init_Robin;          module procedure init_Robin_b;           end interface
       interface init_Periodic;       module procedure init_Periodic_b;        end interface
       interface init_symmetric;      module procedure init_symmetric_b;       end interface
       interface init_antisymmetric;  module procedure init_antisymmetric_b;   end interface

       interface is_Dirichlet;        module procedure get_Dirichlet_b;        end interface
       interface is_Neumann;          module procedure get_Neumann_b;          end interface
       interface is_Robin;            module procedure get_Robin_b;            end interface
       interface is_Periodic;         module procedure get_Periodic_b;         end interface
       interface is_symmetric;        module procedure get_symmetric_b;        end interface
       interface is_antisymmetric;    module procedure get_antisymmetric_b;    end interface

       interface display_type;        module procedure display_bctype_T_only;  end interface
       interface display_meanVal;     module procedure display_bctype_MV_only; end interface

       interface is_defined;          module procedure is_defined_BCT;         end interface
       interface is_prescribed;       module procedure is_prescribed_BCT;      end interface
       interface set_prescribed;      module procedure set_prescribed_BCT;     end interface
       interface get_mean_value;      module procedure get_mean_value_BCT;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

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

       subroutine display_info_bctype(b,un)
         implicit none
         type(bctype),intent(in) :: b
         integer,intent(in) :: un
         logical :: L
         L = .false. ! show all regardless

         if (.not.b%defined) then
           write(un,*) 'defined = ',b%defined
           stop 'Error: trying to export bctype before defined in bctype.f90'
         endif
         if (L) then
           write(un,*) 'Dirichlet     = ',b%Dirichlet
           write(un,*) 'Neumann       = ',b%Neumann
           write(un,*) 'Robin         = ',b%Robin
           write(un,*) 'Periodic      = ',b%Periodic
           write(un,*) 'Symmetric     = ',b%symmetric
           write(un,*) 'Antisymmetric = ',b%antisymmetric
           write(un,*) 'prescribed    = ',b%prescribed
         else
           ! if (b%Dirichlet)     write(un,*) 'Dirichlet'
           ! if (b%Neumann)       write(un,*) 'Neumann'
           ! if (b%Periodic)      write(un,*) 'Periodic'
           ! if (b%symmetric)     write(un,*) 'Symmetric'
           ! if (b%antisymmetric) write(un,*) 'Antisymmetric'
           write(un,'(A,T1)',advance='no') get_bctype(b)
         endif

         write(un,'(F5.2)',advance='no') b%meanVal
         ! write(un,*) 'meanVal = ',b%meanVal
         ! write(un,*) 'defined = ',b%defined
       end subroutine

       subroutine init_Dirichlet_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'D'; b%Dirichlet = .true.
       end subroutine
       subroutine init_Neumann_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'N'; b%Neumann = .true.
       end subroutine
       subroutine init_Robin_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'R'; b%Robin = .true.
       end subroutine
       subroutine init_Periodic_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'P'; b%Periodic = .true.
       end subroutine
       subroutine init_symmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'S'; b%symmetric = .true.
       end subroutine
       subroutine init_antisymmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%defined = .true.; b%BCT = 'A'; b%antisymmetric = .true.
       end subroutine

       function get_Dirichlet_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%Dirichlet
       end function
       function get_Neumann_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%Neumann
       end function
       function get_Robin_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%Robin
       end function
       function get_Periodic_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%Periodic
       end function
       function get_symmetric_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%symmetric
       end function
       function get_antisymmetric_b(b) result(L)
         implicit none
         type(bctype),intent(in) :: b
         logical :: L
         L = b%antisymmetric
       end function

       function get_bctype(b) result(BCT)
         implicit none
         type(bctype),intent(in) :: b
         character(len=1) :: BCT
         BCT = 'X'
         ! BCT = b%BCT
         if (b%Dirichlet)     BCT = 'D'
         if (b%Neumann)       BCT = 'N'
         if (b%Robin)         BCT = 'R'
         if (b%Periodic)      BCT = 'P'
         if (b%symmetric)     BCT = 'S'
         if (b%antisymmetric) BCT = 'A'
       end function

       subroutine display_bctype_T_only(b,un)
         implicit none
         type(bctype),intent(in) :: b
         integer,intent(in) :: un
         if (.not.b%defined) stop 'Error: trying to export bctype (T only) before fully defined in bctype.f90'
         write(un,'(A,T1)',advance='no') get_bctype(b)
       end subroutine

       subroutine display_bctype_MV_only(b,un)
         implicit none
         type(bctype),intent(in) :: b
         integer,intent(in) :: un
         if (.not.b%defined) stop 'Error: trying to export bctype (MV only) before fully defined in bctype.f90'
         write(un,'(F19.10)',advance='no') b%meanVal
       end subroutine

       function get_mean_value_BCT(BCT) result(MV)
        implicit none
        type(bctype),intent(in) :: BCT
        real(cp) :: MV
        MV = BCT%meanVal
       end function

       function is_defined_BCT(BCT) result(L)
        implicit none
        type(bctype),intent(in) :: BCT
        logical :: L
        L = BCT%defined
       end function

       subroutine set_prescribed_BCT(BCT)
         implicit none
         type(bctype),intent(inout) :: BCT
         BCT%prescribed = .true.
       end subroutine

       function is_prescribed_BCT(BCT) result(L)
        implicit none
        type(bctype),intent(in) :: BCT
        logical :: L
        L = BCT%prescribed
       end function

       end module