       module bctype_mod
       use current_precision_mod
       implicit none

       private
       public :: bctype
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_Dirichlet,     is_Dirichlet
       public :: init_Neumann,       is_Neumann
       public :: init_Robin,         is_Robin
       public :: init_Periodic,      is_Periodic
       public :: init_symmetric,     is_symmetric
       public :: init_antisymmetric, is_antisymmetric
       public :: is_defined

       public :: get_mean_value
       public :: display_type,display_meanVal,get_bctype

       type bctype
         private
         logical :: Dirichlet = .false.
         logical :: Neumann = .false.
         logical :: Robin = .false.
         logical :: Periodic = .false.
         logical :: symmetric = .false.
         logical :: antisymmetric = .false.
         logical :: defined = .false.
         real(cp) :: meanVal = 0.0_cp
         character(len=1) :: BCT = 'X'
       end type

       interface init;                module procedure init_copy_b;            end interface
       interface init;                module procedure init_val0;              end interface
       interface init;                module procedure init_val1;              end interface
       interface init;                module procedure init_val2;              end interface
       interface delete;              module procedure delete_bctype;          end interface
       interface display;             module procedure display_bctype;         end interface
       interface print;               module procedure print_bctype;           end interface
       interface export;              module procedure export_bctype;          end interface
       interface import;              module procedure import_bctype;          end interface

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

       subroutine init_copy_b(b_out,b_in)
         implicit none
         type(bctype),intent(inout) :: b_out
         type(bctype),intent(in) :: b_in
         if (.not.b_in%defined) stop 'Error: trying to copy bctype that has not been fully defined in bctype.f90'
         b_out%meanVal = b_in%meanVal
         b_out%Dirichlet = b_in%Dirichlet
         b_out%Neumann = b_in%Neumann
         b_out%Robin = b_in%Robin
         b_out%Periodic = b_in%Periodic
         b_out%symmetric = b_in%symmetric
         b_out%antisymmetric = b_in%antisymmetric
         b_out%defined = b_in%defined
         b_out%BCT = b_in%BCT
       end subroutine

       subroutine delete_bctype(b)
         implicit none
         type(bctype),intent(inout) :: b
         b%Dirichlet = .false.
         b%Neumann = .false.
         b%Robin = .false.
         b%Periodic = .false.
         b%symmetric = .false.
         b%antisymmetric = .false.
         b%defined = .false.
         b%BCT = 'X'
       end subroutine

       subroutine display_bctype(b,un)
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
           write(un,*) 'Dirichlet = ',b%Dirichlet
           write(un,*) 'Neumann = ',b%Neumann
           write(un,*) 'Robin = ',b%Robin
           write(un,*) 'Periodic = ',b%Periodic
           write(un,*) 'Symmetric = ',b%symmetric
           write(un,*) 'Antisymmetric = ',b%antisymmetric
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

       subroutine print_bctype(b)
         implicit none
         type(bctype), intent(in) :: b
         call display(b,6)
       end subroutine

       subroutine export_bctype(b,un)
         implicit none
         type(bctype),intent(in) :: b
         integer,intent(in) :: un
         write(un,*) 'Dirichlet';      write(un,*) b%Dirichlet
         write(un,*) 'Neumann';        write(un,*) b%Neumann
         write(un,*) 'Robin';          write(un,*) b%Robin
         write(un,*) 'Periodic';       write(un,*) b%Periodic
         write(un,*) 'symmetric';      write(un,*) b%symmetric
         write(un,*) 'antisymmetric';  write(un,*) b%antisymmetric
         write(un,*) 'defined';        write(un,*) b%defined
         write(un,*) 'meanVal';        write(un,*) b%meanVal
         write(un,*) 'BCT';            write(un,*) b%BCT
       end subroutine

       subroutine import_bctype(b,un)
         implicit none
         type(bctype),intent(inout) :: b
         integer,intent(in) :: un
         read(un,*); read(un,*) b%Dirichlet
         read(un,*); read(un,*) b%Neumann
         read(un,*); read(un,*) b%Robin
         read(un,*); read(un,*) b%Periodic
         read(un,*); read(un,*) b%symmetric
         read(un,*); read(un,*) b%antisymmetric
         read(un,*); read(un,*) b%defined
         read(un,*); read(un,*) b%meanVal
         read(un,*); read(un,*) b%BCT
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine init_Dirichlet_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Dirichlet = .true.; b%defined = .true.; b%BCT = 'D'
       end subroutine
       subroutine init_Neumann_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Neumann = .true.; b%defined = .true.; b%BCT = 'N'
       end subroutine
       subroutine init_Robin_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Robin = .true.; b%defined = .true.; b%BCT = 'R'
       end subroutine
       subroutine init_Periodic_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%Periodic = .true.; b%defined = .true.; b%BCT = 'P'
       end subroutine
       subroutine init_symmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%symmetric = .true.; b%defined = .true.; b%BCT = 'S'
       end subroutine
       subroutine init_antisymmetric_b(b)
         implicit none
         type(bctype),intent(inout) :: b
         call delete(b); b%antisymmetric = .true.; b%defined = .true.; b%BCT = 'A'
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

       end module