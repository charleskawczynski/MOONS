       module key_value_pair_mod
       use preset_ID_mod
       use string_mod
       implicit none

       private
       public :: key_value_pair
       public :: init,delete,export,import,display,print

       public :: KVP
       public :: set_false
       public :: get_value
       public :: is_key

       type key_value_pair
         type(string) :: key
         logical :: value
         type(preset_ID) :: BC,IC
         integer :: geometry
       end type

       interface init;      module procedure init_KVP;      end interface
       interface init;      module procedure init_copy_KVP; end interface
       interface delete;    module procedure delete_KVP;    end interface
       interface export;    module procedure export_KVP;    end interface
       interface import;    module procedure import_KVP;    end interface
       interface display;   module procedure display_KVP;   end interface
       interface print;     module procedure print_KVP;     end interface

       interface KVP;       module procedure KVP_KVP;       end interface
       interface set_false; module procedure set_false_KVP; end interface
       interface get_value; module procedure get_value_KVP; end interface
       interface is_key;    module procedure is_key_KVP;    end interface

       contains

       ! **********************************************************
       ! ********************* EKVPENTIALS *************************
       ! **********************************************************

       subroutine init_KVP(KVP,key,value,IC,BC,geometry)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         type(preset_ID),intent(in) :: IC,BC
         character(len=*),intent(in) :: key
         logical,intent(in) :: value
         integer,intent(in) :: geometry
         call init(KVP%key,key)
         call init(KVP%IC,IC)
         call init(KVP%BC,BC)
         KVP%value = value
         KVP%geometry = geometry
       end subroutine

       subroutine init_copy_KVP(KVP,KVP_in)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         type(key_value_pair),intent(in) :: KVP_in
         call delete(KVP)
         call init(KVP%key,KVP_in%key)
         call init(KVP%IC,KVP_in%IC)
         call init(KVP%BC,KVP_in%BC)
         KVP%value = KVP_in%value
         KVP%geometry = KVP_in%geometry
       end subroutine

       subroutine delete_KVP(KVP)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         call delete(KVP%key)
         call delete(KVP%IC)
         call delete(KVP%BC)
         KVP%value = .false.
         KVP%geometry = 0
       end subroutine

       subroutine export_KVP(KVP,un)
         implicit none
         type(key_value_pair),intent(in) :: KVP
         integer,intent(in) :: un
         call export(KVP%key,un)
         write(un,*) KVP%value
       end subroutine

       subroutine import_KVP(KVP,un)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         integer,intent(in) :: un
         call import(KVP%key,un)
         write(un,*) KVP%value
       end subroutine

       subroutine display_KVP(KVP,un)
         implicit none
         type(key_value_pair),intent(in) :: KVP
         integer,intent(in) :: un
         ! write(un,*) str(KVP%key)//' = ',KVP%value
         write(un,*) KVP%value,' : '//str(KVP%key)
       end subroutine

       subroutine print_KVP(KVP)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         call display(KVP,6)
       end subroutine

       function KVP_KVP(key,value,IC,BC,geometry) result(KVP)
         implicit none
         type(preset_ID),intent(in) :: IC,BC
         character(len=*),intent(in) :: key
         logical,intent(in) :: value
         integer,intent(in) :: geometry
         type(key_value_pair) :: KVP
         call init(KVP,key,value,IC,BC,geometry)
       end function

       subroutine set_false_KVP(KVP)
         implicit none
         type(key_value_pair),intent(inout) :: KVP
         KVP%value = .false.
       end subroutine

       function get_value_KVP(KVP) result(v)
         implicit none
         type(key_value_pair),intent(in) :: KVP
         logical :: v
         v = KVP%value
       end function

       function is_key_KVP(KVP,key) result(L)
         implicit none
         type(key_value_pair),intent(in) :: KVP
         character(len=*),intent(in) :: key
         logical :: L
         L = identical(KVP%key,key)
       end function

       end module