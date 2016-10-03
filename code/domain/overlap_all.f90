       module overlap_mod
       use current_precision_mod
       use coordinates_mod
       implicit none

       private
       public :: overlap
       public :: init,delete,display,print,export,import ! Essentials
       public :: is_overlap,inside,init_props

       interface init;          module procedure init_overlap_all;              end interface
       interface init;          module procedure init_copy_overlap_all;         end interface
       interface delete;        module procedure delete_overlap_all;            end interface
       interface print;         module procedure print_overlap_all;             end interface
       interface display;       module procedure display_overlap_all;           end interface
       interface export;        module procedure export_overlap_all;            end interface
       interface import;        module procedure import_overlap_all;            end interface

       type overlap_all
         type(overlap),dimension(3) :: OL
         integer :: N ! number of iR  = 1 in all 3, which will define which procedure to call
         ! or
         integer :: N ! number of iR != 1 in all 3, which will define which procedure to call
         integer,dimension(3) :: i
         integer,dimension(3) :: iR
       end type

       contains

       subroutine init_overlap_all(OL_all,OL)
         implicit none
         type(overlap_all),intent(inout) :: OL_all
         type(overlap),dimension(3),intent(in) :: OL
         integer :: i
         do i=1,3
         call init(OL_all%OL(i),OL(i))
         enddo
         call init_props(OL_all)
       end subroutine

       subroutine init_copy_overlap_all(a,b)
         implicit none
         type(overlap_all),intent(inout) :: a
         type(overlap_all),intent(in) :: b
         integer :: i
         do i=1,3
         call init(a%OL(i),b%OL(i))
         enddo
         a% = b%
         a% = b%
         call init_props(a)
       end subroutine

       subroutine init_props_OL_all(OL_all)
         implicit none
         type(overlap_all),intent(inout) :: OL_all
         integer :: i
         OL_all%N = count((/(OL_all%OL(i)%iR.eq.1,i=1,3)/))
         
       end subroutine

       subroutine delete_overlap_all(OL_all)
         implicit none
         type(overlap_all),intent(inout) :: OL_all
         integer :: i
         do i=1,3
         call delete(OL_all%OL(i))
         enddo
       end subroutine

       subroutine print_overlap_all(OL_all)
         implicit none
         type(overlap_all),intent(in) :: OL_all
         integer :: i
         do i=1,3
         call print(OL_all%OL(i))
         enddo
       end subroutine

       subroutine display_overlap_all(OL_all,un)
         implicit none
         type(overlap_all),intent(in) :: OL_all
         integer,intent(in) :: un
         integer :: i
         do i=1,3
         call display(OL_all%OL(i),6)
         enddo
       end subroutine

       subroutine export_overlap_all(OL_all,un)
         implicit none
         type(overlap_all),intent(in) :: OL_all
         integer,intent(in) :: un
         integer :: i
         do i=1,3
         call export(OL_all%OL(i),un)
         enddo
       end subroutine

       subroutine import_overlap_all(OL_all,un)
         implicit none
         type(overlap_all),intent(inout) :: OL_all
         integer,intent(in) :: un
         do i=1,3
         call import(OL_all%OL(i),un)
         enddo
       end subroutine

       end module