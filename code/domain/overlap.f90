       module overlap_mod
       implicit none

       private
       public :: overlap
       public :: init,delete,display,print,export,import ! Essentials

       interface init;       module procedure init_overlap;        end interface
       interface init;       module procedure init_copy_overlap;   end interface
       interface delete;     module procedure delete_overlap;      end interface
       interface print;      module procedure print_overlap;       end interface
       interface display;    module procedure display_overlap;     end interface
       interface export;     module procedure export_overlap;      end interface
       interface import;     module procedure import_overlap;      end interface

       type overlap
         ! R1(1) and R1(2) indicate indexes for the start of overlapping regions
         ! R2(1) and R2(2) indicate indexes for the  end  of overlapping regions
         ! For example:
         !                           R1(1)     R2(1)
         !                            |----------------fluid------------------|
         !     |-------------wall---------------|
         !                           R1(2)     R2(2)
         integer,dimension(2) :: R1,R2
       end type

       contains

       subroutine init_overlap(OL,R1,R2)
         implicit none
         type(overlap),intent(inout) :: OL
         integer,dimension(2),intent(in) :: R1,R2
         OL%R1 = R1
         OL%R2 = R2
       end subroutine

       subroutine init_copy_overlap(a,b)
         implicit none
         type(overlap),intent(inout) :: a
         type(overlap),intent(in) :: b
         a%R1 = b%R1
         a%R2 = b%R2
       end subroutine

       subroutine delete_overlap(OL)
         implicit none
         type(overlap),intent(inout) :: OL
         OL%R1 = 0
         OL%R2 = 0
       end subroutine

       subroutine print_overlap(OL)
         implicit none
         type(overlap),intent(in) :: OL
         call display(OL,6)
       end subroutine

       subroutine display_overlap(OL,u)
         implicit none
         type(overlap),intent(in) :: OL
         integer,intent(in) :: u
         write(u,*) 'R1 = ',OL%R1
         write(u,*) 'R2 = ',OL%R2
       end subroutine

       subroutine export_overlap(OL,u)
         implicit none
         type(overlap),intent(in) :: OL
         integer,intent(in) :: u
         write(u,*) 'R1'; write(u,*) OL%R1
         write(u,*) 'R2'; write(u,*) OL%R2
       end subroutine

       subroutine import_overlap(OL,u)
         implicit none
         type(overlap),intent(inout) :: OL
         integer,intent(in) :: u
         read(u,*) ; read(u,*) OL%R1
         read(u,*) ; read(u,*) OL%R2
       end subroutine

       end module