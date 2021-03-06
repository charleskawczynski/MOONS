       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module boundary_mod
       use datatype_conversion_mod
       use single_boundary_mod
       use IO_tools_mod
       use string_mod
       use BC_logicals_mod
       use dir_manip_mod
       implicit none

       private
       public :: boundary
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_boundary;              end interface
       interface delete;                 module procedure delete_boundary;                 end interface
       interface display;                module procedure display_boundary;                end interface
       interface display_short;          module procedure display_short_boundary;          end interface
       interface display;                module procedure display_wrap_boundary;           end interface
       interface print;                  module procedure print_boundary;                  end interface
       interface print_short;            module procedure print_short_boundary;            end interface
       interface export;                 module procedure export_boundary;                 end interface
       interface export_primitives;      module procedure export_primitives_boundary;      end interface
       interface import;                 module procedure import_boundary;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_boundary;end interface
       interface export_structured;      module procedure export_structured_D_boundary;    end interface
       interface import_structured;      module procedure import_structured_D_boundary;    end interface
       interface import_primitives;      module procedure import_primitives_boundary;      end interface
       interface export;                 module procedure export_wrap_boundary;            end interface
       interface import;                 module procedure import_wrap_boundary;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_boundary;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_boundary;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_boundary;      end interface

       type boundary
         type(BC_logicals) :: BCL
         integer :: n = 0
         type(single_boundary),dimension(:),allocatable :: SB
         type(string) :: name
       end type

       contains

       subroutine init_copy_boundary(this,that)
         implicit none
         type(boundary),intent(inout) :: this
         type(boundary),intent(in) :: that
         integer :: i_SB
         integer :: s_SB
         call delete(this)
         call init(this%BCL,that%BCL)
         this%n = that%n
         if (allocated(that%SB)) then
           s_SB = size(that%SB)
           if (s_SB.gt.0) then
             allocate(this%SB(s_SB))
             do i_SB=1,s_SB
               call init(this%SB(i_SB),that%SB(i_SB))
             enddo
           endif
         endif
         call init(this%name,that%name)
       end subroutine

       subroutine delete_boundary(this)
         implicit none
         type(boundary),intent(inout) :: this
         integer :: i_SB
         integer :: s_SB
         call delete(this%BCL)
         this%n = 0
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call delete(this%SB(i_SB))
           enddo
           deallocate(this%SB)
         endif
         call delete(this%name)
       end subroutine

       subroutine display_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         call display(this%BCL,un)
         write(un,*) 'n    = ',this%n
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call display(this%SB(i_SB),un)
           enddo
         endif
         call display(this%name,un)
       end subroutine

       subroutine display_short_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         call display(this%BCL,un)
         write(un,*) 'n    = ',this%n
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call display(this%SB(i_SB),un)
           enddo
         endif
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_boundary(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_boundary(this)
         implicit none
         type(boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_boundary(this)
         implicit none
         type(boundary),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         call export_primitives(this,un)
         call export(this%BCL,un)
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           write(un,*) s_SB
           if (s_SB.gt.0) then
             do i_SB=1,s_SB
               call export(this%SB(i_SB),un)
             enddo
           else
             write(un,*) 0
           endif
         endif
         call export(this%name,un)
       end subroutine

       subroutine import_boundary(this,un)
         implicit none
         type(boundary),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         call delete(this)
         call import_primitives(this,un)
         call import(this%BCL,un)
         read(un,*) s_SB
         if (s_SB.gt.0) then
           allocate(this%SB(s_SB))
           do i_SB=1,s_SB
             call import(this%SB(i_SB),un)
           enddo
         endif
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n     = ';write(un,*) this%n
       end subroutine

       subroutine import_primitives_boundary(this,un)
         implicit none
         type(boundary),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%n
       end subroutine

       subroutine export_wrap_boundary(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_boundary(this,dir,name)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_boundary(this,dir)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SB
         integer :: s_SB
         call suppress_warnings(this)
         call set_IO_dir(this%BCL,dir//'BCL'//fortran_PS)
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call set_IO_dir(this%SB(i_SB),&
             dir//'SB_'//int2str(i_SB)//fortran_PS)
           enddo
         endif
         call set_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_boundary(this,dir)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SB
         integer :: s_SB
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%BCL,dir//'BCL'//fortran_PS)
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call make_IO_dir(this%SB(i_SB),&
             dir//'SB_'//int2str(i_SB)//fortran_PS)
           enddo
         endif
         call make_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_boundary(this,dir)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SB
         integer :: s_SB
         integer :: un
         call export_structured(this%BCL,dir//'BCL'//fortran_PS)
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           write(un,*) s_SB
           do i_SB=1,s_SB
             call export_structured(this%SB(i_SB),&
             dir//'SB_'//int2str(i_SB)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
         call export_structured(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine export_structured_D_boundary(this,dir)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SB
         integer :: s_SB
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%BCL,dir//'BCL'//fortran_PS)
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           write(un,*) s_SB
           do i_SB=1,s_SB
             call export_structured(this%SB(i_SB),&
             dir//'SB_'//int2str(i_SB)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
         call export_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_boundary(this,dir)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SB
         integer :: s_SB
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%BCL,dir//'BCL'//fortran_PS)
         read(un,*) s_SB
         if (s_SB.gt.0) then
           if (.not.allocated(this%SB)) then
             allocate(this%SB(s_SB))
           endif
           do i_SB=1,s_SB
             call import_structured(this%SB(i_SB),&
             dir//'SB_'//int2str(i_SB)//fortran_PS)
           enddo
         endif
         call import_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_boundary(this)
         implicit none
         type(boundary),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module