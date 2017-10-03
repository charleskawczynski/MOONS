       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module SF_mod
       use current_precision_mod
       use IO_tools_mod
       use data_location_mod
       use block_field_mod
       use block_field_extend_mod
       use data_location_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: SF
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_SF;          end interface
       interface delete;           module procedure delete_SF;             end interface
       interface display;          module procedure display_SF;            end interface
       interface display_short;    module procedure display_short_SF;      end interface
       interface display;          module procedure display_wrap_SF;       end interface
       interface print;            module procedure print_SF;              end interface
       interface print_short;      module procedure print_short_SF;        end interface
       interface export;           module procedure export_SF;             end interface
       interface export_primitives;module procedure export_primitives_SF;  end interface
       interface import;           module procedure import_SF;             end interface
       interface export_structured;module procedure export_structured_D_SF;end interface
       interface import_structured;module procedure import_structured_D_SF;end interface
       interface import_primitives;module procedure import_primitives_SF;  end interface
       interface export;           module procedure export_wrap_SF;        end interface
       interface import;           module procedure import_wrap_SF;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_SF;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_SF;        end interface
       interface suppress_warnings;module procedure suppress_warnings_SF;  end interface

       type SF
         integer :: s = 0
         type(block_field),dimension(:),allocatable :: BF
         logical :: all_neumann = .false.
         integer :: numEl = 0
         integer :: numPhysEl = 0
         real(cp) :: vol = 0.0_cp
         type(data_location) :: DL
       end type

       contains

       subroutine init_copy_SF(this,that)
         implicit none
         type(SF),intent(inout) :: this
         type(SF),intent(in) :: that
         integer :: i_BF
         integer :: s_BF
         call delete(this)
         this%s = that%s
         if (allocated(that%BF)) then
           s_BF = size(that%BF)
           if (s_BF.gt.0) then
             allocate(this%BF(s_BF))
             do i_BF=1,s_BF
               call init(this%BF(i_BF),that%BF(i_BF))
             enddo
           endif
         endif
         this%all_neumann = that%all_neumann
         this%numEl = that%numEl
         this%numPhysEl = that%numPhysEl
         this%vol = that%vol
         call init(this%DL,that%DL)
       end subroutine

       subroutine delete_SF(this)
         implicit none
         type(SF),intent(inout) :: this
         integer :: i_BF
         integer :: s_BF
         this%s = 0
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call delete(this%BF(i_BF))
           enddo
           deallocate(this%BF)
         endif
         this%all_neumann = .false.
         this%numEl = 0
         this%numPhysEl = 0
         this%vol = 0.0_cp
         call delete(this%DL)
       end subroutine

       subroutine display_SF(this,un)
         implicit none
         type(SF),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         write(un,*) 's           = ',this%s
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call display(this%BF(i_BF),un)
           enddo
         endif
         write(un,*) 'all_neumann = ',this%all_neumann
         write(un,*) 'numEl       = ',this%numEl
         write(un,*) 'numPhysEl   = ',this%numPhysEl
         write(un,*) 'vol         = ',this%vol
         call display(this%DL,un)
       end subroutine

       subroutine display_short_SF(this,un)
         implicit none
         type(SF),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         write(un,*) 's           = ',this%s
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call display(this%BF(i_BF),un)
           enddo
         endif
         write(un,*) 'all_neumann = ',this%all_neumann
         write(un,*) 'numEl       = ',this%numEl
         write(un,*) 'numPhysEl   = ',this%numPhysEl
         write(un,*) 'vol         = ',this%vol
         call display(this%DL,un)
       end subroutine

       subroutine display_wrap_SF(this,dir,name)
         implicit none
         type(SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_SF(this)
         implicit none
         type(SF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_SF(this)
         implicit none
         type(SF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_SF(this,un)
         implicit none
         type(SF),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         write(un,*) 's            = ';write(un,*) this%s
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           write(un,*) s_BF
           do i_BF=1,s_BF
             call export(this%BF(i_BF),un)
           enddo
         endif
         write(un,*) 'all_neumann  = ';write(un,*) this%all_neumann
         write(un,*) 'numEl        = ';write(un,*) this%numEl
         write(un,*) 'numPhysEl    = ';write(un,*) this%numPhysEl
         write(un,*) 'vol          = ';write(un,*) this%vol
         call export(this%DL,un)
       end subroutine

       subroutine import_SF(this,un)
         implicit none
         type(SF),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_BF
         integer :: s_BF
         call delete(this)
         read(un,*); read(un,*) this%s
         if (allocated(this%BF)) then
           read(un,*) s_BF
           if (s_BF.gt.0) then
             do i_BF=1,s_BF
               call import(this%BF(i_BF),un)
             enddo
           endif
         endif
         read(un,*); read(un,*) this%all_neumann
         read(un,*); read(un,*) this%numEl
         read(un,*); read(un,*) this%numPhysEl
         read(un,*); read(un,*) this%vol
         call import(this%DL,un)
       end subroutine

       subroutine export_primitives_SF(this,un)
         implicit none
         type(SF),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 's            = ';write(un,*) this%s
         write(un,*) 'all_neumann  = ';write(un,*) this%all_neumann
         write(un,*) 'numEl        = ';write(un,*) this%numEl
         write(un,*) 'numPhysEl    = ';write(un,*) this%numPhysEl
         write(un,*) 'vol          = ';write(un,*) this%vol
       end subroutine

       subroutine import_primitives_SF(this,un)
         implicit none
         type(SF),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%all_neumann
         read(un,*); read(un,*) this%numEl
         read(un,*); read(un,*) this%numPhysEl
         read(un,*); read(un,*) this%vol
       end subroutine

       subroutine export_wrap_SF(this,dir,name)
         implicit none
         type(SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_SF(this,dir,name)
         implicit none
         type(SF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_SF(this,dir)
         implicit none
         type(SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_BF
         integer :: s_BF
         call suppress_warnings(this)
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call set_IO_dir(this%BF(i_BF),&
             dir//'BF_'//int2str(i_BF)//fortran_PS)
           enddo
         endif
         call set_IO_dir(this%DL,dir//'DL'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_SF(this,dir)
         implicit none
         type(SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_BF
         integer :: s_BF
         call suppress_warnings(this)
         call make_dir(dir)
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call make_IO_dir(this%BF(i_BF),&
             dir//'BF_'//int2str(i_BF)//fortran_PS)
           enddo
         endif
         call make_IO_dir(this%DL,dir//'DL'//fortran_PS)
       end subroutine

       subroutine export_structured_D_SF(this,dir)
         implicit none
         type(SF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_BF
         integer :: s_BF
         integer :: un
         write(*,*) 'Exporting SF structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call export_structured(this%BF(i_BF),&
             dir//'BF_'//int2str(i_BF)//fortran_PS)
           enddo
         endif
         call export_structured(this%DL,dir//'DL'//fortran_PS)
       end subroutine

       subroutine import_structured_D_SF(this,dir)
         implicit none
         type(SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_BF
         integer :: s_BF
         integer :: un
         write(*,*) 'Importing SF structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call import_structured(this%BF(i_BF),&
             dir//'BF_'//int2str(i_BF)//fortran_PS)
           enddo
         endif
         call import_structured(this%DL,dir//'DL'//fortran_PS)
         if (allocated(this%BF)) then
           s_BF = size(this%BF)
           do i_BF=1,s_BF
             call set_procedures(this%BF(i_BF))
           enddo
         endif
       end subroutine

       subroutine suppress_warnings_SF(this)
         implicit none
         type(SF),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module