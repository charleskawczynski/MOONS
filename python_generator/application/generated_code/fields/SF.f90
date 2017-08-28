       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sf_mod
       use current_precision_mod
       use IO_tools_mod
       use block_field_mod
       use data_location_mod
       implicit none

       private
       public :: sf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_sf;           end interface
       interface delete; module procedure delete_sf;         end interface
       interface display;module procedure display_sf;        end interface
       interface display;module procedure display_wrapper_sf;end interface
       interface print;  module procedure print_sf;          end interface
       interface export; module procedure export_sf;         end interface
       interface import; module procedure import_sf;         end interface
       interface export; module procedure export_wrapper_sf; end interface
       interface import; module procedure import_wrapper_sf; end interface

       type sf
         type(block_field),dimension(:),allocatable :: bf
         type(data_location) :: dl
         logical :: all_neumann = .false.
         integer :: s = 0
         integer :: numel = 0
         integer :: numphysel = 0
         real(cp) :: vol = 0.0_cp
       end type

       contains

       subroutine init_sf(this,that)
         implicit none
         type(sf),intent(inout) :: this
         type(sf),intent(in) :: that
         integer :: i_bf
         integer :: s_bf
         call delete(this)
         if (allocated(that%bf)) then
           s_bf = size(that%bf)
           if (s_bf.gt.0) then
             allocate(this%bf(s_bf))
             do i_bf=1,s_bf
               call init(this%bf(i_bf),that%bf(i_bf))
             enddo
           endif
         endif
         call init(this%dl,that%dl)
         this%all_neumann = that%all_neumann
         this%s = that%s
         this%numel = that%numel
         this%numphysel = that%numphysel
         this%vol = that%vol
       end subroutine

       subroutine delete_sf(this)
         implicit none
         type(sf),intent(inout) :: this
         integer :: i_bf
         integer :: s_bf
         if (allocated(this%bf)) then
           s_bf = size(this%bf)
           do i_bf=1,s_bf
             call delete(this%bf(i_bf))
           enddo
           deallocate(this%bf)
         endif
         call delete(this%dl)
         this%all_neumann = .false.
         this%s = 0
         this%numel = 0
         this%numphysel = 0
         this%vol = 0.0_cp
       end subroutine

       subroutine display_sf(this,un)
         implicit none
         type(sf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- sf'
         integer :: i_bf
         integer :: s_bf
         if (allocated(this%bf)) then
           s_bf = size(this%bf)
           do i_bf=1,s_bf
             call display(this%bf(i_bf),un)
           enddo
         endif
         call display(this%dl,un)
         write(un,*) 'all_neumann = ',this%all_neumann
         write(un,*) 's           = ',this%s
         write(un,*) 'numel       = ',this%numel
         write(un,*) 'numphysel   = ',this%numphysel
         write(un,*) 'vol         = ',this%vol
       end subroutine

       subroutine print_sf(this)
         implicit none
         type(sf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_sf(this,un)
         implicit none
         type(sf),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_bf
         integer :: s_bf
         if (allocated(this%bf)) then
           s_bf = size(this%bf)
           write(un,*) s_bf
           do i_bf=1,s_bf
             call export(this%bf(i_bf),un)
           enddo
         endif
         call export(this%dl,un)
         write(un,*) 'all_neumann  = ';write(un,*) this%all_neumann
         write(un,*) 's            = ';write(un,*) this%s
         write(un,*) 'numel        = ';write(un,*) this%numel
         write(un,*) 'numphysel    = ';write(un,*) this%numphysel
         write(un,*) 'vol          = ';write(un,*) this%vol
       end subroutine

       subroutine import_sf(this,un)
         implicit none
         type(sf),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_bf
         integer :: s_bf
         call delete(this)
         if (allocated(this%bf)) then
           read(un,*) s_bf
           do i_bf=1,s_bf
             call import(this%bf(i_bf),un)
           enddo
         endif
         call import(this%dl,un)
         read(un,*); read(un,*) this%all_neumann
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%numel
         read(un,*); read(un,*) this%numphysel
         read(un,*); read(un,*) this%vol
       end subroutine

       subroutine display_wrapper_sf(this,dir,name)
         implicit none
         type(sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_sf(this,dir,name)
         implicit none
         type(sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_sf(this,dir,name)
         implicit none
         type(sf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module