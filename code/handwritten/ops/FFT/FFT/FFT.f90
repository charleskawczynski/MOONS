      module fft_mod
      use current_precision_mod
      use ops_fft_mod
      use grid_mod
      implicit none

      private
      public :: FFT
      public :: init,delete,print,display,export,import

      public :: apply

      interface init;    module procedure init_fft;    end interface
      interface delete;  module procedure delete_fft;  end interface
      interface print;   module procedure print_fft;   end interface
      interface display; module procedure display_fft; end interface
      interface export;  module procedure export_fft;  end interface
      interface import;  module procedure import_fft;  end interface

      interface apply;   module procedure apply_fft;   end interface

      type FFT
        type(grid_field_CMP) :: e,tempX
        real(cp),dimension(3) :: dh = 0.0_cp
        integer,dimension(3) :: N = 0
        type(grid_field_CMP),dimension(:),allocatable :: x       ! N
        type(grid_field_CMP),dimension(:),allocatable :: even    ! N/2
        type(grid_field_CMP),dimension(:),allocatable :: odd     ! (N+1)/2
      endtype

      contains

      subroutine init_fft(FFT,x,N,dh)
        implicit none
        type(FFT),intent(inout) :: FFT
        type(grid_field),intent(in) :: N,dh
        integer :: dir,i
        call init(FFT%x,N)
        do dir=1,3
          do i=N,1,-N/2
          call init(FFT%even())
          call init(FFT%tempX,x)
        enddo
      end subroutine

      subroutine delete_fft(FFT)
        implicit none
        type(FFT),intent(inout) :: FFT
        call delete(FFT%e)
        call delete(FFT%tempX)
        FFT%dh = 0.0_cp
      end subroutine

      subroutine display_fft(FFT,un)
        implicit none
        type(FFT),intent(inout) :: FFT
        integer,intent(in) :: un
        call display(FFT%e,un)
        call display(FFT%tempX,un)
        write(un,*) 'FFT%dh = ',FFT%dh
      end subroutine

      subroutine print_fft(FFT)
        implicit none
        type(FFT),intent(inout) :: FFT
        call display(FFT,6)
      end subroutine

      subroutine export_fft(FFT,un)
        implicit none
        type(FFT),intent(inout) :: FFT
        integer,intent(in) :: un
        call export(FFT%e,un)
        call export(FFT%tempX,un)
        write(un,*) 'FFT%dh = '; write(un,*) FFT%dh
      end subroutine

      subroutine import_fft(FFT,un)
        implicit none
        type(FFT),intent(inout) :: FFT
        integer,intent(in) :: un
        call import(FFT%e,un)
        call import(FFT%tempX,un)
        read(un,*); read(un,*) FFT%dh
      end subroutine

      subroutine apply_fft(FFT,x,dir,pad)
        implicit none
        type(FFT),intent(inout) :: FFT
        type(grid_field),intent(inout) :: x
        integer,intent(in) :: dir,pad
        call fft_operate(x,FFT%e,FFT%tempX,dir,pad)
      end subroutine

      end module