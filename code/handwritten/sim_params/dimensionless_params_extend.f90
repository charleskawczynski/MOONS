       module dimensionless_params_extend_mod
       use current_precision_mod
       use dimensionless_params_mod
       implicit none
       private
       public :: init

       interface init;    module procedure init_dir_name;    end interface

       contains

       subroutine init_dir_name(DP,dir,name)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         character(len=*),intent(in) :: dir,name
         call init(DP%dir,dir)
         call init(DP%name,name)
       end subroutine

       end module