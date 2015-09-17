       module monitor_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use SF_mod
       use VF_mod
       use IO_SF_mod
       use IO_VF_mod
       use grid_mod
       use norms_mod
       use ops_aux_mod
       use ops_discrete_mod
       use probe_base_mod
       use probe_transient_mod

       implicit none

       private
       public :: monitor,init,delete
       public :: print,export


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type monitor
         character(len=3) :: name                ! Name of monitor
         type(grid) :: g                         ! Grid
         type(indexProbe),dimension(3) :: p      ! Solution probe vs nstep
         type(probe) :: energy                   ! Transient energy vs nstep
         type(SF) :: div                         ! Divergence of VF
         type(norms) :: norm_div                 ! Norms of divergence
         type(errorProbe) :: p_div               ! Transient divergence vs nstep
       end type

       interface init;     module procedure init_monitor;    end interface
       interface delete;   module procedure delete_monitor;  end interface

       interface print;    module procedure print_monitor;   end interface
       interface export;   module procedure export_monitor;  end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine init_monitor(m,g,U,dir,name,restart)
         implicit none
         type(monitor),intent(inout) :: m
         type(grid),intent(in) :: g
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: dir,name
         logical,intent(in) :: restart
         write(*,*) 'Initializing monitor:'
         m%name = name

         call init(m%p(1),dir,'transient_'//name//'_x',&
         .not.restart,U%x%RF(1)%s,(U%x%RF(1)%s+1)/2*2/3,g)
         call export(m%p(1))
         call init(m%p(2),dir,'transient_'//name//'_y',&
         .not.restart,U%y%RF(1)%s,(U%y%RF(1)%s+1)/2*2/3,g)
         call export(m%p(2))
         call init(m%p(3),dir,'transient_'//name//'_z',&
         .not.restart,U%z%RF(1)%s,(U%z%RF(1)%s+1)/2*2/3,g)
         call export(m%p(3))

         call init(m%energy,dir,'energy'//'_name',.not.restart)

         call init_CC(m%div,g)
         call init(m%p_div,dir,'transient_div_'//name,.not.restart)
         call export(m%p_div)
         call init(m%norm_div)

         write(*,*) '     finished'
       end subroutine

       subroutine delete_monitor(m)
         implicit none
         type(monitor),intent(inout) :: m
         call delete(m%p(1))
         call delete(m%p(2))
         call delete(m%p(3))
         call delete(m%energy)

         call delete(m%div)
         call delete(m%p_div)
       end subroutine

       subroutine print_monitor(m,un,g)
         ! Use un = 6 to print to screen
         implicit none
         type(monitor),intent(in) :: m
         integer,intent(in) :: un
         type(grid),intent(in) :: g
         write(un,*) '**************************************************************'
         write(un,*) '************************** '// m%name //' **************************'
         write(un,*) '**************************************************************'
         write(un,*) ''
         write(un,*) 'N_cells = ',(/g%c(1)%N,g%c(2)%N,g%c(3)%N/)
         write(un,*) 'volume = ',g%volume
         write(un,*) 'min/max(h)_x = ',(/g%c(1)%hmin,g%c(1)%hmax/)
         write(un,*) 'min/max(h)_y = ',(/g%c(2)%hmin,g%c(2)%hmax/)
         write(un,*) 'min/max(h)_z = ',(/g%c(3)%hmin,g%c(3)%hmax/)
         write(un,*) 'min/max(dh)_x = ',(/g%c(1)%dhMin,g%c(1)%dhMax/)
         write(un,*) 'min/max(dh)_y = ',(/g%c(2)%dhMin,g%c(2)%dhMax/)
         write(un,*) 'min/max(dh)_z = ',(/g%c(3)%dhMin,g%c(3)%dhMax/)
         write(un,*) 'stretching_x = ',g%c(1)%dhMax-g%c(1)%dhMin
         write(un,*) 'stretching_y = ',g%c(2)%dhMax-g%c(2)%dhMin
         write(un,*) 'stretching_z = ',g%c(3)%dhMax-g%c(3)%dhMin
         write(un,*) ''
       end subroutine

       subroutine export_monitor(m,g,U,nstep,TF)
         implicit none
         type(monitor),intent(inout) :: m
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         integer,intent(in) :: nstep
         logical,intent(in) :: TF
         if (TF) then
           call apply(m%p(1),nstep,U%x%RF(1)%f)
           call apply(m%p(2),nstep,U%y%RF(1)%f)
           call apply(m%p(3),nstep,U%z%RF(1)%f)
           call div(m%div,U,g)
           call compute(m%norm_div,m%div,g)
           call apply(m%p_div,nstep,m%div,g)
           call printPhysicalMinMax(m%div,'div_'//m%name)
           call compute_energy(m,U,g,nstep,TF)
         endif
       end subroutine

       subroutine compute_energy(m,U,g,nstep,TF)
         implicit none
         type(monitor),intent(inout) :: m
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         integer,intent(in) :: nstep
         logical,intent(in) :: TF
         real(cp) :: K_energy
          if (TF) then
           call totalEnergy(K_energy,U,g)
           call set(m%energy,nstep,K_energy)
           call apply(m%energy)
          endif
       end subroutine


       end module