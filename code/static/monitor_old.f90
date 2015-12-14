       module monitor_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use SF_mod
       use VF_mod
       use IO_SF_mod
       use IO_VF_mod
       use mesh_mod
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
         type(mesh) :: m                         ! Grid
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

       subroutine init_monitor(mon,m,U,dir,name,restart)
         implicit none
         type(monitor),intent(inout) :: mon
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: dir,name
         logical,intent(in) :: restart
         write(*,*) 'Initializing monitor:'
         mon%name = name

         call init(mon%p(1),dir,'transient_'//name//'_x',&
         .not.restart,U%x%RF(1)%s,(U%x%RF(1)%s+1)/2*2/3,m)
         call export(mon%p(1))
         call init(mon%p(2),dir,'transient_'//name//'_y',&
         .not.restart,U%y%RF(1)%s,(U%y%RF(1)%s+1)/2*2/3,m)
         call export(mon%p(2))
         call init(mon%p(3),dir,'transient_'//name//'_z',&
         .not.restart,U%z%RF(1)%s,(U%z%RF(1)%s+1)/2*2/3,m)
         call export(mon%p(3))

         call init(mon%energy,dir,'energy'//'_name',.not.restart)

         call init_CC(mon%div,m)
         call init(mon%p_div,dir,'transient_div_'//name,.not.restart)
         call export(mon%p_div)
         call init(mon%norm_div)

         write(*,*) '     finished'
       end subroutine

       subroutine delete_monitor(mon)
         implicit none
         type(monitor),intent(inout) :: mon
         call delete(mon%p(1))
         call delete(mon%p(2))
         call delete(mon%p(3))
         call delete(mon%energy)

         call delete(mon%div)
         call delete(mon%p_div)
       end subroutine

       subroutine print_monitor(mon,un,m)
         ! Use un = 6 to print to screen
         implicit none
         type(monitor),intent(in) :: mon
         integer,intent(in) :: un
         type(mesh),intent(in) :: m
         write(un,*) '**************************************************************'
         write(un,*) '************************** '// mon%name //' **************************'
         write(un,*) '**************************************************************'
         write(un,*) ''
         write(un,*) 'N_cells = ',m%N_cells
         write(un,*) 'volume = ',m%volume
         write(un,*) 'min/max(h)_x = ',(/m%hmin(1),m%hmax(1)/)
         write(un,*) 'min/max(h)_y = ',(/m%hmin(2),m%hmax(2)/)
         write(un,*) 'min/max(h)_z = ',(/m%hmin(3),m%hmax(3)/)
         write(un,*) 'min/max(dh)_x = ',(/m%dhMin(1),m%dhMax(1)/)
         write(un,*) 'min/max(dh)_y = ',(/m%dhMin(2),m%dhMax(2)/)
         write(un,*) 'min/max(dh)_z = ',(/m%dhMin(3),m%dhMax(3)/)
         write(un,*) 'stretching_x = ',m%dhMax(1)-m%dhMin(1)
         write(un,*) 'stretching_y = ',m%dhMax(2)-m%dhMin(2)
         write(un,*) 'stretching_z = ',m%dhMax(3)-m%dhMin(3)
         write(un,*) ''
       end subroutine

       subroutine export_monitor(mon,m,U,nstep,TF)
         implicit none
         type(monitor),intent(inout) :: mon
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: nstep
         logical,intent(in) :: TF
         if (TF) then
           call apply(mon%p(1),nstep,U%x%RF(1)%f)
           call apply(mon%p(2),nstep,U%y%RF(1)%f)
           call apply(mon%p(3),nstep,U%z%RF(1)%f)
           call div(mon%div,U,m)
           call compute(mon%norm_div,mon%div,m)
           call apply(mon%p_div,nstep,mon%div,m)
           call printPhysicalMinMax(mon%div,'div_'//mon%name)
           call compute_energy(mon,U,m,nstep,TF)
         endif
       end subroutine

       subroutine compute_energy(mon,U,m,nstep,TF)
         implicit none
         type(monitor),intent(inout) :: mon
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: nstep
         logical,intent(in) :: TF
         real(cp) :: K_energy
          if (TF) then
           call totalEnergy(K_energy,U,m)
           call set(mon%energy,nstep,K_energy)
           call apply(mon%energy)
          endif
       end subroutine


       end module