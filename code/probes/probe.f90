       module probe_mod
       ! Implementation:
       !       type(probe) :: p
       !       call init(p,dir,name,restart)
       !       do i=1,1000
       !         call export(p,n,t,d)      ! sets data to be exported
       !       enddo
       use current_precision_mod
       use string_mod
       use array_mod
       use IO_tools_mod
       use sim_params_mod
       use fmt_mod
       use time_marching_params_mod
       use is_nan_mod

       implicit none

       private
       public :: probe
       public :: init,delete,export
       public :: steady,steady_final

       type probe
         type(string) :: dir,name                              ! directory and name
         real(cp) :: d                                         ! data
         logical,dimension(:),allocatable :: SS_reached        ! d(data)/dt < tol history
         logical,dimension(:),allocatable :: SS_reached_final  ! d(data)/dt < tol history-finest mesh
         real(cp) :: d_data_dt                                 ! change in data over time (Euler)
         real(cp) :: t                                         ! time
         real(cp) :: dt                                        ! delta time
         integer :: un                                         ! file unit
         logical :: restart = .false.                          ! restart probe (append existing)
         real(cp) :: NaN,infinity                              ! for checking divergent data
         real(cp) :: SS_tol                                    ! tolerance for checking for SS
         real(cp) :: SS_tol_final                              ! tolerance for checking for SS
         integer :: n_history                                  ! number of points to check SS
       end type

       interface init;        module procedure init_probe;           end interface
       interface delete;      module procedure delete_probe;         end interface
       interface delete;      module procedure delete_probe_many;    end interface
       interface export;      module procedure export_probe;         end interface
       interface import;      module procedure import_probe;         end interface

       interface export;      module procedure export_probe_data;    end interface
       interface steady;      module procedure steady_probe;         end interface
       interface steady_final;module procedure steady_final_probe;   end interface

       contains

       subroutine init_probe(p,dir,name,restart,SP)
         implicit none
         type(probe),intent(inout) :: p
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         logical,intent(in) :: restart
         type(sim_params),intent(in) :: SP
         call delete(p)
         call init(p%dir,dir)
         call init(p%name,name)
         p%restart = restart
         if (.not.p%restart) then
           p%un = new_and_open(dir,name)
           write(p%un,*) 'TITLE = "'//name//' probe"'
           write(p%un,*) 'VARIABLES = t,'//name//',d('//name//')/dt,|d('//name//')/dt|'
           write(p%un,*) 'ZONE DATAPACKING = POINT'
           flush(p%un)
         elseif (p%restart) then
           p%un = open_to_append(dir,name)
         else; stop 'Error: no case found in init_probe in probe.f90'
         endif
         p%n_history = SP%n_history
         p%SS_tol = SP%SS_tol
         p%SS_tol_final = SP%SS_tol_final
         allocate(p%SS_reached(SP%n_history)); p%SS_reached = .false.
         allocate(p%SS_reached_final(SP%n_history)); p%SS_reached_final = .false.
         p%infinity = huge(1.0_cp)
       end subroutine

       subroutine delete_probe(p)
         implicit none
         type(probe),intent(inout) :: p
         if (allocated(p%SS_reached)) deallocate(p%SS_reached)
         if (allocated(p%SS_reached_final)) deallocate(p%SS_reached_final)
         call delete(p%dir)
         call delete(p%name)
         p%n_history = 0
         p%SS_tol = 0.0_cp
         p%SS_tol_final = 0.0_cp
         close(p%un)
       end subroutine

       subroutine delete_probe_many(p)
         implicit none
         type(probe),dimension(:),intent(inout) :: p
         integer :: i,s
         s = size(p)
         if (s.gt.0) then; do i=1,s; call delete(p(i)); enddo; endif
       end subroutine

       subroutine export_probe(p,un)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: un
         integer :: i
         write(un,*) p%n_history
         do i=1,p%n_history; write(un,*) p%SS_reached(i); enddo
         do i=1,p%n_history; write(un,*) p%SS_reached_final(i); enddo
         call export(p%dir,un)
         call export(p%name,un)
       end subroutine

       subroutine import_probe(p,un)
         implicit none
         type(probe),intent(inout) :: p
         integer,intent(in) :: un
         integer :: i
         read(un,*) p%n_history
         do i=1,p%n_history; read(un,*) p%SS_reached(i); enddo
         do i=1,p%n_history; read(un,*) p%SS_reached_final(i); enddo
         call import(p%dir,un)
         call import(p%name,un)
       end subroutine

       subroutine export_probe_data(p,TMP,d)
         implicit none
         type(probe),intent(inout) :: p
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: d
         integer :: i
         do i=1,p%n_history-1
         p%SS_reached(i) = p%SS_reached(i+1)
         p%SS_reached_final(i) = p%SS_reached_final(i+1)
         enddo

         p%d_data_dt = (d - p%d)/(TMP%t - p%t) ! Breaks if double exported data (TMP%t = p%t)
         p%SS_reached(p%n_history) = abs(p%d_data_dt).lt.p%SS_tol
         p%SS_reached_final(p%n_history) = abs(p%d_data_dt).lt.p%SS_tol_final
         p%t = TMP%t
         p%d = d
         if (p%d.gt.p%infinity) then
           write(*,*) 'Error: data>infinity in probe: ',str(p%name)
           write(*,*) 'data = ',p%d
           stop 'Divergence error. Sorry!'
         endif
         if (is_nan(p%d)) then
           write(*,*) 'Error: NaN in data in probe: ',str(p%name)
           write(*,*) 'data = ',p%d
           stop 'Divergence error. Sorry!'
         endif
         if (is_nan(p%d_data_dt)) p%d_data_dt = 0.0_cp
         if (p%d_data_dt.gt.p%infinity) p%d_data_dt = 0.0_cp
         write(p%un,*) p%t,p%d,p%d_data_dt,abs(p%d_data_dt)
         flush(p%un)
       end subroutine

       function steady_probe(p) result(L)
         implicit none
         type(probe),intent(in) :: p
         logical :: L
         L = all(p%SS_reached)
       end function

       function steady_final_probe(p) result(L)
         implicit none
         type(probe),intent(in) :: p
         logical :: L
         integer :: i
         L = all(p%SS_reached_final)
       end function

       end module