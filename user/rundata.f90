       module rundata_mod
       use simParams_mod
       use grid_mod
       use myError_mod
       use myTime_mod
       use IO_tools_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
       real(cp),parameter :: zero = real(0.0,cp)
       real(cp),parameter :: one = real(1.0,cp)
       real(cp),parameter :: two = real(2.0,cp)

       private
       
       character(len=5),parameter :: hfmt = 'F15.6'

       public :: rundata,setRundata
       public :: setDtime,getDtime,getDPseudoTime
       public :: getFo,updateTime
       public :: getRe,getHa,getRem
       public :: getInteractionNumber
       public :: printRundata
       public :: exportRundata

       type rundata
         private
         ! Characteristic Scales
         real(cp) :: t_c          ! Characteristic time (advective)
         real(cp) :: U_c          ! Characteristic velocity
         real(cp) :: L_c          ! Characteristic length
         real(cp) :: t_nu         ! Characteristic time (momentum diffusion)
         real(cp) :: t_eta        ! Characteristic time (magnetic diffusion)
         real(cp) :: dtime        ! Time step (dimensionless)
         real(cp) :: ds           ! Pseudo time step for low Rem induction equation
         real(cp) :: time         ! Time (simulation)
         ! Dimensionless Parameters
         real(cp) :: Re           ! Reynolds number
         real(cp) :: nu           ! Viscosity ( = 1/Re , dimensionless )
         real(cp) :: Ha           ! Hartmann number
         real(cp) :: N            ! Interaction number
         real(cp) :: Rem          ! Magnetic Reynolds number
         real(cp) :: eta          ! Resistivity ( = 1/Rem , dimensionless )
         real(cp) :: Pr_m         ! Magnetic Prandtl number
         ! Stability
         real(cp) :: Fo           ! Fourier number
         real(cp) :: Co           ! Courant number
         real(cp) :: Fo_m         ! Magnetic Fourier number
         real(cp) :: u_grid       ! Grid velocity
         real(cp) :: nu_grid      ! Grid viscosity
         real(cp) :: Re_grid      ! Grid Reynolds number
         real(cp) :: Rem_grid     ! Magnetic Grid Reynolds number
         real(cp) :: dhMin,dhiMin ! Smallest spatial steps
         real(cp) :: umax         ! Maximum value of velocity component
         ! Solution Parameters
         logical :: solveCoupled   ! Coupled momentum / induction (T/F)
         integer :: solveBMethod   ! Method for solving for B-Field
       end type

       interface setDtime;    module procedure setDtimeRundata;    end interface

       contains

       subroutine setRundata(this,dtime,ds,Re,Ha,Rem,&
         u,v,w,g_mom,g_ind,solveCoupled,solveBMethod)
         implicit none
         type(rundata),intent(inout) :: this
         real(cp), intent(in) :: dtime,ds,Re,Ha,Rem
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid), intent(in) :: g_mom,g_ind
         logical, intent(in) :: solveCoupled
         integer, intent(in) :: solveBMethod
         real(cp) :: tempMax1,tempMax2,tempMax3


         ! *************** DIMENSIONLESS PARAMETERS **************
         this%Re = Re
         this%Ha = Ha
         this%N = Ha**2.0d0/Re
         if (solveBMethod.eq.1) then
           this%Rem = 1.0d-4
         else
           this%Rem = Rem
         endif
         this%nu = one/this%Re
         this%eta = one/this%Rem

         ! *************** CHARACTERISTIC SCALES *****************
         ! Right now, the characteristic length and velocity
         ! scales are based on the max length of the interior 
         ! domain and the face velocity of the interior domain.
         ! This is not true in general and must be adjusted by
         ! the user.

         this%L_c = 0.5d0*g_mom%maxRange

         ! Is this even necessary? I think setting U_c = 1
         ! might be better since velocity is basically controlled
         ! by Re. Any BCs should be somehow normalized to 1
         ! anyway.
         ! 
         ! tempMax1 = maxval(abs(u(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3))))
         ! tempMax2 = maxval(abs(v(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nice1(3):Nice2(3))))
         ! tempMax3 = maxval(abs(u(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nin1(3):Nin2(3))))
         ! this%umax = maxval((/tempMax1,tempmax2,tempmax3/))
         ! this%U_c = this%umax

         tempmax1 = maxval(abs(u))
         tempmax2 = maxval(abs(v))
         tempmax3 = maxval(abs(w))
         this%umax = 0.5d0*maxval((/tempMax1,tempmax2,tempmax3/))
         this%U_c = this%umax

         ! ******************** TIME SCALES **********************
         ! Advective time
         this%t_c = this%L_c/this%U_c
         ! Momentum diffusion time
         this%t_nu = this%L_c**2.0d0/this%nu
         ! Magnetic diffusion time
         this%t_eta = this%L_c**2.0d0/this%eta
         ! Dimensionless time step
         this%dtime = dtime
         this%ds = ds
         ! Time
         this%time = zero

         ! ********************* STABILITY ***********************

         this%dhMin = g_ind%dhMin
         this%dhiMin = g_mom%dhMin

         ! Grid velocity
         this%u_grid = this%dhiMin/this%dtime
         ! Grid viscosity (interior)
         this%nu_grid = this%dhiMin**two/this%dtime

         ! Courant number
         this%Co = this%umax/this%u_grid
         ! Fourier number
         this%Fo = this%nu/this%nu_grid
         ! Magnetic Fourier number
         this%Fo_m = this%dtime*this%eta/this%dhMin**2.0d0
         ! Magnetic Fourier number
         this%Fo = this%dtime*this%nu/this%dhMin**2.0d0
         ! Grid Reynolds number
         this%Re_grid = this%Co/this%Fo
         ! Magnetic Prandtl number
         this%Pr_m = this%Rem/this%Re
         ! Magnetic Grid Reynolds number
         this%Rem_grid = this%Co/this%Fo_m

         ! ***************** SOLUTION PARAMETERS *****************
         this%solveBMethod = solveBMethod
         this%solveCoupled = solveCoupled
       end subroutine

       subroutine updateTime(rd,time)
         implicit none
         type(rundata),intent(inout) :: rd
         real(cp), intent(in) :: time
         rd%time = time + rd%dTime
       end subroutine

       function getDtime(this) result(dtime)
         ! This is the dimensionless time, that is 
         ! used in ALL equations.
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: dtime
         dtime = this%dtime
       end function

       function getDPseudoTime(this) result(ds)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: ds
         ds = this%ds
       end function

       function getFo(this) result(Fo)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: Fo
         Fo = this%Fo
       end function

       function getRe(this) result(Re)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: Re
         Re = this%Re
       end function

       function getHa(this) result(Ha)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: Ha
         Ha = this%Ha
       end function

       subroutine setDtimeRundata(this,dtime)
         implicit none
         type(rundata),intent(inout) :: this
         real(cp),intent(in) :: dtime
         this%dtime = dtime
       end subroutine

       function getInteractionNumber(this) result(N)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: N
         N = this%N
       end function

       function getRem(this) result(Rem)
         implicit none
         type(rundata),intent(in) :: this
         real(cp) :: Rem
         Rem = this%Rem
       end function

       subroutine exportRundata(this,dir)
         implicit none
         type(rundata),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: NewU

         NewU = newAndOpen(dir//'parameters/','Rundata')
         call writeRundataToFileOrScreen(this,newU)
         close(NewU)
         call closeAndMessage(newU,'Rundata',dir)
       end subroutine

       subroutine printRundata(this)
         implicit none
         type(rundata),intent(in) :: this
         call writeRundataToFileOrScreen(this,6)
       end subroutine

       subroutine writeRundataToFileOrScreen(this,newU)
         implicit none
         type(rundata), intent(in) :: this
         integer,intent(in) :: NewU
         ! write(newU,*) ''
         ! write(newU,*) ' ******************* RUNDATA ******************* '
         ! write(newU,*) ' ------------ CHARACTERISTIC SCALES ------------ '
         ! write(newU,*) 'L_c,Uc = ',this%L_c,this%U_c
         ! write(newU,*) 't_c,t_nu,t_eta = ',this%t_c,this%t_nu,this%t_eta
         ! write(newU,*) ' ---------- DIMENSIONLESS PARAMETERS ----------- '
         ! write(newU,*) 'Re,nu = ',this%Re,this%nu
         ! write(newU,*) 'Ha,N = ',this%Ha,this%N
         ! if (solveBMethod.eq.1) then
         !   write(newU,*) 'Rem,eta (low Rem) = ',this%Rem,this%eta
         ! else
         !   write(newU,*) 'Rem,eta = ',this%Rem,this%eta
         ! endif
         ! write(newU,*) 'Pr_m = ',this%Pr_m
         ! write(newU,*) ' ------------------ STABILITY ------------------ '
         ! write(newU,*) 'Fo,Co,Re_grid = ',this%Fo,this%Co,this%Re_grid
         ! write(newU,*) 'Fo_m,Rem_grid = ',this%Fo_m,this%Rem_grid
         ! write(newU,*) 'dhMin,dhiMin = ',this%dhMin,this%dhiMin
         ! write(newU,*) 'umax = ',this%umax
         ! write(newU,*) 'dtime = ',this%dtime
         ! write(newU,*) '---------SIMULATION PARAMETERS--------'
         ! write(newU,*) 'solveCoupled = ',this%solveCoupled
         ! write(newU,*) 'solveBMethod = ',this%solveBMethod
         ! write(newU,*) ''

         write(newU,*) ''
         write(newU,*) ' ******************* RUNDATA ******************* '
         write(newU,*) ' ------------ CHARACTERISTIC SCALES ------------ '
         write(newU,'(A10,2'//hfmt//')') ' L_c,Uc = ',this%L_c,this%U_c
         write(newU,'(A18,3'//hfmt//')') ' t_c,t_nu,t_eta = ',this%t_c,this%t_nu,this%t_eta
         write(newU,*) ' ---------- DIMENSIONLESS PARAMETERS ----------- '
         write(newU,'(A9,2'//hfmt//')') ' Re,nu = ',this%Re,this%nu
         write(newU,'(A8,2'//hfmt//')') ' Ha,N = ',this%Ha,this%N
         if (solveBMethod.eq.1) then
           write(newU,'(A21,2'//hfmt//')') ' Rem,eta (low Rem) = ',this%Rem,this%eta
         else
           write(newU,'(A11,2'//hfmt//')') ' Rem,eta = ',this%Rem,this%eta
         endif
         write(newU,'(A8,1'//hfmt//')') ' Pr_m = ',this%Pr_m
         write(newU,*) ' ------------------ STABILITY ------------------ '
         write(newU,'(A18,2'//hfmt//')') ' u_grid,nu_grid = ',this%u_grid,this%nu_grid
         write(newU,'(A17,3'//hfmt//')') ' Fo,Co,Re_grid = ',this%Fo,this%Co,this%Re_grid
         write(newU,'(A17,2'//hfmt//')') ' Fo_m,Rem_grid = ',this%Fo_m,this%Rem_grid
         write(newU,'(A16,2'//hfmt//')') ' dhMin,dhiMin = ',this%dhMin,this%dhiMin
         write(newU,'(A8,1'//hfmt//')') ' umax = ',this%umax
         write(newU,*) 'dtime = ',this%dtime
         write(newU,*) '---------SIMULATION PARAMETERS--------'
         write(newU,'(A16,1L1)') ' solveCoupled = ',this%solveCoupled
         write(newU,'(A16,1I1)') ' solveBMethod = ',this%solveBMethod
       end subroutine

       end module rundata_mod