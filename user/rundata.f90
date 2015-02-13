       module rundata_mod
       use constants_mod
       use simParams_mod
       use griddata_mod
       use myError_mod
       use myTime_mod
       use myIO_mod
       implicit none

       ! Fixes / Improvements: 
       private
       
       ! real(dpn),parameter :: Re = 100.0d0
       ! real(dpn),parameter :: Ha = 100.0d0
       ! real(dpn),parameter :: Rem = 100.0d0
       ! real(dpn),parameter :: dTime = 100.0d0

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
         real(dpn) :: t_c          ! Characteristic time (advective)
         real(dpn) :: U_c          ! Characteristic velocity
         real(dpn) :: L_c          ! Characteristic length
         real(dpn) :: t_nu         ! Characteristic time (momentum diffusion)
         real(dpn) :: t_eta        ! Characteristic time (magnetic diffusion)
         real(dpn) :: dtime        ! Time step (dimensionless)
         real(dpn) :: ds           ! Pseudo time step for low Rem induction equation
         real(dpn) :: time         ! Time (simulation)
         ! Dimensionless Parameters
         real(dpn) :: Re           ! Reynolds number
         real(dpn) :: nu           ! Viscosity ( = 1/Re , dimensionless )
         real(dpn) :: Ha           ! Hartmann number
         real(dpn) :: N            ! Interaction number
         real(dpn) :: Rem          ! Magnetic Reynolds number
         real(dpn) :: eta          ! Resistivity ( = 1/Rem , dimensionless )
         real(dpn) :: Pr_m         ! Magnetic Prandtl number
         ! Stability
         real(dpn) :: Fo           ! Fourier number
         real(dpn) :: Co           ! Courant number
         real(dpn) :: Fo_m         ! Magnetic Fourier number
         real(dpn) :: u_grid       ! Grid velocity
         real(dpn) :: nu_grid      ! Grid viscosity
         real(dpn) :: Re_grid      ! Grid Reynolds number
         real(dpn) :: Rem_grid     ! Magnetic Grid Reynolds number
         real(dpn) :: dhMin,dhiMin ! Smallest spatial steps
         real(dpn) :: umax         ! Maximum value of velocity component
         ! Solution Parameters
         logical :: solveCoupled   ! Coupled momentum / induction (T/F)
         integer :: solveBMethod   ! Method for solving for B-Field
       end type

       contains

       subroutine setRundata(this,dtime,ds,Re,Ha,Rem,&
         u,v,w,gd,solveCoupled,solveBMethod)
         implicit none
         type(rundata),intent(inout) :: this
         real(dpn), intent(in) :: dtime,ds,Re,Ha,Rem
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(griddata), intent(in) :: gd
         logical, intent(in) :: solveCoupled
         integer, intent(in) :: solveBMethod
         real(dpn) :: tempMax1,tempMax2,tempMax3


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

         this%L_c = 0.5d0*getMaxRangei(gd)

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

         this%dhMin = getDhMin(gd)
         this%dhiMin = getDhiMin(gd)

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
         real(dpn), intent(in) :: time
         rd%time = time + rd%dTime
       end subroutine

       function getDtime(this) result(dtime)
         ! This is the dimensionless time, that is 
         ! used in ALL equations.
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: dtime
         dtime = this%dtime
       end function

       function getDPseudoTime(this) result(ds)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: ds
         ds = this%ds
       end function

       function getFo(this) result(Fo)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: Fo
         Fo = this%Fo
       end function

       function getRe(this) result(Re)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: Re
         Re = this%Re
       end function

       function getHa(this) result(Ha)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: Ha
         Ha = this%Ha
       end function

       subroutine setDtime(this,dtime)
         implicit none
         type(rundata),intent(inout) :: this
         real(dpn),intent(in) :: dtime
         this%dtime = dtime
       end subroutine

       function getInteractionNumber(this) result(N)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: N
         N = this%N
       end function

       function getRem(this) result(Rem)
         implicit none
         type(rundata),intent(in) :: this
         real(dpn) :: Rem
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