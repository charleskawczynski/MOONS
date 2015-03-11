       module geometries_mod
       use grid_mod
       use gridGenTools_mod

       ! type(LDCGeom) :: LDC
       ! type(grid) :: g
       ! LDC = init()
       ! 
       ! or
       ! 
       ! LDC = LDCGeom%base1() ! base 1 geometry
       ! 
       ! call setNi(LDC,Ni)
       ! g = init(LDC)
       ! 
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type LDCGeom
         integer :: Ni,Nw
         real(cp) :: twtop,twbot,hmin,hmax,betai,betaw,alphai,alphaw
       end type

       type ductGeom
         integer :: Ni,Nw
         real(cp) :: twtop,twbot,hmin,hmax,betai,betaw
       end type

       interface init;    module interface initLDCGrid;    end interface
       interface init;    module interface initLDC;    end interface
       interface init;    module interface initDuct;   end interface

       contains

       function initLDC() result(LDC)
         type(LDCGeom) :: LDC
         LDC%Ni = 2**6
         LDC%Nw = 2**3
         LDC%twtop = real(0.1,cp)
         LDC%twbot = real(0.1,cp)

         LDC%hmin = real(-1.0,cp)
         LDC%hmax = real(1.0,cp)

         LDC%betai = real(1.1,cp)
         LDC%betaw = real(1.1,cp)

         LDC%alphai = real(0.0,cp)
         LDC%alphaw = real(0.5,cp)
       end function

       function initLDCGrid(LDC) result(g)
         type(LDCGeom),intent(in) :: LDC
         type(grid) :: g
          robertsGrid3(hmin,hmax,N,alpha,beta)

         call init(g,robertsGrid1(),1,2)
       end function



       end module