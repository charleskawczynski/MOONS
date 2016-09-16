         ! Eventually this should be shortened...
         ! 1000+ (Other test cases)
         ! 100+ (Short & Fast cases for testing)
         !    100: 3D MHD LDC - "base case"
         !    101: 2D LDC  / 2D MHD LDC
         !    102: 2D Duct / 2D MHD Duct
         !    103: 3D LDC  / 3D MHD LDC
         !    104: 3D Duct / 3D MHD Duct
         ! 
         !    1: Guj & Stella (LDC, uniform)
         !    2: Guj & Stella (LDC, non-uniform)
         !    3: Pattison (3D MHD LDC)
         !    4: Hunt (Duct Flow)
         !    5: Shercliff (Duct Flow)
         !    6: Bandaru (Channel Flow)
         !    7: Weiss Flux Expulsion (Isolated Eddy)
         ! 
       ! 
       ! 0-99-series (verification cases against exact solutions)
       ! 
       !    0 : LDC          , Re=400,Ha=0  (symmetry) Ni=even
       !    1 : LDC          , Re=400,Ha=0  (symmetry) Ni=odd
       ! 
       !    2 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=32
       !    3 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=64
       !    4 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=128
       !    5 : LDC          , Re=400,Ha=0  (refinement in symmetry plane) N=256
       ! 
       !    6 : Duct Flow    , Re=400,Ha=0  (refinement) N=32
       !    7 : Duct Flow    , Re=400,Ha=0  (refinement) N=64
       !    8 : Duct Flow    , Re=400,Ha=0  (refinement) N=128
       !    9 : Duct Flow    , Re=400,Ha=0  (refinement) N=256
       ! 
       !    50 : LDC         , Re=2000,Ha=0 - Onset of turbulent flow
       !    51 : LDC         , Re=3200,Ha=0 - Turbulent flow
       ! 
       ! 100-series (Lid driven cavity flows)
       ! 
       !    100 : LDC , Re=400  , Ha=0    , S=1    (Guj / stella) uniform
       !    101 : LDC , Re=1000 , Ha=0    , S=1    (Guj / stella) non-uniform
       ! 
       !    102 : LDC , Re=100  , Ha=10   , S=1    (sergey)
       !    103 : LDC , Re=1000 , Ha=100  , S=1    (sergey)
       !    104 : LDC , Re=1000 , Ha=1000 , S=1    (sergey) - instabilities
       ! 
       !    105 : LDC , Re=100  , Ha=10   , S=1     (sergey)
       !    106 : LDC , Re=100  , Ha=10   , S=100   (sergey)
       !    107 : LDC , Re=100  , Ha=10   , S=1000  (sergey)
       !    108 : LDC , Re=100  , Ha=10   , S=10^6  (sergey)
       ! 
       !    109 : LDC , Re=100  , Ha=10   , S=1     (Sergey and Peter)
       ! 
       ! 
       ! 200-series (Duct flows)
       ! 
       !    200 : Duct Flow   , Re=100.0  , Ha=0   (test case)
       !    201 : Duct Flow   , Re=1000   , Ha=100 (mhd test case)
       ! 
       !    110 : FD Duct Flow  , Re=100  , Ha=500  , Rem=0    (Sergey/HIMAG/MOONS)
       ! 
       !    250 : Duct Flow   , Re=15574.07   , Ha=2900    (case B2)
       ! 
       ! 300-series (Cylinder driven cavity flows)
       ! 
       !    300 : CDC , Re=400
       ! 
       ! 400-series (Plasma disruption modeling)
       ! 
       !    400 : PD , Re=100   , Ha=10  (Sergey/MOONS/Peter)
       ! 
       ! 1000-series (Purely sequential)
       ! 
       !    1001: Case 1A,B,C, LDC, Ha = 10,100,1000   Re = 100, tw = 0.1, sig_l = sig_w, Rem = 0, conducting walls except lid
       !    1002: Case 2A,B, FD square duct flow, Ha = 500, Rem = 0, All walls insulating
       !          Hartmann walls conducting, tw = 0.01, sig_l = sig_w, side walls insulating
       !    1003: Case 3, Plasma disruption, Refer to powerpoint
       ! 
       !    1004: LDC by Salah - To produce Fig. 4, Fig. 5
       !    1005: No slip cavity - Jack's experiment
       !    1006: Weiss Benchmark (Isolated Eddy / Single Eddy)
       !    1007: Parker (Cylinder)
       !    1008: Bandaru (Channel Flow)
       !    1009: Kawczynski (LDC demo cases)
       !    1010: Kawczynski (LDC demo cases) for B = 0 BCs
       !    1011: Kawczynski (Hunt/Shercliff demo cases) for B = 0 BCs
       !    1012: Pattison (3D MHD LDC)
       !    1013: 2D LDC (using FFT)
