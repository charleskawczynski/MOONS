     module Mike_Ulrickson_data_mod
     use current_precision_mod
     use time_marching_params_mod
     use VF_extend_mod
     implicit none

     private
     public :: B_r_mean_normalized
     public :: B_p_mean_normalized
     public :: B_maxval
     public :: time_normalized

     integer,parameter :: n_points = 58
     real(cp),parameter :: micro_seconds_to_seconds = 10.0_cp**(-6.0_cp)

     contains

     subroutine B_r_mean_normalized(B)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: B
       call B_radial_mean(B); B = B/B_maxval()
     end subroutine

     subroutine B_p_mean_normalized(B)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: B
       call B_poloidal_mean(B); B = B/B_maxval()
     end subroutine

     subroutine time_normalized(t,t_plasma)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: t
       real(cp),intent(in) :: t_plasma
       call time(t)
       t = t*micro_seconds_to_seconds/t_plasma
     end subroutine

     function B_maxval() result(B_max)
       implicit none
       real(cp),dimension(n_points) :: B_r,B_p
       real(cp) :: B_max
       call B_poloidal_mean(B_p)
       call B_radial_mean(B_r)
       B_max = maxval((/B_r,B_p/))
     end function

     subroutine B_poloidal_mean(B)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: B
       B(1) = 1.20628735198_cp
       B(2) = 1.20626523805_cp
       B(3) = 1.20707869737_cp
       B(4) = 1.20723519238_cp
       B(5) = 1.20736376165_cp
       B(6) = 1.2074670058_cp
       B(7) = 1.20754408138_cp
       B(8) = 1.2076028386_cp
       B(9) = 1.2076045133_cp
       B(10) = 1.19906264524_cp
       B(11) = 1.18985071952_cp
       B(12) = 1.17935726652_cp
       B(13) = 1.16705135857_cp
       B(14) = 1.15640845773_cp
       B(15) = 1.14651285721_cp
       B(16) = 1.13614306957_cp
       B(17) = 1.12399623451_cp
       B(18) = 1.11301096412_cp
       B(19) = 1.09944287811_cp
       B(20) = 1.07422581796_cp
       B(21) = 1.07311875967_cp
       B(22) = 1.07258952018_cp
       B(23) = 1.0718700883_cp
       B(24) = 1.07111312563_cp
       B(25) = 1.07030253938_cp
       B(26) = 1.0692695161_cp
       B(27) = 1.06811719473_cp
       B(28) = 1.06717789214_cp
       B(29) = 1.06612563885_cp
       B(30) = 1.06515491405_cp
       B(31) = 1.06426265249_cp
       B(32) = 1.06346590308_cp
       B(33) = 1.06257444528_cp
       B(34) = 1.06163403839_cp
       B(35) = 1.06083846575_cp
       B(36) = 1.06013321546_cp
       B(37) = 1.05951893339_cp
       B(38) = 1.05891609945_cp
       B(39) = 1.05854588381_cp
       B(40) = 1.05797133085_cp
       B(41) = 1.05715785468_cp
       B(42) = 1.05456087396_cp
       B(43) = 1.0518653267_cp
       B(44) = 1.04924188454_cp
       B(45) = 1.04573744025_cp
       B(46) = 1.04377528768_cp
       B(47) = 1.04035437643_cp
       B(48) = 1.03769737041_cp
       B(49) = 1.01342337526_cp
       B(50) = 0.96027791034_cp
       B(51) = 0.903749303838_cp
       B(52) = 0.854222332346_cp
       B(53) = 0.80439838564_cp
       B(54) = 0.673229708095_cp
       B(55) = 0.503047974139_cp
       B(56) = 0.362047947922_cp
       B(57) = 0.230237554518_cp
       B(58) = 0.127946587997_cp
     end subroutine

     subroutine B_radial_mean(B)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: B
       B(1) = 0.116585219499_cp
       B(2) = 0.116572322302_cp
       B(3) = 0.116736701655_cp
       B(4) = 0.11677028807_cp
       B(5) = 0.116795800294_cp
       B(6) = 0.116817120905_cp
       B(7) = 0.116833884333_cp
       B(8) = 0.11684726979_cp
       B(9) = 0.116847971432_cp
       B(10) = 0.115600851374_cp
       B(11) = 0.114294563194_cp
       B(12) = 0.112790630702_cp
       B(13) = 0.111106981748_cp
       B(14) = 0.109552630539_cp
       B(15) = 0.108160248647_cp
       B(16) = 0.106699436839_cp
       B(17) = 0.104906174502_cp
       B(18) = 0.103378249161_cp
       B(19) = 0.101465046887_cp
       B(20) = 0.0966570989539_cp
       B(21) = 0.0964572114459_cp
       B(22) = 0.0963437025154_cp
       B(23) = 0.0962067083936_cp
       B(24) = 0.0960670838805_cp
       B(25) = 0.0959205358582_cp
       B(26) = 0.0957396781373_cp
       B(27) = 0.0955433221811_cp
       B(28) = 0.095352934493_cp
       B(29) = 0.0951694209666_cp
       B(30) = 0.0950033539726_cp
       B(31) = 0.0948435984205_cp
       B(32) = 0.0947041741138_cp
       B(33) = 0.0945529594517_cp
       B(34) = 0.0943919245411_cp
       B(35) = 0.0942529815142_cp
       B(36) = 0.0941278611412_cp
       B(37) = 0.0940171375051_cp
       B(38) = 0.0939024314695_cp
       B(39) = 0.0938253818267_cp
       B(40) = 0.0937301136411_cp
       B(41) = 0.0935830965168_cp
       B(42) = 0.0932090553856_cp
       B(43) = 0.0927688818927_cp
       B(44) = 0.092369541128_cp
       B(45) = 0.0917603652518_cp
       B(46) = 0.0914503244176_cp
       B(47) = 0.09099925553_cp
       B(48) = 0.0905027020364_cp
       B(49) = 0.0867296506489_cp
       B(50) = 0.0785355000882_cp
       B(51) = 0.0698529355153_cp
       B(52) = 0.0624727982536_cp
       B(53) = 0.0550413661924_cp
       B(54) = 0.0378407725476_cp
       B(55) = 0.0224045648176_cp
       B(56) = -0.00233673927416_cp
       B(57) = 0.0300403947916_cp
       B(58) = 0.0425948541233_cp
     end subroutine

     subroutine time(t)
       implicit none
       real(cp),dimension(n_points),intent(inout) :: t
       t(1) = 0.0_cp
       t(2) = 1000.0_cp
       t(3) = 2000.0_cp
       t(4) = 3000.0_cp
       t(5) = 4000.0_cp
       t(6) = 5000.0_cp
       t(7) = 6000.0_cp
       t(8) = 7000.0_cp
       t(9) = 7100.0_cp
       t(10) = 7200.0_cp
       t(11) = 7300.0_cp
       t(12) = 7400.0_cp
       t(13) = 7500.0_cp
       t(14) = 7600.0_cp
       t(15) = 7700.0_cp
       t(16) = 7800.0_cp
       t(17) = 7900.0_cp
       t(18) = 8000.0_cp
       t(19) = 8100.0_cp
       t(20) = 8200.0_cp
       t(21) = 8300.0_cp
       t(22) = 8400.0_cp
       t(23) = 8500.0_cp
       t(24) = 8600.0_cp
       t(25) = 8700.0_cp
       t(26) = 8800.0_cp
       t(27) = 8900.0_cp
       t(28) = 9000.0_cp
       t(29) = 9100.0_cp
       t(30) = 9200.0_cp
       t(31) = 9300.0_cp
       t(32) = 9400.0_cp
       t(33) = 9500.0_cp
       t(34) = 9600.0_cp
       t(35) = 9700.0_cp
       t(36) = 9800.0_cp
       t(37) = 9900.0_cp
       t(38) = 10000.0_cp
       t(39) = 10100.0_cp
       t(40) = 10200.0_cp
       t(41) = 10300.0_cp
       t(42) = 10400.0_cp
       t(43) = 10500.0_cp
       t(44) = 10600.0_cp
       t(45) = 10700.0_cp
       t(46) = 10800.0_cp
       t(47) = 10900.0_cp
       t(48) = 11000.0_cp
       t(49) = 12000.0_cp
       t(50) = 14000.0_cp
       t(51) = 16000.0_cp
       t(52) = 18000.0_cp
       t(53) = 20000.0_cp
       t(54) = 25000.0_cp
       t(55) = 30000.0_cp
       t(56) = 35000.0_cp
       t(57) = 40000.0_cp
       t(58) = 45000.0_cp
     end subroutine

     end module