       module assign_B0_vs_t_mod
       use current_precision_mod
       use time_marching_params_mod
       use VF_mod
       implicit none

       private
       public :: assign_B0_vs_t

       contains

       subroutine assign_B0_vs_t(B0,TMP)
         implicit none
         type(VF),intent(inout) :: B0
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(58) :: B_all,t_all
         real(cp) :: B,t,t_total
         integer :: i
         t_total = (real(TMP%n_step_stop,cp)-real(TMP%n_step_start,cp))*TMP%dt
         i = floor(real(TMP%n_step,cp)/real(TMP%n_step_stop,cp))
         i = maxval((/i,1/))
         i = minval((/i,58/))
         call B_mean_Mike_Ulrickson_data(B_all); B = B_all(i)
         call t_Mike_Ulrickson_data(t_all); t = t_all(i)
         call assign(B0,B_all(i))
       end subroutine

       subroutine B_mean_Mike_Ulrickson_data(B_mean)
         implicit none
         real(cp),dimension(58),intent(inout) :: B_mean
         B_mean = (/1.2223329788_cp,&
                    1.22230957575_cp,&
                    1.2231447381_cp,&
                    1.22330548762_cp,&
                    1.22343775489_cp,&
                    1.22354417895_cp,&
                    1.22362374097_cp,&
                    1.22368451279_cp,&
                    1.2236862935_cp,&
                    1.21492742695_cp,&
                    1.20548334752_cp,&
                    1.19471246899_cp,&
                    1.18210345204_cp,&
                    1.17118971604_cp,&
                    1.16105003493_cp,&
                    1.15042811147_cp,&
                    1.13796916759_cp,&
                    1.12670089989_cp,&
                    1.1127998558_cp,&
                    1.08678262378_cp,&
                    1.08563718425_cp,&
                    1.0850835848_cp,&
                    1.08433611663_cp,&
                    1.08355096719_cp,&
                    1.08271145374_cp,&
                    1.08164499394_cp,&
                    1.08045703187_cp,&
                    1.07948700086_cp,&
                    1.07840176411_cp,&
                    1.07740067578_cp,&
                    1.07647910875_cp,&
                    1.07565630369_cp,&
                    1.07473625037_cp,&
                    1.07376677197_cp,&
                    1.07294536187_cp,&
                    1.07221648166_cp,&
                    1.07158078686_cp,&
                    1.07095629554_cp,&
                    1.07057013899_cp,&
                    1.06997567447_cp,&
                    1.06914360232_cp,&
                    1.06648693498_cp,&
                    1.06372533855_cp,&
                    1.06103937444_cp,&
                    1.05745159413_cp,&
                    1.05544091663_cp,&
                    1.05194465575_cp,&
                    1.04922381988_cp,&
                    1.02437514415_cp,&
                    0.970016139859_cp,&
                    0.912174503425_cp,&
                    0.861601731101_cp,&
                    0.810757901417_cp,&
                    0.677151529262_cp,&
                    0.504737922161_cp,&
                    0.362312434141_cp,&
                    0.232272799497_cp,&
                    0.134980403054_cp/)
       end subroutine

       subroutine t_Mike_Ulrickson_data(t)
         implicit none
         real(cp),dimension(58),intent(inout) :: t
         t = (/0.0_cp,&
               1000.0_cp,&
               2000.0_cp,&
               3000.0_cp,&
               4000.0_cp,&
               5000.0_cp,&
               6000.0_cp,&
               7000.0_cp,&
               7100.0_cp,&
               7200.0_cp,&
               7300.0_cp,&
               7400.0_cp,&
               7500.0_cp,&
               7600.0_cp,&
               7700.0_cp,&
               7800.0_cp,&
               7900.0_cp,&
               8000.0_cp,&
               8100.0_cp,&
               8200.0_cp,&
               8300.0_cp,&
               8400.0_cp,&
               8500.0_cp,&
               8600.0_cp,&
               8700.0_cp,&
               8800.0_cp,&
               8900.0_cp,&
               9000.0_cp,&
               9100.0_cp,&
               9200.0_cp,&
               9300.0_cp,&
               9400.0_cp,&
               9500.0_cp,&
               9600.0_cp,&
               9700.0_cp,&
               9800.0_cp,&
               9900.0_cp,&
               10000.0_cp,&
               10100.0_cp,&
               10200.0_cp,&
               10300.0_cp,&
               10400.0_cp,&
               10500.0_cp,&
               10600.0_cp,&
               10700.0_cp,&
               10800.0_cp,&
               10900.0_cp,&
               11000.0_cp,&
               12000.0_cp,&
               14000.0_cp,&
               16000.0_cp,&
               18000.0_cp,&
               20000.0_cp,&
               25000.0_cp,&
               30000.0_cp,&
               35000.0_cp,&
               40000.0_cp,&
               45000.0_cp/)
       end subroutine

       end module