      public :: multiply
      interface multiply;          module procedure multiply_VF_VF;         end interface
      interface multiply;          module procedure multiply_VF_VF_VF;      end interface
      interface multiply;          module procedure multiply_VF_SF;         end interface
      interface multiply;          module procedure multiply_VF_S;          end interface
      interface multiply;          module procedure multiply_S_VF;          end interface
