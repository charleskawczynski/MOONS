      public :: subtract
      interface subtract;          module procedure subtract_VF_VF;         end interface
      interface subtract;          module procedure subtract_VF_VF_VF;      end interface
      interface subtract;          module procedure subtract_VF_SF;         end interface
      interface subtract;          module procedure subtract_VF_S;          end interface
      interface subtract;          module procedure subtract_S_VF;          end interface