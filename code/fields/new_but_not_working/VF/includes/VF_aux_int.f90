      public :: dot_product
      public :: invert
      public :: square
      public :: sum
      interface dot_product;       module procedure dot_product_VF;           end interface
      interface invert;            module procedure invert_VF;                end interface
      interface invert;            module procedure invert_VF_S;              end interface
      interface square;            module procedure square_VF;                end interface
      interface sum;               module procedure sum_VF;                   end interface