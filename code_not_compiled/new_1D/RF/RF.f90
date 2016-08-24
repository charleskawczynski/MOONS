      module RF_mod
        ! Pre-processor directives: (_DEBUG_RF_,_PARALLELIZE_RF_)
        ! 
        ! Naming convention: name = operation_type1_type2
        ! 
        !      RF = type(realField)
        !      R  = real(cp),dimension(:,:,:)
        !      S  = real(cp)
        ! 
        ! Example(1): Adding a scalar to RF
        !             name = add_RF_S
        ! Example(2): Subtracting a real field from RF
        !             name = subtract_RF_R
        ! Example(3): Subtracting a RF from a real field
        !             name = subtract_R_RF
        ! 
        ! NOTES: RF stands for 'real field'
        ! 
        ! Rules:
        ! a = a + b => call add(a,b)
        ! a = a - b => call subtract(a,b)
        ! a = a * b => call multiply(a,b)
        ! a = a / b => call divide(a,b)
        ! a = b / a => call divide(b,a)
        ! OR
        ! c = a + b => call add(c,a,b)
        ! c = a - b => call subtract(c,a,b)
        ! c = a * b => call multiply(c,a,b)
        ! c = a / b => call divide(c,a,b)
        ! c = b / a => call divide(c,b,a)

        use RF_base_mod
        use RF_ops_mod
        use RF_aux_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: realField
        public :: init,delete,display,print,export,import ! Essentials

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! BC initialization
        public :: init_BCs

        ! Monitoring
        public :: print_physical

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product,swap
        ! Auxiliary
        public :: square,min,max,maxabs
        public :: maxabsdiff,mean,sum
        public :: size

      end module