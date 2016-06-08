       module table_mod
       ! Helps export information in tabular format.
       ! 
       ! Example:
       ! 
       ! real(cp),dimension(6) :: f
       ! call export_table('Face:',f,number of spaces per column,unit)
       ! 
       ! 
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_table

       interface export_table;    module procedure export_table_CP;      end interface
       interface export_table;    module procedure export_table_INT;     end interface
       interface export_table;    module procedure export_table_LOG;     end interface
       interface export_table;    module procedure export_table_CHAR;    end interface

       contains

       subroutine export_table_CP(label,row,col_space,after_decimal,u)
         implicit none
         character(len=*),intent(in) :: label
         real(cp),dimension(:),intent(in) :: row
         integer,intent(in) :: col_space,after_decimal,u
         type(string) :: s_label,c_space,s_row,n_precision
         call init(n_precision,int2str(after_decimal))
         call init(s_label,int2str(len(label)))
         call init(c_space,int2str(col_space))
         call init(s_row,int2str(size(row)))
         call remove_leading_zeros(s_label)
         call remove_leading_zeros(c_space)
         call remove_leading_zeros(s_row)
         write(u,('(A'//str(s_label)//','//str(s_row)//'F'//str(c_space)//'.'//str(n_precision)//')')) label,row
       end subroutine

       subroutine export_table_INT(label,row,col_space,u)
         implicit none
         character(len=*),intent(in) :: label
         integer,dimension(:),intent(in) :: row
         integer,intent(in) :: col_space,u
         type(string) :: s_label,c_space,s_row
         call init(s_label,int2str(len(label)))
         call init(c_space,int2str(col_space))
         call init(s_row,int2str(size(row)))
         call remove_leading_zeros(s_label)
         call remove_leading_zeros(c_space)
         call remove_leading_zeros(s_row)
         write(u,('(A'//str(s_label)//','//str(s_row)//'I'//str(c_space)//')')) label,row
       end subroutine

       subroutine export_table_LOG(label,row,col_space,u)
         implicit none
         character(len=*),intent(in) :: label
         logical,dimension(:),intent(in) :: row
         integer,intent(in) :: col_space,u
         type(string) :: s_label,c_space,s_row
         call init(s_label,int2str(len(label)))
         call init(c_space,int2str(col_space))
         call init(s_row,int2str(size(row)))
         call remove_leading_zeros(s_label)
         call remove_leading_zeros(c_space)
         call remove_leading_zeros(s_row)
         write(u,('(A'//str(s_label)//','//str(s_row)//'L'//str(c_space)//')')) label,row
       end subroutine

       subroutine export_table_CHAR(label,row,col_space,u)
         implicit none
         character(len=*),intent(in) :: label
         character(len=1),dimension(:),intent(in) :: row
         integer,intent(in) :: col_space,u
         type(string) :: s_label,c_space,s_row
         call init(s_label,int2str(len(label)))
         call init(c_space,int2str(col_space))
         call init(s_row,int2str(size(row)))
         call remove_leading_zeros(s_label)
         call remove_leading_zeros(c_space)
         call remove_leading_zeros(s_row)
         write(u,('(A'//str(s_label)//','//str(s_row)//'A'//str(c_space)//')')) label,row
       end subroutine

       end module