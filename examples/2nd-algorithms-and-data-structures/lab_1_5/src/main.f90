! Copyright 2015 Fyodorov S. A.

program reference_lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   ! MALE = 4_"М"
   ! MALE = "М"
   type(student), pointer  :: Group_List => Null(), Boys_List => Null(), Girls_List => Null()
   integer(I_)             :: Boys_Amount = 0, Girls_Amount = 0

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   Group_List => Read_class_list(input_file)

   if (Associated(Group_List)) then
      call Output_class_list(output_file, Group_List, "Исходный список:", "rewind")

      call Get_list_by_gender(Group_List, Boys_List, Boys_Amount, MALE)
      call Get_list_by_gender(Group_List, Girls_List, Girls_Amount, FEMALE)
   
      call Sort_class_list(Boys_List, Boys_Amount)
      call Sort_class_list(Girls_List, Girls_Amount)
   
      if (Associated(Boys_List)) &
         call Output_class_list(output_file, Boys_List, "Успеваемость юношей:", "append")
      if (Associated(Girls_List)) &
         call Output_class_list(output_file, Girls_List, "Успеваемость девушек:", "append")
   end if

end program reference_lab_1_5
