! Copyright 2019 Fyodorov S. A.

program reference_lab_list
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   type(node), allocatable :: List
   integer                 :: value = 0

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   
   List = Read_list(input_file)
   
   call Output_list(output_file, List, "Исходный список:", "rewind")

   call Put(List, 8)
   call Output_list(output_file, List, "Список после вставки числа:", "append")

   call Get(List, value)
   call Output_list(output_file, List, "Список после забирания первого числа:", "append")
   
   call Delete(List, 7)
   call Output_list(output_file, List, "Список после забирания числа '7':", "append")
   
   call Get(List, value)
   call Output_list(output_file, List, "Список после забирания первого числа:", "append")
end program reference_lab_list
