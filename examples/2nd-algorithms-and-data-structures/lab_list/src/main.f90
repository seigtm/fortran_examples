! Copyright 2019 Fyodorov S. A.

program reference_lab_list
   use Environment
   use List_Process
   use List_IO

   implicit none
   character(:), allocatable :: input_file, output_file

   type(node), pointer   :: List => Null()
   type(node2), pointer  :: List2 => Null(), Tail => Null()
   type(sorted_node), pointer   :: List3 => Null(), Sorted_List => Null()
   integer              :: value = 0
   type(node_tree), pointer :: tree => Null()

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   
   List => Read_list(input_file)
   call Read_list2(input_file, List2, Tail)
   call Read_sorted_list(input_file, List3, Sorted_list)
   tree => Read_tree(input_file)
   
   if (Associated(List)) then
      call Output_list(output_file, List, "Исходный список:", "rewind")

      call Put(List, 8)
      call Output_list(output_file, List, "Список после вставки числа:", "append")

      call Get(List, value)
      !value = Get(List) ! XXX
      call Output_list(output_file, List, "Список после забирания первого числа:", "append")
      
      call Delete(List, 7)
      call Output_list(output_file, List, "Список после забирания числа 7:", "append")
      
      call Get(List, value)
      call Output_list(output_file, List, "Список после забирания первого числа:", "append")
      
      call Output_list2(output_file, List2, "Двунаправленный список:", "append")
      
      call Output_ordered_list(output_file, List3, "Неотсортированный список:", "append")
      call Output_sorted_list(output_file, Sorted_list, "Отсортированный список:", "append")
      
      call Output_tree(output_file, tree, "Простой обход дерева:", "append")
      call Delete_in_tree(tree, 3)
      call Output_tree(output_file, tree, "Дерево после удалени корня:", "append")
   end if
   
end program reference_lab_list
