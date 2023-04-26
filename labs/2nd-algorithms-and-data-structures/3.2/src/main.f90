! Баранов К.П., 20021, ЛР 3, вариант 2.

! В текстовом файле F1 находятся стpоки вида "IVANOV 1970".
! Разpаботать процедуpы:
!  1. P1 - создания динамической стpоки из записей (поле array[1..10] и поле integer);
!  2. P2 - вывода динамической стpоки (списка) S в естественном поpядке в файл из записей F2;
!  3. P3 - вывода в текстовый файл F3 содеpжимого файла записей F2.
! В головной пpогpамме pеализовать пpеобpазование и пеpедачу инфоpмации по схеме:
!  "текстовый файл F1 -> динамическая стpока -> типизиpованный файл F2 -> output".
! После вывода динамическую стpоку уничтожить.

program ll
   use environment
   use ll_io
   implicit none

   character(:),        allocatable :: input_file, output_file, data_file
   type(person_node_t), allocatable :: list

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   data_file   = "binary.dat"

   list = read_list(input_file)
   if(allocated(list)) then
      call create_data_file(list, data_file)
      call output(output_file, data_file, "Список:")
      call deallocate_list(list)
   end if
end program ll
