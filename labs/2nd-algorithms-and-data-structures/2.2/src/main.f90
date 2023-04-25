! Баранов К.П., 20021, ЛР 2, вариант 2.

! Задание выполняется в виде программного проекта из двух модулей, в котором необходимо
!  использовать динамические однонаправленные списки.
! Списки необходимо обрабатывать чистой хвостовой рекурсией при всех операциях с ними.
! При возможности применяется регулярное программирование.

! Задание: разработать чистую функцию проверки того, что данная строка A(M)
!  состоит только из символов заданной строки B(L).

! Указание: элементом списка является символ строки. Возвращать номер символа, не
!  найденного в строке B(L). Иначе возвращаем M+1.

program ll
   use environment
   use ll_process
   use ll_io
   implicit none

   character(:), allocatable :: input_file_list, input_file_alphabet, output_file
   type(node),   allocatable :: list, alphabet
   integer(I_)               :: list_size, alphabet_size, in_alphabet_counter

   input_file_list     = "../data/list.txt"      ! Путь до входного файла с исходной строкой - A(M).
   input_file_alphabet = "../data/alphabet.txt"  ! Путь до входного файла с алфавитом - B(L).
   output_file         = "output.txt"            ! Путь до выходного файла.

   ! Считываем строки A(M) - исходная и B(L) - алфавит.
   list     = read_list(input_file_list,     list_size)
   alphabet = read_list(input_file_alphabet, alphabet_size)

   if(allocated(list) .and. allocated(alphabet)) then
      call output_list(output_file, list,     "Исходный список:",  "rewind")
      call output_list(output_file, alphabet, "Исходный алфавит:", "append")
      in_alphabet_counter = in_alphabet(list, alphabet)

      if(in_alphabet_counter /= list_size + 1) then
         call output_result(output_file, "Исходный список содержит символы, "  // &
            "которые не присутствуют в алфавите. Номер первого "               // &
            "не найденного символа в исходной строке = " // in_alphabet_counter // "!", "append")
      else
         call output_result(output_file, "Исходный список состоит исключительно " // &
            "из символов алфавита!", "append")
      end if
   end if
end program ll
