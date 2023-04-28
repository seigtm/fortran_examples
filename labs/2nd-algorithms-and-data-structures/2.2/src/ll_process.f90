module ll_process
   use environment
   use ll_io
   implicit none

contains
   ! Чистая функция проверки того, что строка list состоит только из символов
   !  алфавита alphabet.
   ! Возвращает номер символа в исходной строке не найденного в алфавите,
   !  либо размер исходной строки + 1, если все символы содержатся в алфавите.
   pure recursive integer(I_) function in_alphabet(list, alphabet, res) result(count)
      type(node), allocatable, intent(in) :: list, alphabet
      integer(I_),             intent(in) :: res

      if(allocated(list) .and. contains(alphabet, list%value)) then
        count = in_alphabet(list%next, alphabet, res + 1)
      else
         count = res
      end if
   end function in_alphabet

   ! Вспомогательная рекурсивная функция, которая непосредственно
   !  проверяет включение символа ch в алфавит alphabet.
   ! Возвращает true, если символ содержится в алфавите
   !  и false в обратном случае.
   pure recursive logical function contains(alphabet, ch) result(res)
      type(node), allocatable, intent(in) :: alphabet
      character(kind=CH_),     intent(in) :: ch

      if(alphabet%value == ch) then
         res = .true.
      else if(allocated(alphabet%next)) then
         res = contains(alphabet%next, ch)
      else
         res = .false.
      end if
   end function contains
end module ll_process
