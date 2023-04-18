! Copyright 2015 Fyodorov S. A.

module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO

   implicit none

contains
   
   ! Формирование разницы двух кодов в виде новых строк.
   pure recursive function Diff_Codes(InitialCode, ModdedCode) result(DiffCode)
      type(SourceLine), pointer     :: DiffCode
      type(SourceLine), intent(in)  :: InitialCode
      type(SourceLine), intent(in)  :: ModdedCode

      ! Поиск и запись отличных строк в рамках исходного файла InitialCode.
      ! Если строки равны:
      if (InitialCode%String == ModdedCode%String) then
         ! Если остались ещё строки, то переход к следующей.
         if (Associated(InitialCode%next)) then
            DiffCode => Diff_Codes(InitialCode%next, ModdedCode%Next)  ! Почему на 23 строке Initial_code идёт с next, а на 33 строке – без него?
         ! В противном случае если остались строки в модифицированном файле, то добавление их в список.
         else if (Associated(ModdedCode%next)) then
            ! Запись всех строк оставшейся части ModdedCode.
            DiffCode => Add_Recent_Source_Lines(ModdedCode%next)
         end if !  ELSE DiffCode => Null()
      ! Если строки не равны, то добавление её в список.
      else
         allocate (DiffCode)
         DiffCode%String = CH__"++ " // ModdedCode%String
         DiffCode%next => Diff_Codes(InitialCode, ModdedCode%Next)  ! Почему на 23 строке Initial_code идёт с next, а на 33 строке – без него?
         ! Потому что на 23 строке мы рекурсивно вызываем эту же функцию, если существуют следующие строки в коде.
         ! А вот на 33 строке не совсем та же ситуация: мы выполняем код выше только в случае, если строки не совпали:
         !     1     &&  1 =>      (строки    совпали, игнорируем).
         !     2     &&  2 =>      (строки    совпали, игнорируем).
         !     3     &&  3 =>      (строки    совпали, игнорируем).
         ! [ пусто ] &&  4 => ++ 4 (строки не совпали, дописываем).
         !     5     &&  5 =>      (строки    совпали, игнорируем).
         ! Если бы мы не делали этой махинации, а сдвинулись на Next ещё и в InitialCode, то мы бы просто пропустили сравнение
         !  строчек "5", что привело бы к неправильному ответу в данном случае.
         ! В общем, мы не сдвигаемся как раз таки для случая, когда у нас в ModdedCode добавилась новая строчка, которой не было в
         !  InitialCode.
      end if
   end function Diff_Codes

   pure recursive function Add_Recent_Source_Lines(ModdedCode) result(DiffCode)
      type(SourceLine), pointer     :: DiffCode
      type(SourceLine), intent(in)  :: ModdedCode

      allocate (DiffCode)
      DiffCode%String = CH__"++ " // ModdedCode%String
      if (Associated(ModdedCode%next)) &
         DiffCode%next => Add_Recent_Source_Lines(ModdedCode%Next)
   end function Add_Recent_Source_Lines

end module Source_process
