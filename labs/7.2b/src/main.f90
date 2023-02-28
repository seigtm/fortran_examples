! Баранов К.П., группа 20021.
! Лабораторная работа №7, вариант 7.2 (б).

! Задача: преобразовать заданную числовую послед-ть:
!  a1, a2, a3, ..., a75 так, чтобы элементы,
!  удовлетворяющие условию ai > 0, располагались
!  в конце последовательности в порядке убывания.

! Указания: построить маску для положительных элементов.
! Сформировать массив A = [Paсk(, .not. Mask), Paсk(, Mask)].
! Отсортировать последние положительные элементы.
! Count.

program SortWithPackMask
   use Environment
   ! use Sort_With_Pack_Mask_Mod
   ! use Sort_With_Pack_Mask_IO
   implicit none

   character(*), parameter   :: INPUT_FILE  = "../data/input.txt", &
                                OUTPUT_FILE = "output.txt"
end program SortWithPackMask
