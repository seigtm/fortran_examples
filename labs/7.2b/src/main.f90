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
   implicit none

   character(*), parameter   :: input_file  = "../data/input.txt", &
                                output_file = "output.txt",        &
                                fmt_int     = '(a, T14, "= ", i0)'
   integer(I_)               :: in, out, array_size
   real(R_), allocatable     :: array(:)

   open(file=input_file, newunit=in)
      read(in, *) array_size
      allocate(array(array_size))
      read(in, *) array
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, fmt_int)  "array_size", array_size
      write(out, '(a, T14, "= (", ' // array_size // 'f6.2, ")" )') "array", array
      write(out, *)
   close(out)

   call SortNegatives(array)

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, '(a, T14, "= (", ' // array_size // 'f6.2, " )")') "sorted array", array
   close(out)

contains
   pure subroutine SortNegatives(array)
      real(R_), intent(out) :: array(:)
      logical :: mask(size(array))

      mask = (array > 0)
      array = [Pack(array=array, mask=.not. mask), Pack(array=array, mask=mask)]
      ! TODO: Сортировка положительных элементов в порядке убывания (от большего к меньшему).
      ! ... some sort algorithm ...
      ! ... maybe from his examples ...
   end subroutine SortNegatives

end program SortWithPackMask
