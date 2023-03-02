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
   integer(I_)               :: in, array_size, out
   real(R_), allocatable     :: array(:)

   open(file=input_file, newunit=in)
      read(in, *) array_size
      allocate(array(array_size))
      read(in, *) array
   close(in)

   open(file=output_file, encoding=E_, newunit=out)
      write(out, fmt_int)  "array_size", array_size
      write(out, '(a, T14, "= (", ' // array_size // 'f6.2, " )")') "array", array
      write(out, *)
   close(out)

   call SortPositives(array)

   open(file=output_file, encoding=E_, newunit=out, position="append")
      write(out, '(a, T14, "= (", ' // array_size // 'f6.2, " )")') "sorted array", array
   close(out)

contains
   pure subroutine SortPositives(array)
      real(R_), intent(out) :: array(:)
      logical               :: positives(size(array))
      integer(I_)           :: positives_count, positives_index, i, max_index
      real(R_)              :: tmp

      ! Маска положительных элементов исходного масива, т.е.:
      !    [ 1.00 -1.00  2.00 -2.00 -3.00  3.00 -4.00  4.00  5.00 -5.00 ] =>
      ! => [ True False  True False False  True False  True  True False ]
      positives = (array > 0)

      ! Если положительных элементов нет, то и сортировать в массиве нечего.
      positives_count = Count(positives)
      if (positives_count == 0) return

      ! Переформируем наш массив: сперва элементы <= 0, а затем > 0.
      array = [Pack(array=array, mask=.not. positives), Pack(array=array, mask=positives)]

      ! Индекс первого положительного элемента в массиве.
      ! Size - кол-во положительных + 1, т.к. в начале у нас неположительные (<= 0).
      positives_index = Size(positives) - positives_count + 1

      ! Сортировка положительных элементов массива в порядке убывания
      !  (от большего к меньшему) методом выбора:
      ! 1. Находим в неотсортированной части массива индекс максимального элемента.
      ! 2. Меняем его с первым элементом в неотсортированной части
      !    (если это не один и тот же элемент).
      ! 3. Отсортированная часть увеличилась. Повторяем пп. 1-2.
      do i = positives_index, Size(positives) - 1
         ! MaxLoc вернёт нам индекс относительно i, потому нам нужно
         !  добавить к этому значению i и вычесть единицу.
         max_index = MaxLoc(array(i:), 1) + i - 1

         ! Если текущий индекс не является индексом максимального элемента.
         ! (Мы пропускаем замену за ненадобностью, если это один и тот же элемент).
         if (i /= max_index) then
            ! Меняем местами текущий и максимальный
            !  на данной итерации элементы.
            tmp              = array(i)
            array(i)         = array(max_index)
            array(max_index) = tmp
         end if
      end do
   end subroutine SortPositives

end program SortWithPackMask
