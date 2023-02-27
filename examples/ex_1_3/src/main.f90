! Объявляем программу.
program exercise_1_3
    ! Включаем модуль Environment в программу.
    use Environment
 
    ! Отключаем механизм задания типов переменных по умолчанию.
    implicit none
 
    ! Задаём именованные символьные константы с длинами,
    !  равными длинам постоянных выражений после знака =.
    ! * - это предполагаемый параметр длины.
    character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
    ! Задаём переменную с длиной, изменяемой во время выполнения.
    ! : - это отложенный параметр длины.
    character(:), allocatable  :: fmt
    ! Переменные интегрального типа (32-битные целые знаковые числа).
    integer                    :: In = 0, Out = 0, i = 0
    ! Переменные вещественного типа (32-битные).
    real(R_)                   :: x = 0, ln_x
    ! Переменная - одномерный массив из четырёх вещественных 32-битных чисел.
    real(R_)                   :: Items(4) = 0
 
    ! Открываем файл по пути из константы input_file.
    ! unit - это номер устройства, указывающего на файл.
    ! Номер может быть любым числом в диапазоне [9 - 99].
    ! Каждый открытый файл должен обладать уникальным значением unit.
    ! Однако в данном случае используется спецификатор newunit из Fortran 2008.
    ! newunit автоматически выбирает значение unit и сохраняет выбранное значение,
    !  в данном случае, в переменную In.
    open (file=input_file, newunit=In)
       ! Считываем из устройства с номером In
       ! Значение в переменную x (вещественное 32-битное число).
       ! * означает отсутствие формата чтения из файла.
       read (In, *) x
    ! Закрываем устройство, ассоциируемое с номером In.
    close (In)
 
    ! Открываем файл по пути из константы output_file
    !  с кодировкой E_ из модуля Environment ("UTF-8").
    open (file=output_file, encoding=E_, newunit=Out)
       ! Присваиваем символьной константе значение (формат).
       fmt = "(a, T7, '= ', f6.2)"
       ! Пишем в файл с номером устройства Out в соответствии с форматом fmt.
       write (Out, "(a, T7, '= ', f6.2)") "x", x
    ! Закрываем устройство, ассоциируемое с номером Out.
    close (Out)
 
    ! Присваиваем элементу массива под индексом 1 значение x-1.
    Items(1) = x-1
 
    ! Выполняем цикл по переменной i от 2 до 4.
    do i = 2, 4
       ! Присваиваем элементу массива под индексом i результат выражения, где:
       !  Items(i-1) - предыдущий элемент массива относительно текущего (i и i-1).
       !  Items(1) - элемент массива под индексом 1.
       ! Таким образом мы формируем массив вида: [(x-1), (x-1)^2, (x-1)^3, (x-1)^4].
       ! Он пригодится нам в следующем этапе.
       Items(i) = Items(i-1) * Items(1)
       ! Конец тела цикла
    end do
 
    ! Производим поэлементное деление массива.
    ! Таким образом формируется ряд Меркатора - ряд Тейлора для функции натурального логарифма:
    !  ln(1 + x) = x - x^2 / 2 + x^3 / 3 - x^4 / 4 + ...
    Items = Items / [1, -2, 3, -4] ! ВЕКТОРИЗАЦИЯ. SSE, AVX
    ! Аналогичный код, но с явным указанием индексов:
    !  Items(1:4) = Items(1:4) / [1, -2, 3, -4]
 
    ! Используется этот код с 4-мя операциями,
    ! а не следующий, при котором векторизация невозможна:
    ! Items(2:4) = Items(2:4) / [-2, 3, -4]
 
    ! Вычисляем сумму значений элементов массива
    !  и присваиваем результат переменной ln_x.
    ! Таким образом мы находим окончательный результат - натуральный логарифм.
    ln_x = Sum(Items)
 
    ! Открываем файл по пути из константы output_file в режиме append.
    open (file=output_file, encoding=E_, newunit=Out, position='append')
       ! Пишем в файл с номером устройства Out в соответствии с форматом fmt.
       write (Out, fmt) "ln(x)", ln_x
       ! Проверка: в данном случае мы пишем разницу в вычислении натурального логарифма
       !  между результатом, полученным библиотечной (встроенной) функцией log(x), и вычисленным нами.
       ! Разумеется, в соответствии с форматом, заданным в переменной fmt.
       write (Out, fmt) "error", log(x) - ln_x
    ! Закрываем устройство, ассоциируемое с номером Out.
    close (Out)
 
 ! Конец программы.
 end program exercise_1_3