! Copyright 2015 Fyodorov S. A.

program exercise_io
   use Environment

   implicit none
   character(*), parameter    :: array_file = "../data/array.txt", matrix_file = "../data/matrix.txt", &
                                 cube_file = "../data/cube.txt", output_file = "output.txt"
   integer                    :: In = 0, Out = 0, N = 0, M = 0, K = 0, O = 0, P = 0, Q = 0, i = 0, j = 0
   real(R_), allocatable      :: A(:, :), B(:), C(:, :, :)
   character(:), allocatable  :: fmt

   ! Чтение массива, заданного в ЛЮБОМ формате файла.
   open (file=array_file, newunit=In) ! С encoding=E_ может не работать на некоторых компиляторах.
      read (In, *) K
      allocate (B(K))
      read (In, *) B
   close (In)
      
   ! Чтение прямоугольной матрицы A(N, M),
   ! где N - число строк, M - число столбцов.
   ! Элементы матрицы заданы в файле в ЛЮБОМ формате,
   ! но развёрнуты по строкам.
   open (file=matrix_file, newunit=In) ! С encoding=E_ может не работать на некоторых компиляторах.
      read (In, *) N, M
      allocate (A(N, M))
      ! Хранение в памяти по столбцам.
	  ! Запись по строкам.
      read (In, *) (A(i, :), i = 1, N)
      ! allocate (A(M, N)) ! A(j, i): j - номер столбца, i - номер строки
      ! Хранение в памяти по строкам.
	  ! Запись по строкам.
      ! read (In, *) A
   close (In)

   ! Чтение трёхмерной матрицы C(O, P, Q),
   ! где O - число строк, P - число столбцов, Q - число прямоугольных матриц (глубина).
   ! Элементы матрицы заданы в файле в ЛЮБОМ формате,
   ! но развёрнуты по строкам.
   ! Прямоугольные матрицы разделены ПУСТЫМИ строками (см. входной файл).
   open (file=cube_file, newunit=In) ! С encoding=E_ может не работать на некоторых компиляторах.
      read (In, *) O, P, Q
      allocate (C(O, P, Q))
      ! Запись в память по строкам.
      read (In, *) ((C(i, :, j), i = 1, O), j = 1, Q)
   close (In)

   
   open (file=output_file, encoding=E_, newunit=Out)
      ! Вывод массива.
      write (Out, '("Массив:")')
      ! Программирование формата (Kf6.2) -- вывод строки из K вещественных чисел.
      fmt = "(" // K // "f6.2)" ! fmt == "(5f6.2)"
	  ! fmt = String_plus_int("(", K) // "f6.2)"
      write (Out, fmt) B
      ! Так лучше не программироват формат -- код подвержен ошибкам:
      ! write (fmt, '("(", i0, "f6.2)" )') K
	  ! fmt == "(5f6.2)"
      
      ! Вывод прямоугольной матрицы.
      write (Out, '(/, "Прямоугольная матрица:")')
      ! Программирование формата (Mf6.2) для вывода строки из M вещественных чисел.
      fmt = "("//M//"f6.2)"
      write (Out, fmt) (A(i, :), i = 1, N)
      ! Так лучше не программироват формат -- код подвержен ошибкам:
      ! write (fmt, '( "(", i0, "f6.2)" )') M
      
      ! Вывод трёхмерной матрицы.
      write (Out, '(/, "Трёхмерная матрица:")')
      ! Программирование формата (O(Pf7.2, /)) для вывода O строк из P вещественных чисел
      ! и пустой строки на конце.
      fmt = "(" // O // "(" // P // "f7.2, /))" ! fmt = "(<O>(<P>f7.2, /))"
     !fmt = "(<Q>(<O>(<P>f7.2, /), /))"
      write (Out, fmt) ((C(i, :, j), i = 1, O), j = 1, Q)
      !write (Out, fmt) C(1, :, 1), ..., C(O, :, 1), (C(i, :, 2), i = 1, O), ..., (C(i, :, Q), i = 1, O)
      ! Так лучше не программироват формат -- код подвержен ошибкам:
      ! write (fmt, '( "(", i0, "(", i0, "f7.2, /))" )') O, P
   close (Out)
end program exercise_io
