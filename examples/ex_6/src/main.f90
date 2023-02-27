program exercise_6
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: sin_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   !sin_x = SinXImp(x)
   sin_x = SinX(x)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Sin(x)", sin_x, "Fortran Sin(x)", Sin(x), "Error", sin_x - Sin(x)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   real(R_) pure function SinXImp(x) result(SinX)
      real(R_), intent(in) :: x
      
      ! 2pi == 2 * 4*Arctg(1), т. к. Tg(1) = pi/4 => pi == 4*Arctg(1)
      real(R_), parameter :: double_PI = 8 * Atan(1._R_)
      real(R_)    r, q, x_s, x_2, OldSinX
      integer     n

      ! Делим x по модулю 2*pi, т. к. Sin(2*pi*n+a) = Sin(a).
      x_s = Mod(x, double_PI)
      
      ! Чтобы не вычислять каждый раз.
      x_2   = x_s * x_s
      
      n     = 0
      r     = x_s
      SinX  = r

      ! Цикл с постусловием: пока сумма не перестанет меняться.
      do
         n        = n + 2
         q        = - x_2 / (n*(n + 1))
         r        = r * q
         OldSinX  = Sinx
         SinX     = SinX + r
         if (OldSinX == Sinx) exit
      end do
      !print "('Число членов суммы: ', i0)", n / 2 + 1
      !print "('Число итераций: ', i0)", n / 2
   end function SinXImp

   ! Чистая функция в регулярном стиле.
   ! * При возникновении сложностей реализовывать необязательно.
   ! До цикла с использованием векторных операций вычисляются 5 членов суммы,
   ! а на каждой итерации -- 4 члена суммы.
   real(R_) pure function SinX(x)
      real(R_), intent(in) :: x
      
      ! 2pi == 2 * 4*Arctg(1), т. к. Tg(1) = pi/4 => pi == 4*Arctg(1)
      real(R_), parameter :: double_PI = 8 * Atan(1._R_)
      ! Относительная погрешность вычислений.
      real(R_) R(4), Numerators(4), Denominators(4), n_fact, x_s, x_8
      integer  Ns(8)

      ! Делим x по модулю 2*pi, т. к. Sin(2*pi*n+a) = Sin(a).
      x_s = Mod(x, double_PI)
      
      ! Получение суммы СРАЗУ первых 4-ёх членов (со 2-ого по 5-ый),
      ! а затем в цикле расчитываем СРАЗУ по 4 члена за итерацию.

      ! 1. Вычисление числителей: -x^3, x^5, -x^7, x^9.
      !    На этапе компиляции за счёт оптимизации компилятором.
      Numerators = x_s ** [3, 5, 7, 9]
      Numerators = Numerators * [-1, 1, -1, 1] ! ВЕКТОРИЗАЦИЯ
      
      ! Вычисление x^8 для будущих вычислений числителей (получаем из x^9).
      x_8 = Numerators(4) / x_s
      
      ! 2. Вычисление знаменателей-факториалов: 3!, 5!, 7!, 9!.
      !    На этапе компиляции за счёт оптимизации компилятором.
      Denominators = [2*3, 2*3*4*5, 2*3*4*5*6*7, 2*3*4*5*6*7*8*9]
      ! Вычисление всех необходимых множителей для этих знаменателей.
      ! Будет использовано при вычислении очередных 4-ёх знаменателей.
      Ns = [2, 4, 6, 8, 3, 5, 7, 9]

      ! 3. Вычисление суммы членов со 2-ого по 5-ый.
      R = Numerators / Denominators ! ВЕКТОРИЗАЦИЯ

      ! 4. Сумма первых 5-ти членов.
      SinX = x_s + Sum(R)
      
      ! Цикл с постусловием: пока сумма не перестанет меняться из-за последнего члена.
      ! На каждой итерации цикла проводится вычисление СРАЗУ 4-ёх членов суммы. 
      do while (SinX + R(4) /= SinX)
         ! Получение СРАЗУ 4-ёх очередных членов суммы.
         
         ! 1. Вычисление очередных 4-ёх числителей.
         ! Например, на первой итерации Numerators == [-x^3, x^5, -x^7, x^9] и,
         ! умножая его на x^8, получаем следующие числители [-x^11, x^13, -x^15, x^17]. 
         Numerators = Numerators * x_8

         ! 2. Вычисление очередных знаменателей-факториалов: (n+2)!, (n+4)!, (n+6)!, (n+8)!
         !     1) Запись накопленного факториала для будущих вычислений знаменателей.
         !        Например, на первой итерации n_fact == 9!.
         n_fact = Denominators(4)
      
         !     2) Вычисление всех необходимых множителей для очередных 4-ёх знаменателей:
         !        [n+1, n+3, n+5, n+7, n+2, n+4, n+6, n+8]
         !        Например, на первой итерации Ns == [2, 4, 6, 8, 3, 5, 7, 9] и,
         !        прибавляя 8, получаем необходимые множители для очередных 4-ёх знаменателей:
         !        [10, 12, 14, 16, 11, 13, 15, 17]
         Ns = Ns + 8
         
         !     3) Получение множителей:
         !        [(n+1)*(n+2), (n+3)*(n+4), (n+5)*(n+6), (n+7)*(n+8)] ==
         !        == [n+1, n+3, n+5, n+7] * [n+2, n+4, n+6, n+8]
         Denominators = Ns(1:4) * Ns(5:8) ! ВЕКТОРИЗАЦИЯ
         !     4) Получение (n+2)! == n! * (n+1)*(n+2).
         Denominators(1) = n_fact * Denominators(1)
         !     5) Вычисление знаменателей: (n+4)!, (n+6)!, (n+8)!.
         Denominators(2) = Denominators(1) * Denominators(2) ! == (n+4)! == (n+2)! * (n+3)*(n+4)
         Denominators(3) = Denominators(2) * Denominators(3) ! == (n+6)! == (n+4)! * (n+5)*(n+6)
         Denominators(4) = Denominators(3) * Denominators(4) ! == (n+8)! == (n+6)! * (n+7)*(n+8)
         
         ! 3. Вычисление очередных 4-ёх членов суммы.
         R = Numerators / Denominators

         ! Прибавление очередных 4-ёх членов к накапливаемой сумме.
         SinX = SinX + Sum(R)
      end do
      !print "('Число членов суммы: ', i0)", (Ns(8)+1) / 2
      !print "('Число итераций: ', i0)", (Ns(8)-1) / 8
   end function SinX 
end program exercise_6
