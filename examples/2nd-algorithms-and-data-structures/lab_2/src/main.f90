! Copyright 2015 Fyodorov S. A.

! Во входном файле F1 находится исходный код программы на Fortran, а в файле F2 —
! тот же код, но с добавлением некоторых строк. Сформировать файл из новых
! строк, пометив их в начале как «++ ».

program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3

   type(SourceLine), pointer :: InitialCode  => Null()   ! Первоначальный код.
   type(SourceLine), pointer :: ModdedCode   => Null()   ! Модифицированный код.
   type(SourceLine), pointer :: DiffCode     => Null()   ! Новые строки.

   F1 = "../data/source.f90"
   F2 = "../data/mod_source.f90"
   F3 = "source.f90.diff"
   
   InitialCode => Read_Source_Code(F1)
   ! call Output_Source_Code(F3, InitialCode)
   ModdedCode  => Read_Source_Code(F2)
   ! call Output_Source_Code(F3, ModdedCode)
  
   if (Associated(InitialCode) .and. Associated(ModdedCode)) then
      DiffCode => Diff_Codes(InitialCode, ModdedCode)
      
      if (Associated(DiffCode)) &
      call Output_Source_Code(F3, DiffCode)
   end if

end program reference_lab_2
