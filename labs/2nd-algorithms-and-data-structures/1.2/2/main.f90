! Баранов К.П., 20021, ЛР 1, вариант 2, средства 2.
! Средства:
!   - массивы строк
!   + массивы символов
!   + внутренние процедуры головной программы
!   - массив структур или структура массивов (попробовать и сравнить оба)
!   - файлы записей
!   * модули
!   - хвостовая рекурсия
!   - однонаправленные списки заранее неизвестной длины
!   + регулярное программирование.

! Необходимо прочитать список известной длины из не менее чем 12 строк.
! Данные в одной строке имеют заданный формат и отделяются друг от друга дополнительным пробелом.

! Дан список группы в виде:
!   ФАМИЛИЯ   И.О.    ПОЛ   ПРОПИСКА СРЕДНИЙ_БАЛЛ
!   15 симв. 5 симв. 1 симв. 1 симв.   4 симв.
!   Пример входного файла (в графе прописки буква П стоит у петербурцев, С — у гостей Санкт-Петербурга):
!   Иванов И. И. М П 4.35
!   Отсортировать по убыванию среднего балла по отдельности списки петербуржцев и гостей Санкт-Петербурга.
!   Пример выходного файла:
!   Петербуржцы:
!   Иванов И. И. М 4.35
!   Барабашкин И. И. М 4.32
!   Гости города:
!   Петров И. И. М 4.35
!   Петрыкин И. И. М 4.32

program sort_students
   use Environment

   implicit none

   integer, parameter               :: students_count = 18, surname_length = 15, initials_length = 5, marks_count = 5
   character(kind=CH_), parameter   :: male = Char(1052, CH_)  ! CH__"\u1052" CH__"М"

   character(:), allocatable  :: input_file, output_file, format

   ! Массивы фамилий, инициалов, полов, оценок и средних оценов и временные
   ! переменные для обменов при сортировке.
   character(surname_length, kind=CH_)                :: surname_tmp = "", surnames(students_count) = ""
   character(surname_length, kind=CH_), allocatable   :: surnames_male(:), surnames_female(:)
   
   character(initials_length, kind=CH_)               :: initials_tmp = "", initials(students_count) = ""
   character(initials_length, kind=CH_), allocatable  :: initials_male(:), initials_female(:)
   
   character(kind=CH_)                             :: gender(students_count) = ""
   
   integer                                         :: marks_tmp(marks_count) = 0, marks(students_count, marks_count) = 0
   integer, allocatable                            :: marks_male(:, :), marks_female(:, :), position_male(:), position_female(:)
   
   real(R_)                                        :: avg_mark_tmp = 0, avg_marks(students_count) = 0
   real(R_), allocatable                           :: avg_marks_male(:), avg_marks_female(:)

   logical, allocatable                            :: is_male(:), is_female(:)
   integer                                         :: male_count = 0, female_count = 0

   integer :: in, out, io, i, j
   integer, parameter                              :: indexes(*) = [(i, i = 1, students_count)]
   logical :: swap

   ! Code goes brrrr...
end program sort_students
