program lab1_1
    ! Определить средний возраст юношей
    ! фамилия <-> и.о. <-> пол <-> год рождения
    !   15    <->   5  <->  1  <->     4

    ! ОСОБЕННОСТИ
    ! массив строк
    use Environment

    integer(I_), parameter               :: LIST_LEN = 15, SURNAME_LEN = 15, &
            INITIALS_LEN = 5, GENDER_LEN=1, BORN_YEAR_LEN=4

    ! Массивы фамилий, инициалов, пола, года рождения, два последних специально помечены указателями для оптимизации памяти
    ! именно два последних массива будут использоваься для выявления среднего возраста юношей, остальное не нужно
    character(SURNAME_LEN, kind=CH_)    :: surnames(LIST_LEN)  = ""
    character(INITIALS_LEN, kind=CH_)   :: initials(LIST_LEN)  = ""
    character(kind=CH_), target         :: gender(LIST_LEN)    = ""
    integer(I_), target                 :: born_year(LIST_LEN) = 0

    ! другие переменные
    character(:), allocatable           :: input_file, output_file, format
    integer(I_)                         :: In=1, Out=0, i=-1, average_year=0 ! последняя перменная используется для подсчета среднего возраста

    ! Указатели (для экономии памяти, вместо огромных массивов передаем лишь ссылку на них)
    character(kind=CH_), pointer        :: ptr_gender(:)
    integer(I_), pointer                :: ptr_born_year(:)

    ! Присваивание указателей
    ptr_gender      => gender(:)
    ptr_born_year   => born_year(:)

    ! Название выходного и входного файлов
    input_file = "../data/input.txt"
    output_file = "output.txt"

    ! Чтение входных данных из файла
    open (file=input_file, encoding=E_, newunit=In)
        ! формат следующий: принять строку, пропусть пробел, принять строку и пропустить пробел пробел -> 2(a, x)
        ! считать строку, пропустить пробел -> a, x
        ! считать число, состаящее из 4рех цифр -> i4
        format = '(2(a, x), a, x, i4)'
        read (In, format) (surnames(i), initials(i), gender(i), born_year(i), i = 1, LIST_LEN)
    close (In)

    average_year = make_average_year_per_men(LIST_LEN, ptr_gender, ptr_born_year)

    ! Вывод исходного списка
    open (file=output_file, encoding=E_, newunit=Out)
        ! формат - вывести строку, после вывести число состоящее из двух цифр
        format = '(a, i2)'
        write (out, format, advance="no") "Средний возраст юношей в группе: ", average_year

        ! алгоритм для печатния "лет" "год" "года" для конкретного возраста
        ! mod(a,b) = a % b = остаток от деления
        if (average_year > 4 .and. average_year < 21) then
            write(out, *) "лет"
        else if (mod(average_year, 10) == 1) then
            write(out, *) "год"
        else if (mod(average_year, 10) > 1 .and. mod(average_year, 10) < 5) then
            write(out, *) "года"
        else
            write(out, *) "лет"
        end if
    close (Out)

contains
    ! pure - гарантирует субрутину без побочных эффектов
    pure integer function make_average_year_per_men(list_len, gender, born_year)
        ! Входные переменные
        integer(I_),            intent(in)  :: list_len, born_year(:)
        character(kind=CH_),    intent(in)  :: gender(:)
        ! Внутренние переменные
        logical, allocatable                :: mask(:) ! нужна для определения возрастов именно юношей
            ! ввиду проблемы с кодировками, мы насильно указываем кодировку и размерность одного символа
            ! для правильного отображение русских букв
            ! parametr - константность, нужна для устранения побочных эффектов
        character(kind=CH_),    parameter   :: MALE = Char(1052, CH_)
        integer(I_),            parameter   :: NOW_YEAR=2021

        ! выделяем место в памяти под маску, размер маски соответсвует размеру участников и размерам каждого из массивов
        allocate(mask(list_len))
        mask = .false.

        ! специальный тип цикла, необходим для распараллеливания на несколько потоков процессов
        ! данные не связанные и не имеют зависимости друг от друга, значит их можно распараллелить
        ! это все задается специальным словом concurrent
        do concurrent (i = 1:list_len)
            if (gender(i) == MALE) mask(i) = .true.
        end do
        ! указываем функции суммы маску, чтобы посчитались лишь те элементы, где в маске true,
        ! т.е. только юноши
        ! count - используется для подсчета значений true в маске, именно на столько юношей мы должны разделить сумму их
        ! годов рождения для получения среднего года рождения
        ! вычитаем из 2021, текущего года, для получения среднего возраста
        make_average_year_per_men = NOW_YEAR - sum(born_year, mask) / count(mask)
    end function make_average_year_per_men

end program lab1_1
