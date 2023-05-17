program lab1_2
    ! Определить средний возраст юношей
    ! фамилия <-> и.о. <-> пол <-> год рождения
    !   15    <->   5  <->  1  <->     4

    ! ОСОБЕННОСТИ
    ! массив символов
    ! внутренние процедуры головной программы
    use Environment

    integer(I_), parameter               :: LIST_LEN = 15, SURNAME_LEN = 15, &
            INITIALS_LEN = 5, GENDER_LEN=1, BORN_YEAR_LEN=4

    ! Массивы фамилий, инициалов, пола, года рождения
    character(kind=CH_)                 :: surnames(LIST_LEN, SURNAME_LEN)   = ""
    character(kind=CH_)                 :: initials(LIST_LEN, INITIALS_LEN)  = ""
    character(kind=CH_),    target      :: gender(LIST_LEN, GENDER_LEN)      = ""
    integer(I_),            target      :: born_year(LIST_LEN)               = 0

    ! другие переменные
    character(:), allocatable           :: input_file, output_file, format
    integer(I_)                         :: In=1, Out=0, i=-1, average_year=0 ! последняя перменная используется для подсчета среднего возраста

    ! Указатели
    character(kind=CH_), pointer        :: ptr_gender(:, :)
    integer(I_), pointer                :: ptr_born_year(:)

    ! Присваивание указателей
    ptr_gender      => gender(:, :)
    ptr_born_year   => born_year(:)

    ! Название выходного и входного файлов
    input_file = "../data/input.txt"
    output_file = "output.txt"

    ! Чтение входных данных из файла !!в массив символов!!!
    open (file=input_file, encoding=E_, newunit=In)
        ! формат следующий: считываем 15 символов -> 15a1
        ! считываем 1 пробел, никуда его не добавляя -> 1x
        ! считать 5 символов -> 5a1
        ! считываем 1 пробел, никуда его не добавляя -> 1x
        ! считать 1 символов -> 1a1
        ! считать число, состаящее из 4рех цифр -> i4
        format = '(15a1, 1x, 5a1, 1x, 1a1, 1x, i4)'
        read (In, format) (surnames(i, :), initials(i, :), gender(i, :), born_year(i), i = 1, LIST_LEN)
    close (In)


    average_year = make_average_year_per_men(LIST_LEN, ptr_gender, ptr_born_year)

    ! Вывод исходного списка
    open (file=output_file, encoding=E_, newunit=Out)
        format = '(a, i2)'
        write (out, format, advance="no") "Средний возраст юношей в группе: ", average_year

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
    pure integer function make_average_year_per_men(list_len, gender, born_year)
        ! Входные переменные
        integer(I_),            intent(in)  :: list_len, born_year(:)
        character(kind=CH_),    intent(in)  :: gender(:, :)
        ! Внутренние переменные
        logical, allocatable                :: mask(:)
        character(kind=CH_),    parameter   :: MALE = Char(1052, CH_)
        integer(I_),            parameter   :: NOW_YEAR=2021

        allocate(mask(list_len))
        mask = .false.

        do concurrent (i = 1:list_len)
            ! массив состоит лишь из 1-го элемента, буквы пола, поэтому делаем через выборку (i, 1)
            ! изначально можно без этого, но того требует задание, "работа с массивом символов"
            ! нельзя (i,:), т.к. получим массив, а не символ
            if (gender(i,1) == MALE) mask(i) = .true.
        end do

        make_average_year_per_men = NOW_YEAR - sum(born_year, mask) / count(mask)
    end function make_average_year_per_men
end program lab1_2
