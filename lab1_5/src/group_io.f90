module Group_IO
    use Environment

    implicit none

    ! Длины данных
    integer(I_), parameter  :: LIST_LEN         = 15
    integer(I_), parameter  :: SURNAME_LEN      = 15
    integer(I_), parameter  :: INITIALS_LEN     = 5
    integer(I_), parameter  :: GENDER_LEN       = 1
    integer(I_), parameter  :: BORN_YEAR_LEN    = 1

    ! Структура данных для хранения данных о человеке
    ! добавлен указатель на следующий элемент однонаправленного списка
    type human
        character(SURNAME_LEN, kind=CH_)        :: surname      = ""
        character(INITIALS_LEN, kind=CH_)       :: initials     = ""
        character(kind=CH_)                     :: gender       = ""
        integer(I_)                             :: years        = 0
        type(human), pointer                    :: next         => Null()
    end type human

contains
    ! читает с файла записи, является по сути функцией-оберткой, в себе вызывает функцию чтения записей
    function read_human_list(input_file) result(humans)
        ! входные переменные
        type(human), pointer       :: humans
        character(*), intent(in)   :: input_file

        ! локальные переменные
        integer(I_) :: In=1

        ! открываем поток чтения с файла, ссылаемся ссылкой на функцию, которая должна возвращать структуру
        open (file=Input_File, encoding=E_, newunit=In)
            humans => read_human(In)
        close (In)
    end function read_human_list

    ! Чтение человека
    ! как это происходит:
    ! 1) аллоцируем новую ссылку нашего типа данных human
    ! 2) задаем формат чтения с файла
    ! 3) считываем с файла по формату все данные в стркутуру, обязательно указывать поля, т.к.
        ! структура содержит в себе еще и поле-ссылку на следующую структуру
    ! 4) вызываем проверку статуса ввода-вывода, если нет ошибок, значит есть еще строчки в файле, а значит
        ! можно считывать дальше, присваиваем указателю в структуре новую функцию чтения, эту же функцию
        ! если же IO вернет EOF или другое - деаллоцируем поле-ссылку и заканчиваем выполнение
        ! тем самым у нас получается ссылка на голову списка, по которой, рекурсивно,
        ! можно перемещаться до конца
    recursive function read_human(In) result(human_one)
        type(human),    pointer     :: human_one
        integer,        intent(in)  :: In
        integer(I_)                 :: IO
        character(:), allocatable   :: format

        allocate (human_one)
        format = '(2(a, x), a, x, i4)'
        read (In, format, iostat=IO) human_one%surname, human_one%initials, &
                human_one%gender, human_one%years
        call Handle_IO_status(IO, "reading line from file")
        if (IO == 0) then
            human_one%next => read_human(In)
        else
            deallocate (human_one)
        end if
    end function read_human

    ! вывод списка людей
    ! как происходит:
        ! 1) входные параметры - название файла для вывода, указатель на однонаправленный лист
        ! 2) заголовок, который будет выведен перед выводом основго списка, позиция при записи в файл
    ! является по сути также функцией-оберткой, основная функция записи будет вызвана в данной
    subroutine output_human_list(output_File, humans, list_name, position)
        character(*),   intent(in)      :: output_File, position, list_name
        type(human),    intent(in)      :: humans
        integer(I_)                     :: Out=0

        open (file=Output_File, encoding=E_, position=position, newunit=Out)
            write (out, '(/a)') List_Name
            call output_student(Out, humans)
        close (Out)
    end subroutine output_human_list

    ! основная функция записи, рекурсивно
    ! как это сделано?
        ! 1) записываем структуру по заданному формату
        ! 2) проверяем, существует ли следующая структура
        ! 3) если структура существует, то снова вызываем функцию, передав уже ссылку на следующую структуру
                ! если ссылки не существует, то заканчивает работу
    recursive subroutine output_student(Out, human_one)
        integer,        intent(in)      :: Out
        type(human),    intent(in)      :: human_one

        integer  :: IO
        character(:), allocatable  :: format

        format = '(2(a, x), a, x, i4)'
        write (Out, format, iostat=IO) human_one%surname, human_one%initials, &
                human_one%gender, human_one%years
        call Handle_IO_status(IO, "writing human")
        if (Associated(human_one%next)) &
                call Output_student(Out, human_one%next)
    end subroutine Output_student

    ! вывод среднего возраста мужчин в списке
    ! функция как в лабораторной 1.4
    subroutine output_average_year(year, output_file)
        ! входные переменные
        character(*),   intent(in)      :: output_file
        integer(I_),    intent(in)      :: year
        ! локальные переменные
        integer(I_)                     :: Out=0
        character(:),   allocatable     ::  format

        open (file=output_File, encoding=E_, position="append", newunit=Out)
            format = '(a, i2)'

            write (out, format, advance="no") "Средний возраст юношей в группе: ", year
            if (year > 4 .and. year < 21) then
                write(out, *) "лет"
            else if (mod(year, 10) == 1) then
                write(out, *) "год"
            else if (mod(year, 10) > 1 .and. mod(year, 10) < 5) then
                write(out, *) "года"
            else
                write(out, *) "лет"
            end if
        close (Out)
    end subroutine output_average_year
end module Group_IO

