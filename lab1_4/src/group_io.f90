module Group_IO
    use Environment

    implicit none ! без неявных средств, без неявных инициализаций также

    ! Длины данных
    integer(I_), parameter  :: LIST_LEN         = 15
    integer(I_), parameter  :: SURNAME_LEN      = 15
    integer(I_), parameter  :: INITIALS_LEN     = 5
    integer(I_), parameter  :: GENDER_LEN       = 1
    integer(I_), parameter  :: BORN_YEAR_LEN    = 1

    ! Структура данных для хранения данных о человеке
    type human
        character(SURNAME_LEN, kind=CH_)        ::  surname     = ""
        character(INITIALS_LEN, kind=CH_)       ::  initials    = ""
        character(kind=CH_)                     ::  gender      = ""
        integer(I_)                             ::  years       = 0
    end type human

contains
    ! Создание неформатированного файла данных.
    subroutine create_data_file(input_file, data_file)
        ! входные переменные
        character(*), intent(in)   :: input_file, data_file
        ! локальные переменные
        type(human)                :: one_human
        integer                    :: In=1, Out=0, IO, i=-1, recl=-1
        character(:), allocatable  :: format

        ! принцип работы:
        ! 1) открваем файл на вход (In)
        ! 2) создам recl - длину одной строки в неформатированном файле данных
            ! стоит помнить, что символьный тип кодируется не стандартно, так что умножаем на столько
            ! сколько занимает этот тип байт
            ! во входном файле у нас лишь 1 цифра, поэтому просто прибавляем 1 раз столько символом
            ! типа integer, сколько занимает эта цифра
        ! 3) открываем файл на выход (out), задааем ему параметр form="unformatted" означающий
            ! что файл будет неформатированным. задаем ему acces="direct" - означающий читать файл по
            ! переменной recl, из-за этого также указываем сам recl, т.е. длину одной строки
        ! 4) создаем строку формата, то, как мы будем считывать данные из обычного(!) файла
        ! 5) в цикле с 1 до конца считываем с обычного файла в созданный нами тип одну запись,
            ! проверяем чтобы не было ошибок чтения записи (функция написана в environment) считаем ее встроенной
            ! записываем при помощи reс=i - перменную нашего созданного типа
            ! rec=i означает, что каждая запись будет гарантирована написана в новой как бы строке
            ! неформатированного файла данных
        ! 6) проверяем на ошибки IO и идем так до конца цикла
        open (file=Input_File, encoding=E_, newunit=In)
            recl = (SURNAME_LEN + INITIALS_LEN + GENDER_LEN)*CH_ + I_
            open (file=data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
            format = '(2(a, x), a, x, i4)'
                do i = 1, LIST_LEN
                    read (In, format, iostat=IO) one_human
                    call Handle_IO_status(IO, "reading formatted class list, line " // i)
                    write (Out, iostat=IO, rec=i) one_human
                    call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
                end do
            close (In)
        close (Out)
    end subroutine Create_data_file

    ! Чтение списка с неформатированного файла
    ! result(humans) - выходная переменная из фукнции
    function read_human_list(data_file) result(humans)
        type(human)                :: humans(LIST_LEN)
        character(*), intent(in)   :: data_file

        integer(I_) :: In=1, IO, recl=-1

        ! особенность recl в том, что кол-во символов одной строки нужно умножить на общее
        ! количество, т.к. нужно дать функции open всю длинну нашего неформатированного файла
        recl = ((SURNAME_LEN + INITIALS_LEN + GENDER_LEN)*CH_ + I_) * LIST_LEN
        open (file=data_file, form='unformatted', newunit=In, access='direct', recl=recl)
            ! rec=1 - означает считать лишь одну строку за раз
            read (In, iostat=IO, rec=1) humans
                call Handle_IO_status(IO, "reading unformatted class list")
        close (In)
    end function read_human_list

    ! Вывод списка класса.
    subroutine output_human_list(output_File, humans, list_name, position)
        character(*), intent(in)    :: output_file, position, list_name
        type(human), intent(in)     :: humans(:)

        integer                     :: Out, IO
        character(:), allocatable   :: format

        ! переменная position нужна для более гибкого управления функцией, через нее можно задать:
        !   а) стереть и начать сначала
        !   б) дописать в конец
        open (file=output_File, encoding=E_, position=position, newunit=Out)
            write (out, '(/a)') list_name
            format = '(2(a, x), a, x, i4)'
            write (Out, format, iostat=IO) humans
            ! вызов функции проверки IO, если ошбика - укажет определенно в чем она
            call Handle_IO_status(IO, "writing " // List_name)
        close (Out)
    end subroutine output_human_list

    ! вывод среднего возраста мужчин в списке
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

