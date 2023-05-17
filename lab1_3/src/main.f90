program lab1_3
    ! Определить средний возраст юношей
    ! фамилия <-> и.о. <-> пол <-> год рождения
    !   15    <->   5  <->  1  <->     4

    ! ОСОБЕННОСТИ
    ! массив структур
    ! файлы записей
    ! модули
    use Environment
    use Group_Process   ! модуль, содержащий чистые функции, для решения задания
    use Group_IO        ! модуль, содержащий функции ввода вывода

    implicit none

    character(:), allocatable                       :: input_file, output_file, data_file
    type(human)                                     :: humans(LIST_LEN)
    integer(I_)                                     :: average_year=0

    ! Название выходного и входного файлов, а также неформатированного файла
    input_file = "../data/input.txt"
    output_file = "output.txt"
    data_file   = "input.dat" ! неформатированный файл, иначе говоря raw-файл данных

    ! передаем входной файл и название неформатированного файла
    call create_data_file(input_file, data_file)

    ! вывод функции будет массив нашего созданного типа данных
    ! в функцию передаем лишь неформатированный файл
    humans = read_human_list(data_file)

    ! субрутина для вывода исходного списка
    ! выходной файл, массив структур, заголовок для вывода, позиция для записи
    call output_human_list(output_file, humans, "Исходные данные:", "rewind")

    ! функция вернет средний возраст мужчин, передаем лишь массив
    average_year = make_average_year_per_men(humans)
    ! вызываем субрутину для печати среднего возраста мужчин
    ! передаем переменную с числом и файл, куда выводить
    call output_average_year(average_year, output_file)

end program lab1_3
