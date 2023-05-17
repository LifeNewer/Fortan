program lab1_4
    ! Определить средний возраст юношей
    ! фамилия <-> и.о. <-> пол <-> год рождения
    !   15    <->   5  <->  1  <->     4

    ! ОСОБЕННОСТИ
    ! массив структур, файлы записей, модули из лабораторной 1.3
    ! рекурсия
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

    ! создаем неформатированный файл
    call create_data_file(input_file, data_file)

    ! читаем в массив записи
    humans = read_human_list(data_file)

    ! субрутина для вывода исходного списка
    call output_human_list(output_file, humans, "Исходные данные:", "rewind")

    ! получаем средний возраст
    ! вторая переменная нужна для рекурсивной субрутины
    average_year = make_average_year_per_men(humans, LIST_LEN)
    ! вывод среднего возраста
    call output_average_year(average_year, output_file)

end program lab1_4
