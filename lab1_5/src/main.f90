program lab1_5
    ! Определить средний возраст юношей
    ! фамилия <-> и.о. <-> пол <-> год рождения
    !   15    <->   5  <->  1  <->     4

    ! ОСОБЕННОСТИ
    ! модули
    ! хвостовая рекурсия
    ! односвязные списки
    use Environment
    use Group_Process
    use Group_IO

    implicit none

    character(:), allocatable                       :: input_file, output_file
    ! из-за того, что список неизвестной длины - нуллим изначательный указатель в любом случае
    ! чтобы в нем не было мусора
    ! МОДИФИЦИРОВАЛ ТИП ДАННЫХ human, добавив указатель на следующую такую структуру
    type(human), pointer                            :: humans => Null()
    integer(I_)                                     :: average_year=0

    ! Название выходного и входного файлов
    input_file = "../data/input.txt"
    output_file = "output.txt"

    ! функция вернет ссылку на первую структуру в однонаправленном листе, перемещаться по которому можно по
    ! полю next в самой структуре, т.к. она является ссылкой на следующую стркутуру или null
    humans => Read_human_list(input_file)

    ! если ссылка существует, то
    if (Associated(humans)) then
        ! выводим в файл исходный список
        call output_human_list(output_file, humans, "Исходный список:", "rewind")

        ! получаем средний возраст мужчин и выводим его
        call make_average_year_per_men(humans, average_year)
        call output_average_year(average_year, output_file)
    end if

end program lab1_5
