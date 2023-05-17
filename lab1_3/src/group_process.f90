module Group_Process
    use Environment
    use Group_IO

    implicit none

contains
    pure integer function make_average_year_per_men(humans)
        ! Входные переменные
        type(human),            intent(in)  :: humans(:)
        ! Внутренние переменные
        character(kind=CH_),    parameter   :: MALE = Char(1052, CH_)
        integer(I_),            parameter   :: NOW_YEAR=2021

        ! формула проста:
        ! 1) отнимаем текущий год от
        ! 2) суммы годов рождения массива возрастов мужчин
            ! как это происходит:
            ! через pack, из массива типа данных, что мы создали, мы выбираем массив годов рождения
            ! так что для того возраста, что будет выбран - соответсвует что это мужчина
            ! далее через функцию sum получаем сумму годов рожения
        ! 3) поделенного на сумму все того же выбранный массив годов рождения по признаку гендера
            ! как это происходит:
            ! все также получаем массив годов рождения
            ! вместо сумму всех эл-тов этого массива используем size, т.е. считаем сколько
            ! в этом массиве элементов
        make_average_year_per_men = NOW_YEAR - &
                sum(pack(humans%years, humans%gender == MALE)) / &
                        size(pack(humans%years, humans%gender == MALE))
    end function make_average_year_per_men
end module Group_Process