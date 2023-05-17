module Group_Process
    use Environment
    use Group_IO

    implicit none

contains
    ! рекурсивная функция, вычисляющая сумму годов рождения мужчин и их кол-во
    ! как?
    ! 1) если поле гендер в структуре - мужчина, то
    ! 1.2) суммируем в переменную суммы его год рождения
    ! 1.3) суммируем в переменную кол-ва + 1
    ! 1.4) если существует ссылка на следующую структуру - вызываем снова эта функцию, куда передаем все
        ! те же параметры, но ссылка уже указывает на другую структуру
    ! 2) если гендер - женщина, то проверяем существует ли слеудующая структура и если да, то вызываем ее
    pure recursive subroutine get_sum_year_and_count_of_men(humans, acc, count)
        ! входные переменные
        type(human), intent(in)         :: humans
        integer(I_), intent(inout)      :: acc,count
        ! локальные переменные
        character(kind=CH_), parameter  :: MALE = Char(1052, CH_)

        if (humans%gender == MALE) then
            acc   = acc + humans%years
            count = count + 1
            if (associated(humans%next)) then
                call get_sum_year_and_count_of_men(humans%next, acc, count)
            end if
        else
            if (associated(humans%next)) then
                call get_sum_year_and_count_of_men(humans%next, acc, count)
            end if
        end if
    end subroutine get_sum_year_and_count_of_men

    ! возвращает выисленный средний возраст
    ! как?
    ! 1) вызываем субрутину, куда передаем ссылку на список, переменную для суммирования годов и
        ! и переменную для подсчета мужчин
    ! 2) вычисляем средний возраст, отбавляя от текущего года поделенную на кол-во мужчин,
        ! сумму их годов рождения
    pure subroutine make_average_year_per_men(humans, average_year)
        ! Входные переменные
        type(human),            intent(in)  :: humans
        integer(I_),            intent(out) :: average_year
        ! Внутренние переменные
        integer(I_),            parameter   :: NOW_YEAR=2021
        integer(I_)                         :: sum_of_born_year, count_men

        sum_of_born_year = 0
        count_men        = 0

        call get_sum_year_and_count_of_men(humans, sum_of_born_year, count_men)

        average_year = NOW_YEAR - sum_of_born_year / count_men
    end subroutine make_average_year_per_men
end module Group_Process