module Group_Process
    use Environment
    use Group_IO

    implicit none

contains
    pure recursive subroutine get_sum_and_size_of_men_born_year(sum_of_born_year, count_men, humans, iterator)
        ! входные переменные
        integer(I_),    intent(inout)   :: sum_of_born_year, count_men
        type(human),    intent(in)      :: humans(:)
        integer(I_), intent(in) :: iterator
        ! локальные переменные
        character(kind=CH_),    parameter   :: MALE = Char(1052, CH_)

        if (iterator >= 0) then
            if (humans(iterator)%gender == MALE) then
                sum_of_born_year = sum_of_born_year + humans(iterator)%years
                count_men = count_men + 1
                call get_sum_and_size_of_men_born_year(sum_of_born_year, count_men, humans, iterator-1)
            else
                call get_sum_and_size_of_men_born_year(sum_of_born_year, count_men, humans, iterator-1)
            end if
        end if

    end subroutine get_sum_and_size_of_men_born_year


    pure integer function make_average_year_per_men(humans, list_len)
        ! Входные переменные
        type(human),            intent(in)  :: humans(:)
        integer(I_),            intent(in)  :: list_len
        ! Внутренние переменные
        integer(I_),            parameter   :: NOW_YEAR=2021
        integer(I_)                         :: sum_of_born_year, count_men

        ! инициализиурем переменные, в pure - нельзя инициализировать обычные переменные при объявлении
        sum_of_born_year = 0
        count_men = 0

        ! вызываем рекурсивную(!) субрутину, что вернет нам сумма мужских годов рождения и кол-во мужчин
        ! принимает: переменную для суммы, переменную для кол-ва, сам массив, число экземпляром
        call get_sum_and_size_of_men_born_year(sum_of_born_year, count_men, humans, list_len)
        make_average_year_per_men = NOW_YEAR - sum_of_born_year / count_men
    end function make_average_year_per_men
end module Group_Process