program task1_4
    ! Для заданного значения f вычислить по приближенной формуле значение ln(2+a)
    use Environment

    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                 :: In=0, Out=0
    real(R_)                    :: a=0.0, massive(3) = 0.0, log_2_plus_a
    real(R_), parameter         :: constant_part = 0.693147

    ! чтение из файла переменной а
    open (file=input_file, newunit=In)
        read (In, *) a
    close (In)

    ! по условию задания
    a = 2 + a
    ! a / (4+a) = t
    ! a^3 / (3 * (4+a)^3) = 1/3 * t^3
    ! a^5 / (5 * (4+a)^5) = 1/5 * t^5

    massive(1) = a / (4+a) ! t
    massive(2) = (1.0/3.0) * (massive(1)**3)
    massive(3) = (1.0/5.0) * (massive(1)**5)

    !Если возникнет вопрос с тем, что приблизителньое значение слишком далекое от реального
    !    massive(2) = a**3/(3*(4+a)**3)
    !    massive(2) = a**5/(5*(4+a)**5)

    log_2_plus_a = constant_part + 2 * sum(massive)

    ! открытие файла для записи результата
    open (file=output_file, newunit=Out)
        write(Out, *)  "1 elem  = ", massive(1)
        write(Out, *)  "2 elem  = ", massive(2)
        write(Out, *)  "3 elem  = ", massive(3)
        write(Out, *)  "ln(2+a) = ", log_2_plus_a

    close (Out)
end program task1_4
