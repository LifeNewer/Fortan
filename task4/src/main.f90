program task4_1v
    ! протабулировать функцию f(x)=x^2*tg(x) + sin(x)/x
    ! 0.01254 < x < 0.03254
    ! dx = 0.0002
    use Environment

    character(*), parameter     :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)                 :: In=0, Out=0
    real(R_)                    :: fx, low, high, delta

    ! нижняя граница, верхняя граница, дельта икс
    open (file=input_file, newunit=In)
        read (In, *) low, high, delta
    close (In)

    open (file=output_file, newunit=Out)
        ! записываем границы табулирования
        write(Out, *) "low  level:", low
        write(Out, *) "high level:", high

        ! пока нижняя граница не привысит верхнюю берем x = low, т.е. нижней границы
        do while (low <= high)
            fx = low**2 * tan(low) + sin(low)/low
            write(Out, *) "x = ", low, "f(x) = ", fx ! выводим ответ
            low = low + delta ! прибавляем на дельту икс
        end do
    close (Out)

end program task4_1v
