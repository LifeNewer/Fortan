program task3_2
    ! найти факториал m = k!, предусмотреть проверки
    use Environment

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)             :: In=0, Out=0, m=1, k=0, i=0

    open (file=input_file, newunit=In)
        read (In, *) k
    close (In)

    ! проверка на то, чтобы переменная не равнялась меньше нуля
    if (k < 0) then
        print *, "меньше нуля"
        m = -1
    else if (k == 0) then
        print *, "zero"
        ! если факториал равен нулю - ответ 1
        m = 1
    else
        ! считаем факториал
        do i=1, k
            m = m * i
        end do
    end if

    open (file=output_file, newunit=Out)
        write(Out, *) "m = ", m
    close (Out)
end program task3_2
