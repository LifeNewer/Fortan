program task6_1v
    ! вычислить с заданной абсолютной погрешностью ABSERR
    ! e^x = 1 + x/1! + x^2/2! +  x^3/3! ... пока не будет удовлетворять абсолютной погрешности

    ! абсолютная погрешность выисляется как модуль точного значения числа - приближенное число
    ! воспльзуемся математической формулой exp(x) - возвращающей e^x, как число, которое вычисленно точно, тем самым
    ! абсолютная погрешность будет равна = |exp(x) - наш ряд| > заданная погрешность
    use Environment

    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer(I_)             :: In=0, Out=0
    double precision        :: abserr=0., x=0. ! формат двойного float, чтобы посчитать более точно

    open (file=input_file, newunit=In)
        read (In, *) x, abserr ! считываем x и погрешность
    close (In)

    open (file=output_file, newunit=Out)
        ! формат строки вывода: a50 - 50 знаков символа, f40.35, всего 40 цифр, 35 после запятой. f - для вещественных чисел, a - для символов (текста)
        write (*,"(A50,f40.35)")   "Абсолютная погрешность = ", abserr
        write (*,"(A17,f15.12)")  "my exp^x   = ", check_exp(x, abserr)
        write (*,"(A17,f15.12)")  "math exp^x = ", exp(x)
    close (Out)

contains
    double precision function fact(n)
        integer, intent(in) :: n
        integer :: i

        fact = 1.0
        if (n /= 1) then
            do i = 2, n
                fact = fact * i
            end do
        end if

    end function fact

    double precision function new_variant_for_exp(root, pos)
        double precision,   intent(in)   :: root
        integer,            intent(in)   :: pos

        ! получаем новый член прогрессии x^pos/pos!

        new_variant_for_exp = (root**pos / fact(pos))
    end function new_variant_for_exp

    double precision function check_exp(root, area_of_root)
        double precision,   intent(in)  :: root, area_of_root
        double precision                :: math_exp=0.0, local_exp=1.0, new_x_local=0.0
        integer                         :: i_f=1

        math_exp = exp(root)
        ! e^x = 1 + x/1! + x^2/2! +  x^3/3!

        do while (math_exp - local_exp > area_of_root) ! вычитаем наш ряд от правильного значения и сравниваем с погрешностью
            new_x_local = new_variant_for_exp(root, i_f) !получаем новую переменную ряда
            local_exp = local_exp + new_x_local ! суммируем с рядом
            i_f = i_f + 1 ! для следующего члена ряда
        end do
        check_exp = local_exp
    end function check_exp
end program task6_1v
