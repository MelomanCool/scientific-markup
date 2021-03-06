#lang pollen

◊h1{Задание 1}

◊h2{Правило представления чисел в дополнительном коде}

◊${
    Доп(x) =
    ◊piecewise{
        x,          & если \quad x \geq 0 \\
        2^k - |x|,  & если \quad x < 0
    }
}

Перепишем правило для ◊${ k = 8 }.

◊${ 2^8 = 256

    Доп(x) =
    ◊piecewise{
        x,          & если \quad x \geq 0 \\
        256 - |x|,  & если \quad x < 0
    }
}


◊h2{Перевод числа из дополнительного кода в обычное представление}

1. Сравнить дополнительный код ◊${ Доп(x) } с величиной наибольшего неотрицательного числа рассматриваемого (типа) размера, равного ◊${ (2^{k-1} - 1) }.
2. Если ◊${ Доп(x) < 2^{ k - 1 } - 1 }, то ◊${ x = Доп(x) },
   иначе ◊${ |x| - 2^k - Доп(x) }, а ◊${ x } --- отрицательно.

Перепишем правило для ◊${ k = 8 }.

◊${ 2^7 = 128 }.

1. Сравнить дополнительный код ◊${ Доп(x) } с величиной наибольшего неотрицательного числа рассматриваемого (типа) размера, равного ◊${ 127 }.
2. Если ◊${ Доп(x) < 127 }, то ◊${ x = Доп(x) },
   иначе ◊${ |x| = 256 - Доп(x) }, а ◊${ x } --- отрицательно.


◊h2{Сложение целых чисел}

◊${
    Сумма(x, y) =
    (x + y) \mod 2^k =
    ◊piecewise{
        x + y,      & если \quad x + y < 2^k,   & CF=0 \\
        x + y - 2^k,    & если \quad x + y \geq 2^k,    & CF=1
    }
}

Перепишем для ◊${ k = 8 }:

◊${
    Сумма(x, y) =
    (x + y) \mod 256 =
    ◊piecewise{
        x + y,      & если \quad x + y < 256,   & CF=0 \\
        x + y - 256,    & если \quad x + y \geq 256,    & CF=1
    }
}


◊h2{Разность целых чисел}

◊${
    Сумма(x, y) =
    (x - y) \mod 2^k =
    ◊piecewise{
        x - y,      & если \quad x \geq y,  & CF=0 \\
        (x + 2^k) - y,  & если \quad x < y, & CF=1
    }
}


Перепишем для ◊${ k = 8 }:

◊${
    Сумма(x, y) =
    (x - y) \mod 256 =
    ◊piecewise{
        x - y,      & если \quad x \geq y,  & CF=0 \\
        (x + 256) - y,  & если \quad x < y, & CF=1
    }
}


◊h1{Задание 2}

Выполнить расчеты по схемам:

1. Аргументы --- беззнаковые числа типа байт
   a) ◊${ A + B }
   b) ◊${ B - A }
   c) ◊${ R + D }
   d) ◊${ R - D }
2. Аргументы --- знаковые числа типа байт
   a) ◊${ B + P }
   b) ◊${ P - B }
   c) ◊${ X + Y }
   d) ◊${ X - Y }
   e) ◊${ Z - P }
   f) ◊${ Z + P }

**Примечение:** каждый результат расчета дополнить указанием значения флага, информативного для конкретного случая

◊h2{Вариант 2}

◊quick-table{
    A  | B  |  D  |  R  |  P  |  X  | Y  |  Z
    32 | 67 | 189 | 167 | -32 | 122 | 67 | -35
}

◊${
    A + B = 32 + 67 = 99 \\
    B - A = 67 - 32 = 35 \\
    R + D = 167 + 189 = ... \\
    R - D = 167 - 189 = ... \\
}
