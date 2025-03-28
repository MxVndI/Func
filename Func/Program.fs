open System

printfn "Hello from F#"

type SolveResult =
    None
    | Linear of float
    | Quadratic of float*float

let solve a b c = 
    let D = b*b-4. * a * c
    if a =0. then
        if b=0. then None
        else Linear(-c/b)
    else
        if D<0 then None
        else Quadratic(((-b+sqrt(D))/(2.* a), (-b-sqrt(D))/(2.*a)))

let circleArea r =
    Math.PI * r * r

let cylinderVolume area height =
     area * height * 1.

let volumeCylinderSuperPos =
    circleArea >> cylinderVolume

let volumeCylinderCurr radius height =
    (circleArea radius) * height

let rec sumDigitsUp n =
    if n = 0 then 0
    else n % 10 + sumDigitsUp (n / 10)

let sumDigitsDown num =
    let rec sumCounter sum num =  
        if num = 0 then sum
        else sumCounter (sum + num % 10) (num / 10)
    sumCounter 0 num

[<EntryPoint>]
let main argv =
    let res = solve 1. 2. -3.
    System.Console.WriteLine(res)

    System.Console.WriteLine("Введите радиус круга:")
    let radius = float (Console.ReadLine()) |> float
    
    System.Console.WriteLine("Введите высоту цилиндра:")
    let height = float (Console.ReadLine()) |> float

    Console.WriteLine($"Площадь круга: {circleArea radius}")
    Console.WriteLine($"Объем цилиндра: {volumeCylinderSuperPos radius height} \n")

    Console.WriteLine($"Площадь круга: {circleArea radius}")
    Console.WriteLine($"Объем цилиндра: {volumeCylinderCurr radius height}")

    Console.Write("Введите число: ")
    let num = Console.ReadLine() |> int
    Console.WriteLine($"Сумма цифр числа (верхняя рекурсия): {sumDigitsUp num}")
    Console.WriteLine($"Сумма цифр числа (нижняя рекурсия): {sumDigitsDown num}")

    0
    