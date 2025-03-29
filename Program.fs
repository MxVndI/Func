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

let rec countDivUp temp num =
    match num with
    | _ when temp > num -> 0
    | _ when num % temp > 0 -> (countDivUp (temp + 1) num)
    | _ when num % temp = 0 -> 1 + (countDivUp (temp + 1) num)


let rec countDivisorsDown num tmp count =
    match num with
    | _ when tmp>num -> count
    | _ when num % tmp > 0 -> countDivisorsDown num (tmp + 1) count
    | _ when num % tmp = 0 -> countDivisorsDown num (tmp + 1) (count + 1)
    
let ans b = 
    match b with
    | true -> (countDivUp 1)
    |_ -> sumDigitsUp


[<EntryPoint>]
let main argv =
    let res = ans true
    System.Console.WriteLine($"dasds {res}")

    let res1 = (ans true) 6
    System.Console.WriteLine(res1)
    
    Console.WriteLine($"qweqweqweа: {countDivisorsDown 6 1 0}")
    Console.WriteLine($"qweqweqweа: {countDivUp 6 1}")
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