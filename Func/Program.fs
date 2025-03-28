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
    0
    