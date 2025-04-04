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

let fact num =
    let rec factCounter mult num =
        if num = 0 then mult
        else factCounter (mult * num) (num - 1)
    factCounter 1 num

let sumOrFact flag =
    match flag with
    | true -> sumDigitsDown
    | _ -> fact
    
let rec cifrFold num (func: int -> int -> int) acc (condition: int -> bool) =
    match num with
    | 0 -> acc
    | tmp ->
        let digit = abs (tmp % 10)
        let flag = condition digit
        match flag with
        | true -> cifrFold (tmp / 10) func (func acc (tmp % 10)) condition
        | false -> cifrFold (tmp / 10) func acc condition

let quiz lang =
    match lang with
    | "F#" -> "Подлиза"
    | "Prolog" -> "Я тебе не верю"
    | "Ruby" -> "Арсений Сергеевич, это вы?"
    | someth -> "Фух"

let chooseLanguageSuperPos () = 
    Console.Write("Введите любимый язык: ")
    (Console.ReadLine >> quiz >> Console.WriteLine)

let chooseLanguageCurry () = 
    Console.Write("Введите любимый язык: ")
    let subChooseLanguageCurry reader func writer =
        let answer = reader()
        let langResult = func answer
        writer langResult
    subChooseLanguageCurry Console.ReadLine quiz Console.WriteLine

let rec gcd x y = 
    match y with
    | 0 -> x
    | someth -> gcd y (x%y)

let obhodProst num (func: int->int->int) init  =
    let rec obhodProstLoop num func acc current =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                match result with
                | 1 -> func acc current
                | _ -> acc
            obhodProstLoop num func newAcc (current - 1)
    obhodProstLoop num func init num

let obhodProstCondition num (func: int->int->int) init (condition: int -> bool)  =
    let rec obhodProstLoop num func acc current condition =
        match current with
        | 0 -> acc
        | someth ->
            let newAcc =
                let result = gcd num current
                let flag = condition current
                match result, flag with
                | 1, true -> func acc current
                | _, _ -> acc
            obhodProstLoop num func newAcc (current - 1) condition
    obhodProstLoop num func init num condition

let isPrime num =
    if num <= 1 then false
    else
        let rec isPrimeLoop curr =
            match curr with
            | _ when curr * curr > num -> true
            | _ when num % curr = 0 -> false
            | _ -> isPrimeLoop (curr + 1)
        isPrimeLoop 2
        

let maxProstDel num =
    let rec maxProstDelLoop max del =
        match del with
        | _ when del > num -> max
        | _ when (num % del = 0) && (isPrime del) -> maxProstDelLoop del (del+1)
        | _ -> maxProstDelLoop max (del+1)
    maxProstDelLoop 0 2

let EulerFinder num = 
    obhodProst num (fun x y -> x + 1) 0

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

    Console.WriteLine($"qweqweqweа: {countDivisorsDown 6 1 0}")
    Console.WriteLine($"qweqweqweа: {countDivUp 6 1}")

    let meow c = 
        Console.Write("Введите число: ")
    let num = Console.ReadLine() |> int
    Console.WriteLine($"Ответ: {((sumOrFact true) num)}")
    Console.WriteLine($"Сумма: {cifrFold num (fun x y -> x + y) 0  }")
    Console.WriteLine($"Произведение: {cifrFold num (fun x y -> x * y) 1  }")
    Console.WriteLine($"Максимальный: {cifrFold num (fun x y -> if x > y then x else y) 0 }")
    Console.WriteLine($"Минимальный: {cifrFold num (fun x y -> if x < y then x else y) 10 }")

    Console.WriteLine($"Сумма цифр, которые больше 7: {cifrFold num (fun x y -> (x + y)) 0 (fun z -> z > 7)}")
    Console.WriteLine($"Произведение цифр, которые меньше 3: {cifrFold num (fun x y -> (x * y)) 1 (fun z -> z < 3)}")
    Console.WriteLine($"Максимальное нечётное число: {cifrFold num (fun x y -> if x > y then x else y) 0 (fun z -> z % 2 = 1)}")
    chooseLanguageSuperPos()
    chooseLanguageCurry()

    Console.Write("Введите число: ")
    let num = Console.ReadLine() |> int
    Console.WriteLine($"Сумма: {obhodProst num (fun x y -> x + y) 0  }")
    Console.WriteLine($"Произведение: {obhodProst num (fun x y -> x * y) 1  }")
    Console.WriteLine($"Максимальный: {obhodProst num (fun x y -> if x > y then x else y) 0 }")
    Console.WriteLine($"Минимальный: {obhodProst num (fun x y -> if x < y then x else y) 10 }")
    Console.WriteLine($"Эйлер: {EulerFinder num}")

    Console.WriteLine($"Сумма: {obhodProstCondition num (fun x y -> x + y) 0 (fun x -> if x > 8 then false else true)}")
    Console.WriteLine($"Произведение: {obhodProstCondition num (fun x y -> x * y) 1  (fun x -> if x > 8 then false else true)}")
    Console.WriteLine($"Максимальный: {obhodProstCondition num (fun x y -> if x > y then x else y) 0 (fun x -> if x > 8 then false else true)}")
    Console.WriteLine($"Минимальный: {obhodProstCondition num (fun x y -> if x < y then x else y) 10 (fun x -> if x > 8 then false else true)}")
    Console.WriteLine($"Эйлер: {EulerFinder num }")

    let num = Console.ReadLine() |> int
    Console.WriteLine($"Максимальный простой делитель числа: {maxProstDel num}")
    0
    