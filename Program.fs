let factDigit num =
    let rec factDigitLoop mult num =
        if num = 0 then mult
        else factDigitLoop (mult * num) (num - 1)
    factDigitLoop 1 num


let SumOrFact flag =
    match flag with
    | true -> sumDigitsDown
    | false -> factDigit

[<EntryPoint>]
let main argv = 
    Console.Write("Введите число: ")
let num = Console.ReadLine() |> int
Console.WriteLine($"Ответ: {((SumOrFact true) num)}")