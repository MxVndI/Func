let rec isPrime n = 
    let rec isPrimeLoop i =
        if i * i > n then true
        else if n % i = 0 then false
        else isPrimeLoop (i+1)
    in 
    if n <= 1 then false
    else if n <= 3 then true
    else isPrimeLoop 2

let p x = 3 * x * x + 3 * x + 1

let solution n =
    let rec loop x count =
        let temp = p x
        if temp > n then count
        else loop (x + 1) (if isPrime temp then count + 1 else count)
    loop 1 0

[<EntryPoint>]
let main argv = 
    let result = solution 1_000_000
    printfn "Количество простых чисел: %d" result
    0
