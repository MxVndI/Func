let rec readList n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

let readData = 
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    readList n

let rec writeList = function
    [] ->   let z = System.Console.ReadKey()
            0
    | (head : int)::tail -> 
                       System.Console.WriteLine(head)
                       writeList tail  

let findLastMaxIndex list =
    let rec loop currentMax maxIndex index = function
        | [] -> maxIndex
        | x :: xs ->
            if x >= currentMax
            then loop x index (index + 1) xs
            else loop currentMax maxIndex (index + 1) xs

    match list with
    | [] -> -1
    | head :: tail -> loop head 0 1 tail

[<EntryPoint>]
let main argv = 
//    writeList readData
   let listik = readData
   System.Console.WriteLine (findLastMaxIndex listik)
   0
