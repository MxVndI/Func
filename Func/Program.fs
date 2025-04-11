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

let countElemsAfterLastMax list =
    let rec loop currentMax lastMaxIndex currentIndex = function
        | [] -> List.length list - lastMaxIndex - 1
        | h :: t ->
            if h >= currentMax
            then loop h currentIndex (currentIndex + 1) t
            else loop currentMax lastMaxIndex (currentIndex + 1) t
    match list with
    | [] -> 0
    | head :: tail -> loop head 0 1 tail

let elemsAfterFirstMax list =
    let rec loop currentMax acc = function
        | [] -> acc
        | h :: t ->
            if h > currentMax then 
                loop h [] t
            else 
                loop currentMax (acc @ [h]) t

    match list with
    | [] -> []
    | head :: tail -> 
        loop head [] tail 

let countAfterLastMax list newList=
    if List.isEmpty list then 0
    else
        let maxVal = List.max list
        let lastIndex = List.findIndexBack (fun x -> x = maxVal) list
        List.length list - lastIndex - 1

let elemsAfterFirstMaxList list =
    if List.isEmpty list then []
    else
        let maxValue = List.max list
        list
        |> List.tryFindIndex (fun x -> x = maxValue)
        |> Option.map (fun idx -> List.skip (idx + 1) list)
        |> Option.defaultValue []

let findUniqueRec list =
    let rec findDifferent main = function
        | [] -> failwith "Нет уникального элемента"
        | x :: xs -> if x <> main then x else findDifferent main xs

    match list with
    | [] -> failwith "Пустой список"
    | [x] -> x
    | x1 :: x2 :: rest ->
        if x1 = x2 then
            findDifferent x1 list
        else
            match rest with
            | x3 :: _ ->
                if x3 = x1 then x2
                elif x3 = x2 then x1
            | [] -> failwith "Слишком короткий список"

let findUniqueElement list =
    list
    |> List.groupBy id
    |> List.pick (fun (key, group) -> 
        if List.length group = 1 then Some key else None)

let rec countEvenRec list =
    match list with
    | [] -> 0
    | head :: tail ->
        let increment = if head % 2 = 0 then 1 else 0
        increment + countEvenRec tail

let countEvenList list =
    list
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

let averageOfAbsRec list =
    let rec loop sum = function
        | [] -> sum
        | h :: t -> loop (sum + abs h) t
    
    if List.isEmpty list then 0.0
    else
        let total = loop 0 list
        float total / float (List.length list)

let averageOfAbsList list =
    if List.isEmpty list then 0.0
    else
        list
        |> List.map abs
        |> List.averageBy float

let buildListsPureRec list =
    let rec findIndex x lst idx =
        match lst with
        | [] -> None
        | h :: t -> if h = x then Some idx else findIndex x t (idx + 1)

    let rec updateCounter index value lst =
        match lst with
        | [] -> []
        | h :: t -> 
            if index = 0 then (h + value) :: t
            else h :: updateCounter (index - 1) value t

    let rec processElements accL1 accL2 = function
        | [] -> (List.rev accL1, List.rev accL2)
        | h :: t ->
            match findIndex h accL1 0 with
            | Some idx ->
                let newL2 = updateCounter idx 1 accL2
                processElements accL1 newL2 t
            | None ->
                processElements (h :: accL1) (1 :: accL2) t

    processElements [] [] list

let buildListsList list =
    list
    |> List.groupBy id
    |> List.map (fun (k, v) -> (k, List.length v))
    |> List.unzip

let countRussianRec (str: string) =
    let rec loop count index =
        if index >= str.Length then count
        else
            let c = str.[index]
            let isRussian = 
                (c >= 'А' && c <= 'Я') || 
                (c >= 'а' && c <= 'я')
            loop (count + if isRussian then 1 else 0) (index + 1)
    loop 0 0

let countRussianList (str: string) =
    str 
    |> List.ofSeq
    |> List.filter (fun c -> 
        (c >= 'А' && c <= 'Я') || 
        (c >= 'а' && c <= 'я'))
    |> List.length

[<EntryPoint>]
let main argv = 
//    writeList readData
   let listik = readData
   //System.Console.WriteLine (countElemsAfterLastMax listik)
   System.Console.WriteLine (elemsAfterFirstMax listik)
   System.Console.WriteLine (elemsAfterFirstMaxList listik)

   0
