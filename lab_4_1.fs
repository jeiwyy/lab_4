//Дерево содержит строки. Заменить в каждой первый символ на заданный.
open System

type tree =
    | Node of string * tree * tree
    | NULL

let rec insert inpTree inpValue =
    match inpTree with
    | NULL -> Node(inpValue, NULL, NULL)
    | Node(value, left, right) -> 
        if inpValue < value then 
            Node(value, insert left inpValue, right) 
        elif inpValue > value then 
            Node(value, left, insert right inpValue) 
        else 
            inpTree

let rndm = Random()
let rec makeTree countEl Tree =
    match countEl with
    | 0 -> Tree
    | _ ->
        let chars = "abcdefghijklmnopqrstuvwxyz".AsSpan()
        let newVal = rndm.GetString(chars, 4)
        let changedTree = insert Tree newVal
        makeTree (countEl - 1) changedTree

let rec printTree indent isLast node =
        match node with
        | NULL -> ()
        | Node(data, left, right) ->
            let marker = if isLast then "└── " else "├── "
            printfn "%s%s%A" indent marker data
            let newIndent = indent + (if isLast then "    " else "│   ")
            match left, right with
            | NULL, NULL -> ()
            | l, NULL -> printTree newIndent true l
            | NULL, r -> printTree newIndent true r
            | l, r -> 
                printTree newIndent false r
                printTree newIndent true l

let rec mapTree f Tree = 
    match Tree with
    | NULL -> NULL
    | Node (data, left, right) ->
        let newData = f data
        let newLeft = mapTree f left
        let newRight = mapTree f right
        Node (newData, newLeft, newRight)

let changeChar (firChar :string) (inpValue :string) = 
    if inpValue.Length > 0 then
        firChar + inpValue.[1..]
    else
        firChar

let rec inpChar () = 
    printf "Введите символ на который заменить первый: "
    let ourChar = Console.ReadLine()
    if ourChar.Length = 1 then
        ourChar
    else
        printfn "Введено неверное значение, повторите попытку"
        inpChar ()

[<EntryPoint>]
let main args =
    let ourTree = makeTree (10) NULL
    printfn "Дерево: "
    printTree "" true ourTree
    let ourChar = inpChar ()
    let curChangeChar = changeChar ourChar
    let newTree = mapTree curChangeChar ourTree
    printfn "Дерево после изменения: "
    printTree "" true newTree
    0