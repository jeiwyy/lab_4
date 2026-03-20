//Сформировать список из положительных листьев дерева (узел является листом, если у него нет ни левого, ни правого поддерева).
open System

type tree =
    | Node of int * tree * tree
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
        let newVal = rndm.Next(-10, 20)
        let changedTree = insert Tree newVal
        makeTree (countEl - 1) changedTree

let rec printTree indent isLast node =
        match node with
        | NULL -> ()
        | Node(data, left, right) ->
            let marker = if isLast then "└── " else "├── "
            printfn "%s%s%A" indent marker data
            let secMarker = if isLast then "    " else "│   "
            let newIndent = indent + secMarker
            match left, right with
            | NULL, NULL -> ()
            | l, NULL -> printTree newIndent true l
            | NULL, r -> printTree newIndent true r
            | l, r -> 
                printTree newIndent false r
                printTree newIndent true l

let rec foldTree f acc Tree = 
    match Tree with
    | NULL -> acc
    | Node (data, left, right) ->
        let accLeft = foldTree f acc left
        let accNode = f accLeft (Node(data, left, right))
        foldTree f accNode right


let formListPositive acc node =
    match node with
    | Node(data, NULL, NULL) ->
        if data > 0 then
            acc @ [data]
        else
            acc
    | _ ->
        acc

[<EntryPoint>]
let main args =
    let ourTree = makeTree (rndm.Next(10, 20)) NULL
    printfn "Дерево: "
    printTree "" true ourTree
    let newTree = foldTree formListPositive [] ourTree
    printfn "Список из положительных листов дерева: %A" newTree
    0