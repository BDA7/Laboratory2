module Program

open ArrayTrie


[<EntryPoint>]
let main argv =
    printfn "use 1"
    let stringTrie =
        Trie.empty
        |> Trie.insert ([ 'h'; 'e'; 'l'; 'l'; 'o' ])
        |> Trie.insert ([ 'h'; 'e'; 'r'; 'l'; 'd' ])
        |> Trie.insert ([ '!' ])
        |> Trie.insert ([ 'D'; 'a'; 'n'; 'i'; 'l'; 'a' ])
        |> Trie.insert ([])
    
    printfn "%A" stringTrie
    
    printfn "use 2"
    let filterTrie = stringTrie |> Trie.filter (fun x -> x.Length = 6)
    printfn "%A" filterTrie
    
    printfn "use 3"
    let mapTrie = stringTrie |> Trie.toMap (fun x -> int (x))
    printfn "%A" mapTrie
    
    printfn "use 4"
    let foldTrie = stringTrie |> Trie.leftFold (+) ' '
    printfn "%A" foldTrie
    
    printfn "use 5"
    let newTrie = Trie.empty |> Trie.insert ([ 'h'; 'i' ])
    let updateStringTrie = Trie.addNewTrie stringTrie newTrie
    printfn "%A" updateStringTrie
    
    0
