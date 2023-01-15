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
    
    let btreeInts =
        Trie.empty
        |> Trie.insert ([ 1; 4; 7 ])
        |> Trie.insert ([ 1; 5; 6 ])
        |> Trie.insert ([ 2; 5 ])
        |> Trie.insert ([ 5; 150; 625; 1255 ])
        
    printfn "%A" stringTrie
    let twoStringTrie = stringTrie |> Trie.removeFromTrie 'D'
    printfn "equals"
    let nTrie = stringTrie |> Trie.trieEquals twoStringTrie
    printfn "%A" nTrie
    
    printfn "use 2"
    let filterTrie = stringTrie |> Trie.filter (fun x -> x.Length = 6)
    printfn "%A" filterTrie
    
    printfn "use 3"
    let mapTrie = stringTrie |> Trie.toMap (fun x -> int (x))
    printfn "%A" mapTrie
    
    printfn "use 4"
    let foldTrie = (0, btreeInts) ||> Trie.rightFold (fun x acc -> acc - x)
    let foldTrie2 = btreeInts |> Trie.leftFold (-) 0
    printfn $"{foldTrie} {foldTrie2}"
    
    printfn "use 5"
    let newTrie = Trie.empty |> Trie.insert ([ 'h'; 'i' ])
    let updateStringTrie = Trie.addNewTrie stringTrie newTrie
    printfn "%A" updateStringTrie
    
    0
