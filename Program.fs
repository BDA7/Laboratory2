module Program

    open ArrayTrie


    [<EntryPoint>]
    let main argv =
        let stringTrie = Trie.empty |> Trie.insert(['h'; 'e'; 'l'; 'l'; '0']) |> Trie.insert(['h'; 'o'; 'r'; 'l'; 'd']) |> Trie.insert([])
        let adder = Trie.empty |> Trie.insert([4;6]) |> Trie.insert([5;6;4]) |> Trie.insert([1;2])
        let adder2 = Trie.empty |> Trie.insert([4;8]) |> Trie.insert([5;7;4]) |> Trie.insert([2;8])
        let summary = Trie.addNewTrie adder adder2
        let filterTrie = Trie.leftFold (*) 1 adder
        printfn "%A" summary
        0