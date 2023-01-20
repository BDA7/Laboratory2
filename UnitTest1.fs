module Laboratory2

open ArrayTrie
open NUnit.Framework
open FsCheck.NUnit

[<SetUp>]
let Setup () = ()

let btreeInts =
    Trie.empty
    |> Trie.insert ([ 1; 4; 7 ])
    |> Trie.insert ([ 1; 5; 6 ])
    |> Trie.insert ([ 2; 5 ])
    |> Trie.insert ([ 5; 150; 625; 1255 ])

let btreeChars =
    Trie.empty
    |> Trie.insert ([ 'a'; 'b'; 'c' ])
    |> Trie.insert ([ 'a'; 'f'; 'g' ])
    |> Trie.insert ([ 'b'; 'c'; 'f'; 'g' ])
    |> Trie.insert ([ 'a' ])

[<TestFixture>]
type TestClass() =

    [<Test>]
    member this.``Test create``() =
        let emptyBtree = Trie.empty
        assert ((Trie.getBigSize emptyBtree) = 0)
        assert ((Trie.getBigSize btreeInts) = 4)
        assert ((Trie.getSizeAll btreeInts) = 12)
        assert ((Trie.getBigSize btreeChars) = 4)
        assert ((Trie.getSizeAll btreeChars) = 11)

    [<Test>]
    member this.``Test Insert delete``() =
        let newBtreeInts =
            btreeInts |> Trie.insert ([ 5; 6; 8 ]) |> Trie.insert ([ 1; 150; 4689 ])
        
        let newBtreeChars =
            btreeChars
            |> Trie.insert ([ 'a'; '1'; 'g' ])
            |> Trie.insert ([ 'z'; 'h'; 'u'; 'y'; 'z' ])

        let deleteElmBtreeInts = newBtreeInts |> Trie.removeFromTrie (1)
        let deleteElmBtreeChars = newBtreeChars |> Trie.removeFromTrie ('a')
        
        assert ((Trie.getBigSize newBtreeInts) > (Trie.getBigSize deleteElmBtreeInts))
        assert ((Trie.getSizeAll newBtreeInts) > (Trie.getSizeAll deleteElmBtreeInts))
        
        assert ((Trie.getBigSize newBtreeChars) > (Trie.getBigSize deleteElmBtreeChars))
        assert ((Trie.getSizeAll newBtreeChars) > (Trie.getSizeAll deleteElmBtreeChars))

    [<Test>]
    member this.``Test Filter``() =
        let filterBtreeInts =
            btreeInts |> Trie.filter (fun x _ -> x = 1) |> Trie.getBigSize

        let filterBtreeChars =
            btreeChars |> Trie.filter (fun x _ -> x = 'a') |> Trie.getBigSize
        assert (filterBtreeInts = 2)
        assert (filterBtreeChars = 3)

    [<Test>]
    member this.``Test Map``() =
        let mapBtreeInts = btreeInts |> Trie.toMap (fun x -> $"{x}") |> box
        let mapBtreeChars = btreeChars |> Trie.toMap (fun x -> int (x)) |> box
        assert (mapBtreeInts :? Trie<string>)
        assert (mapBtreeChars :? Trie<int>)

    [<Test>]
    member this.``Test Fold``() =
        let leftFoldBtreeIntsSum = btreeInts |> Trie.leftFold (+) 0
        let rightFoldBtreeIntsSum = btreeInts |> Trie.rightFold (+) 0
        let leftFoldBtreeIntsSub = btreeInts |> Trie.leftFold (-) 0
        let rightFoldBtreeIntsSub = btreeInts |> Trie.rightFold (fun x acc -> acc - x) 0
        let leftFoldBtreeIntsUm = btreeInts |> Trie.leftFold (*) 1
        let rightFoldBtreeIntsUm = btreeInts |> Trie.rightFold (*) 1
        assert (leftFoldBtreeIntsSub = rightFoldBtreeIntsSub)
        assert (leftFoldBtreeIntsSum = rightFoldBtreeIntsSum)
        assert (leftFoldBtreeIntsUm = rightFoldBtreeIntsUm)
    
    [<Test>]
    member this.``Test equals`` () =
        let oneTrie = btreeChars |> Trie.removeFromTrie 'b'
        let twoTrie = btreeChars |> Trie.removeFromTrie 'a'
        let threeTrie = btreeChars |> Trie.removeFromTrie 'b'
        assert(Trie.trieEquals oneTrie oneTrie)
        assert(Trie.trieEquals oneTrie threeTrie)
        assert (not (Trie.trieEquals oneTrie twoTrie))
        assert (not ((Trie.trieEquals btreeChars oneTrie) || (Trie.trieEquals btreeChars twoTrie) || (Trie.trieEquals btreeChars threeTrie)))

    
    [<Property>]
    member this.``Test Neutral Element`` (data: (int list[])) =
        let emptyTrie = Trie.empty
        let newTrie = Trie.empty |> Trie.addAll data (data.Length-1)
        let mergedTrie = Trie.addNewTrie emptyTrie newTrie
        Trie.trieEquals newTrie mergedTrie
    
    
    [<Property>]
    member this.``Test Associativity``(data1: (int) list, data2: (int) list, data3: (int) list) =
        let oneTrie = Trie.empty |> Trie.insert data1
        let twoTrie = Trie.empty |> Trie.insert data2
        let threeTrie = Trie.empty |> Trie.insert data3
        let mergedOneTwo = Trie.addNewTrie oneTrie twoTrie
        let mergedTwoThree = Trie.addNewTrie twoTrie threeTrie
        Trie.trieEquals (Trie.addNewTrie mergedOneTwo threeTrie) (Trie.addNewTrie mergedTwoThree oneTrie)
        