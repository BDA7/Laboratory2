module Laboratory2

open ArrayTrie
open NUnit.Framework

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

        let deleteElmBtreeInts = newBtreeInts |> Trie.remove ([ 1; 4; 7 ])
        let deleteElmBtreeChars = newBtreeChars |> Trie.remove ([ 'a'; 'f'; 'g' ])

        assert ((Trie.getBigSize newBtreeInts) > (Trie.getBigSize deleteElmBtreeInts))
        assert ((Trie.getSizeAll newBtreeInts) > (Trie.getSizeAll deleteElmBtreeInts))

        assert ((Trie.getBigSize newBtreeChars) > (Trie.getBigSize deleteElmBtreeChars))
        assert ((Trie.getSizeAll newBtreeChars) > (Trie.getSizeAll deleteElmBtreeChars))

    [<Test>]
    member this.``Test Filter``() =
        let filterBtreeInts =
            btreeInts |> Trie.filter (fun x -> x.Length = 3) |> Trie.getBigSize

        let filterBtreeChars =
            btreeChars |> Trie.filter (fun x -> x.Length < 3) |> Trie.getBigSize

        assert (filterBtreeInts = 2)
        assert (filterBtreeChars = 1)

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
        let leftFoldBtreeIntsSub = btreeInts |> Trie.leftFold (*) 1
        let rightFoldBtreeIntsSub = btreeInts |> Trie.rightFold (*) 1
        assert (leftFoldBtreeIntsSum = rightFoldBtreeIntsSum)
        assert (leftFoldBtreeIntsSub = rightFoldBtreeIntsSub)

    [<Test>]
    member this.``Test Neutral Element``() =
        let newBtreeInts =
            Trie.empty
            |> Trie.insert ([ 5; 6; 9 ])
            |> Trie.insert ([ 4; 7; 8 ])
            |> Trie.insert ([ 1; 4; 7 ])

        let newBtree = Trie.addNewTrie btreeInts newBtreeInts |> Trie.getBigSize
        assert (newBtree > Trie.getBigSize newBtreeInts)
        assert (newBtree > Trie.getBigSize btreeInts)
        assert (newBtree < ((Trie.getBigSize newBtreeInts) + (Trie.getBigSize btreeInts)))
        assert (newBtree = ((Trie.getBigSize newBtreeInts) + (Trie.getBigSize btreeInts) - 1))
