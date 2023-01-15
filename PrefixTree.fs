module ArrayTrie

open Microsoft.FSharp.Collections


type Trie<'T when 'T: comparison> =
    { IsTerminal: bool
      Children: Map<'T, Trie<'T>> }

module Trie =
    let empty =
        { IsTerminal = false
          Children = Map.empty }

    let rec insert branch trie =
        match branch with
        | [] -> { trie with IsTerminal = true }
        | el :: els ->
            let child =
                match Map.tryFind el trie.Children with
                | Some t -> t
                | None -> empty

            let child' = insert els child

            let children =
                if Map.containsKey el trie.Children then
                    Map.remove el trie.Children
                else
                    trie.Children

            let children' = Map.add el child' children
            { trie with Children = children' }

    let rec contains branch trie =
        match branch with
        | [] ->
            trie.IsTerminal
        | el :: els ->
            match Map.tryFind el trie.Children with
            | Some t -> contains els t
            | None -> false
    let rec toSeq trie =
        seq {
            for KeyValue(label, trie) in trie.Children do
                if trie.IsTerminal then
                    yield [ label ]

                let children = toSeq trie

                for child in children do
                    yield label :: child
        }

    let getSizeAll trie =
        let trieArr = toSeq trie |> Seq.toArray

        let rec size num count =
            if (num < 0) then
                count
            else
                size (num - 1) (count + trieArr[num].Length)

        size (trieArr.Length - 1) 0

    let getBigSize trie =
        let size = toSeq trie |> Seq.toArray |> Array.length
        size

    let rec private removeOneFromList v l =
        let rec remove acc =
            function
            | x :: xs when x = v -> List.rev acc @ xs
            | x :: xs -> remove (x :: acc) xs
            | [] -> acc

        remove [] l

    let rec addAll (newArr: _[]) num newTrie =
            if (num < 0) then
                newTrie
            else
                let myTrie = newTrie |> insert newArr[num]
                addAll newArr (num - 1) myTrie
                
              
    let removeFromTrie value (trie: Trie<'T>): Trie<'T>  =
            let rec castToSeq trie =
                seq {
                    for KeyValue(label, trie) in trie.Children do
                        if trie.IsTerminal then
                            yield [ label ]

                        let children = castToSeq trie

                        for child in children do
                            yield label :: child
                }
            let values = castToSeq trie |> Seq.toList |> removeOneFromList value
            let newTrie = addAll (List.toArray values) (values.Length-1) empty
            newTrie
            

    let addNewTrie firstTrie twoTrie =
        let trieSeq = toSeq twoTrie |> Seq.toArray
        let summaryTrie = addAll trieSeq (trieSeq.Length - 1) firstTrie
        summaryTrie

    let private insideMap (f: 'K -> 'V) arr = arr |> List.map (f)

    let toMap (f: 'K -> 'V) trie =
        let newArr = toSeq trie |> Seq.toArray
        let updateArr = newArr |> Array.map (fun z -> insideMap f z)
        let newTrie = addAll updateArr (newArr.Length - 1) empty
        newTrie

    let filter (f: list<'K> -> bool) trie =
        let newArr = toSeq trie |> Seq.filter (f) |> Seq.toArray
        let newTrie = addAll newArr (newArr.Length - 1) empty
        newTrie

    let private insideLeftFold (f: 'T -> 'V -> 'T) (init: 'T) lst =
        let fold = List.fold (f) init lst
        fold

    let leftFold (f: 'T -> 'V -> 'T) (init: 'T) (trie: Trie<'V>) =
        let trieList = toSeq trie |> Seq.toList
        let folder = (init, trieList) ||> List.fold (fun acc x -> insideLeftFold f acc x)
        folder

    let private insideRightFold (f: 'V -> 'T -> 'T) (init: 'T) (lst: 'V list) =
        let fold = List.foldBack (f) lst init
        fold

    let rightFold (f: 'V -> 'T -> 'T) (init: 'T) (trie: Trie<'V>): 'T =
        let trieList = toSeq trie |> Seq.toArray
        
        let rec insideFolder num init (lst: _[]): 'T =
            if (num < 0) then init
            else
                let newInit = f lst[num] init
                insideFolder (num-1) newInit lst

        let rec folder num init =
            if (num < 0) then init
            else
                let chooseList = trieList[num] |> List.toArray
                let newInit = insideFolder (chooseList.Length-1) init chooseList
                folder (num-1) newInit

        folder (trieList.Length-1) init
    
    let trieEquals (trieOne: Trie<'V>) (trieTwo: Trie<'V>) : bool =
        let seqOne = trieOne |> toSeq |> Seq.toArray
        let seqTwo = trieTwo |> toSeq |> Seq.toArray
        if (seqOne <> seqTwo) then false
        else true
