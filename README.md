# Функциональное программирование
## Лабораторная работа 2
### Вариант Prefix Btree
**Выполнил:** Бондаренко Данила Александрович \
**Группа:** P34112 \
**Преподаватель:** Пенской Александр Владимирович

### Требования к стуктуре
>Функции:
>- добавление и удаление элементов;
>- фильтрация;
>- отображение (map);
>- свертки (левая и правая);
>- структура должна быть моноидом.

> Требования к библиотеке
>- Структуры данных должны быть неизменяемыми.
>- Библиотека должна быть протестирована в рамках unit testing.
>- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
>- Структура должна быть полиморфной.
>- Требуется использовать идиоматичный для технологии стиль программирования.

### Реализация
Сама структура является массивом узлов и терминальным значением
```f#
type Trie<'T when 'T: comparison> =
    { IsTerminal: bool
      Children: Map<'T, Trie<'T>> }
```
При добавлении ветви идет поиск подходящих вершин, если они не находятся, ветвь начинает строиться сначала доходя до последнего(терминального) элемента.
```f#
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
```
Удаление осуществляется путем преобразования в последовательность и удаления элемента из нее и обратного преобразования в дерево
```f#
    let remove el trie =
        let newArr = toSeq trie |> Seq.toList |> removeOneFromList el |> List.toArray
        let newTrie = addAll newArr (newArr.Length - 1) empty
        newTrie
```
Функция поиска индекса возвращает false, если элемент не найден
```f#
    let rec contains branch trie =
        match branch with
        | [] -> trie.IsTerminal
        | el :: els ->
            match Map.tryFind el trie.Children with
            | Some t -> contains els t
            | None -> false
```
Функция фильтрации проходится по всем элементам и оставляет только удовлетворяющие условию
```f#
    let filter (f: list<'K> -> bool) trie =
        let newArr = toSeq trie |> Seq.filter (f) |> Seq.toArray
        let newTrie = addAll newArr (newArr.Length - 1) empty
        newTrie
```
Функция toMap проходится по всем элементам и применяет к ним задануюю функцию пораждая новое дерево

```f#
    let toMap (f: 'K -> 'V) trie =
        let newArr = toSeq trie |> Seq.toArray
        let updateArr = newArr |> Array.map (fun z -> insideMap f z)
        let newTrie = addAll updateArr (newArr.Length - 1) empty
        newTrie
```
Функция leftFold проходится по всем элементам, то высчитывается acc с помощью передаваемой функции и переходится к следующему элементу.
```f#
    let private insideLeftFold (f: 'T -> 'V -> 'T) (init: 'T) lst =
        let fold = List.fold (f) init lst
        fold

    let leftFold (f: 'T -> 'V -> 'T) (init: 'T) (trie: Trie<'V>) =
        let trieList = toSeq trie |> Seq.toList
        let folder = (init, trieList) ||> List.fold (fun acc x -> insideLeftFold f acc x)
        folder
```
Функция rightFold делает тоже самое, но начиная с конца
```f#
    let private insideRightFold (f: 'V -> 'T -> 'T) (init: 'T) lst =
        let fold = List.foldBack (f) lst init
        fold

    let rightFold (f: 'V -> 'T -> 'T) (init: 'T) (trie: Trie<'V>) =
        let trieList = toSeq trie |> Seq.toList
        let folder =
            (trieList, init) ||> List.foldBack (fun x acc -> insideRightFold f acc x)
        folder
```
Функция addNewTrie служит для слияния двух деревьев.
```f#
    let addNewTrie firstTrie twoTrie =
        let trieSeq = toSeq twoTrie |> Seq.toArray
        let summaryTrie = addAll trieSeq (trieSeq.Length - 1) firstTrie
        summaryTrie
```
### Выводы
В ходе выполнения лабораторной работы я столкнулся с множеством проблем. Проблемы были по большей части с типизацией, тк потомки все потомки одной вершины хранятся в листе, а уже полноценные ветви в листе и выходит лист в листе с данными и пришлость очень сильно мучиться чтобы реализовать функции, еще момент с фолдом к char, F# складывает их не образуя строку, а складывая их вес и выдает символ по этому весу.
