module MyList exposing (MyList(..), add, append, filter, isEmpty, length, map, member, reverse)


type MyList a
    = Cons a (MyList a)
    | Nil


add : a -> MyList a -> MyList a
add n list =
    Cons n list


append : MyList a -> MyList a -> MyList a
append list1 list2 =
    let
        appendR x y =
            case x of
                Nil ->
                    Nil

                Cons head Nil ->
                    add head y

                Cons head tail ->
                    appendR tail (add head y)
    in
    appendR (reverse list1) list2


reverse : MyList a -> MyList a
reverse list =
    let
        reverseAcc l acc =
            case l of
                Nil ->
                    acc

                Cons head Nil ->
                    add head acc

                Cons head tail ->
                    reverseAcc tail (add head acc)
    in
    reverseAcc list Nil


map : (a -> b) -> MyList a -> MyList b
map f list =
    case list of
        Nil ->
            Nil

        Cons head tail ->
            let
                newHead =
                    f head

                newTail =
                    map f tail
            in
            Cons newHead newTail


isEmpty : MyList a -> Bool
isEmpty list =
    case list of
        Nil ->
            True

        _ ->
            False


length : MyList a -> Int
length list =
    let
        lengthAcc l acc =
            case l of
                Nil ->
                    acc

                Cons head Nil ->
                    acc + 1

                Cons head tail ->
                    lengthAcc tail acc + 1
    in
    lengthAcc list 0


filter : (a -> Bool) -> MyList a -> MyList a
filter f list =
    case list of
        Nil ->
            Nil

        Cons head tail ->
            if f head then
                Cons head (filter f tail)

            else
                filter f tail


member : a -> MyList a -> Bool
member item list =
    let
        lth =
            filter (\x -> x == item) list |> length
    in
    lth > 0
