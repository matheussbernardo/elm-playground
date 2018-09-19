module MyTree exposing
    ( MyTree(..)
    , exampleTree
    , fromList
    , insert
    , member
    )


type MyTree a
    = Node a (MyTree a) (MyTree a)
    | Empty


exampleTree : MyTree Int
exampleTree =
    Node 4 (Node 1 Empty Empty) (Node 9 Empty Empty)


member : comparable -> MyTree comparable -> Bool
member target tree =
    case tree of
        Empty ->
            False

        Node value left right ->
            if target < value then
                member target left

            else if target > value then
                member target right

            else
                True


insert : comparable -> MyTree comparable -> MyTree comparable
insert target tree =
    case tree of
        Empty ->
            Node target Empty Empty

        Node value left right ->
            if target < value then
                Node value (insert target left) right

            else if target > value then
                Node value left (insert target right)

            else
                tree


fromList : List comparable -> MyTree comparable
fromList list =
    List.foldl insert Empty list
