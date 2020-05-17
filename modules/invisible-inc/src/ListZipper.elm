module ListZipper exposing (ListZipper, current, first, fromList, reverse, right, singleton, swap)


type alias ListZipper a =
    ( List a, a, List a )


singleton : a -> ListZipper a
singleton x =
    ( [], x, [] )


fromList : List a -> Maybe (ListZipper a)
fromList xs =
    case xs of
        [] ->
            Nothing

        c :: r ->
            Just ( [], c, r )


right : ListZipper a -> Maybe (ListZipper a)
right ( l, c, r ) =
    case r of
        [] ->
            Nothing

        nc :: nr ->
            Just ( c :: l, nc, nr )


reverse : ListZipper a -> ListZipper a
reverse ( l, c, r ) =
    ( List.reverse r, c, List.reverse l )


swap : ListZipper a -> ListZipper a
swap ( l, c, r ) =
    ( r, c, l )


current : ListZipper a -> a
current ( _, c, _ ) =
    c


first : ListZipper a -> ListZipper a
first =
    whileJust right


whileJust f x =
    case f x of
        Just nx ->
            whileJust f nx

        Nothing ->
            x
