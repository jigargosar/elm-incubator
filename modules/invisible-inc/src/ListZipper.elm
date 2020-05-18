module ListZipper exposing
    ( ListZipper
    , current
    , fromList
    , last
    , right
    , singleton
    , swap
    , toList
    )

import More exposing (..)


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


toList : ListZipper a -> List a
toList ( l, c, r ) =
    List.reverse l ++ c :: r


right : ListZipper a -> Maybe (ListZipper a)
right ( l, c, r ) =
    case r of
        [] ->
            Nothing

        nc :: nr ->
            Just ( c :: l, nc, nr )


swap : ListZipper a -> ListZipper a
swap ( l, c, r ) =
    ( r, c, l )


current : ListZipper a -> a
current ( _, c, _ ) =
    c


last : ListZipper a -> ListZipper a
last =
    whileJust right
