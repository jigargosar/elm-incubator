module AStarSearch exposing (aStar)

import Basics.Extra exposing (maxSafeInteger)
import Dict exposing (Dict)
import List.Extra as List
import Set exposing (Set)


type alias AStarConfig comparable =
    { neighbours : comparable -> List ( comparable, Float )
    , cost : comparable -> Float
    , start : comparable
    , goal : comparable
    }


type alias AStarAcc comparable =
    { openSet : Set comparable
    , gScores : Dict comparable Float
    , fScores : Dict comparable Float
    , cameFrom : Dict comparable comparable
    }


{-| Finds Path from start to goal using a hueristic function

    A* (pronounced "A-star") is a graph traversal and path search algorithm,
    which is often used in computer science due to its completeness, optimality, and optimal efficiency
        -- [Wikipedia] : <https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode>

    @parm neighbours : A function that maps a node to its outgoing nodes and corresponding weight.
    @parm cost : A heuristic function that never over estimates the cost from a given node to goal
    @parm start: Any comparable value can be used as start and goal
    @parm goal: See above line.

    @returns: List of nodes from start (exclusive) to goal (inclusive).
        i.e. the shortest weighted path.
        Empty list signifies absence of any valid path

-}
aStar :
    (comparable -> List ( comparable, Float ))
    -> (comparable -> Float)
    -> comparable
    -> comparable
    -> List comparable
aStar neighbours cost start goal =
    aStarHelp { neighbours = neighbours, cost = cost, start = start, goal = goal }
        { openSet = Set.singleton start
        , gScores = Dict.singleton start 0
        , fScores = Dict.singleton start (cost start)
        , cameFrom = Dict.empty
        }


getOrMaxSafeInteger k =
    Dict.get k >> Maybe.withDefault maxSafeInteger


aStarHelp : AStarConfig comparable -> AStarAcc comparable -> List comparable
aStarHelp config acc =
    let
        findNextNode openSet =
            openSet
                |> Set.toList
                |> List.minimumBy (\n -> getOrMaxSafeInteger n acc.fScores)

        stepNeighbours current =
            { acc | openSet = Set.remove current acc.openSet }
                |> updateNeighbours config current
    in
    case findNextNode acc.openSet of
        Nothing ->
            []

        Just current ->
            if current == config.goal then
                buildPath acc.cameFrom current []

            else
                aStarHelp config (stepNeighbours current)


buildPath : Dict comparable comparable -> comparable -> List comparable -> List comparable
buildPath cameFrom x xs =
    case Dict.get x cameFrom of
        Just pv ->
            buildPath cameFrom pv (x :: xs)

        Nothing ->
            x :: xs


updateNeighbours :
    AStarConfig comparable
    -> comparable
    -> AStarAcc comparable
    -> AStarAcc comparable
updateNeighbours config current acc =
    let
        currentGScore =
            getOrMaxSafeInteger current acc.gScores
    in
    let
        neighbours =
            config.neighbours current
    in
    List.foldl (updateNeighbourReducer config current currentGScore) acc neighbours


updateNeighbourReducer :
    AStarConfig comparable
    -> comparable
    -> Float
    -> ( comparable, Float )
    -> AStarAcc comparable
    -> AStarAcc comparable
updateNeighbourReducer config current currentGScore ( neighbour, weight ) acc =
    let
        tentativeGScore =
            currentGScore + weight
    in
    if tentativeGScore < getOrMaxSafeInteger neighbour acc.gScores then
        { acc
            | openSet = Set.insert neighbour acc.openSet
            , gScores = Dict.insert neighbour tentativeGScore acc.gScores
            , fScores = Dict.insert neighbour (tentativeGScore + config.cost neighbour) acc.fScores
            , cameFrom = Dict.insert neighbour current acc.cameFrom
        }

    else
        acc
