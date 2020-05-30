module AStarSearch exposing (aStar)

import Dict exposing (Dict)
import List.Extra as List


type alias AStarConfig comparable =
    { neighbours : comparable -> List ( comparable, Float )
    , cost : comparable -> Float
    , start : comparable
    , goal : comparable
    }


type alias AStarNode comparable =
    { gScore : Float
    , fScore : Float
    , value : comparable
    , closed : Bool
    }


type alias AStarAcc comparable =
    { openSet : Dict comparable (AStarNode comparable)
    , neighboursCache : Dict comparable (List ( comparable, Float ))
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
        { openSet = Dict.singleton start (AStarNode 0 (cost start) start False)
        , neighboursCache = Dict.empty
        , cameFrom = Dict.empty
        }


aStarHelp : AStarConfig comparable -> AStarAcc comparable -> List comparable
aStarHelp config acc =
    case
        acc.openSet
            |> Dict.values
            |> List.filterNot .closed
            |> List.minimumBy .fScore
    of
        Nothing ->
            []

        Just currentNode ->
            if currentNode.value == config.goal then
                buildPath acc.cameFrom currentNode.value []

            else
                aStarHelp config
                    ({ acc
                        | openSet =
                            Dict.update currentNode.value
                                (Maybe.map (\n -> { n | closed = True }))
                                acc.openSet
                     }
                        |> updateNeighbours config currentNode
                    )


buildPath : Dict comparable comparable -> comparable -> List comparable -> List comparable
buildPath cameFrom x xs =
    case Dict.get x cameFrom of
        Just pv ->
            buildPath cameFrom pv (x :: xs)

        Nothing ->
            x :: xs


updateNeighbours :
    AStarConfig comparable
    -> AStarNode comparable
    -> AStarAcc comparable
    -> AStarAcc comparable
updateNeighbours config currentNode acc0 =
    let
        reducer : ( comparable, Float ) -> AStarAcc comparable -> AStarAcc comparable
        reducer ( neighbour, weight ) acc =
            updateNeighbourReducer config currentNode neighbour weight acc
                |> Maybe.withDefault acc
    in
    currentNode.value
        |> config.neighbours
        |> List.foldl reducer acc0


updateNeighbourReducer :
    AStarConfig comparable
    -> AStarNode comparable
    -> comparable
    -> Float
    -> AStarAcc comparable
    -> Maybe (AStarAcc comparable)
updateNeighbourReducer config currentNode neighbourValue weight acc =
    let
        tentativeGScore =
            currentNode.gScore + weight

        shouldUpdate =
            case Dict.get neighbourValue acc.openSet of
                Nothing ->
                    True

                Just node ->
                    if tentativeGScore < node.gScore then
                        True

                    else
                        False
    in
    if shouldUpdate then
        Just
            { acc
                | openSet =
                    Dict.insert neighbourValue
                        { gScore = tentativeGScore
                        , fScore = tentativeGScore + config.cost neighbourValue
                        , value = neighbourValue
                        , closed = False
                        }
                        acc.openSet
                , cameFrom = Dict.insert neighbourValue currentNode.value acc.cameFrom
            }

    else
        Nothing
