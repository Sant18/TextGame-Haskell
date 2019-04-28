--imports certain List functions--
import Data.List
--imports certain Char functions--
import Data.Char

--type decleration, TYPE is used to make code easier to understand--
--Helps user not mismatch types in any way--
type Location = String
type Direction = String
type Thing = String
type Response = String

--organize data structure--
{-All state information needed by a function has to be provided in the parameters to that function
Not every function needs all the state information.-}
--This is a list of room connections--
--PathMap is a simple list--
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("zomb", "down"), "tunnel"),
    (("tunnel", "up"), "zomb"),
    (("tunnel", "west"), "tunnel entrance"),
    (("tunnel entrance", "east"), "tunnel"),
    (("tunnel entrance", "south"), "area"),
    (("area", "south"), "building"),
--this means if you go north from the area, you find yourself in the tunnel entrance--
    (("area", "north"), "tunnel entrance"),
    (("building", "west"), "cage"),
    (("building", "north"), "area"),
    (("building", "east"), "closet"),
    (("cage", "east"), "building"),
    (("closet", "west"), "building")
    ]

--This function is a list of where certain things are, including the player--
--LocationMap is a simple list--
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("fuel", "zomb"),
{-The key is located in the tunnel entrance-}("key", "tunnel entrance"),
    ("lantern", "building"),
    ("bow and arrow", "closet"),
    ("myself", "area"),
    ("zomb", "alive")
    ]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

--Main function--
main :: IO (String)
main = do
    putStrLn "\nThis is a Zombie game\n"
    putStrLn instructions
    play_game $ return (paths, locations, "")
    return "Thank you for playing"
    
instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main                                 -- to start the game.\n" ++
    "north  south  east  west  up  down   -- to go in that direction.\n" ++"drop item                            -- to put down the item.\n" ++
    "take item                            -- to pick up the item.\n" ++
    "kill                                 -- to attack a zombie.\n" ++
    "search                               -- to search the area.\n" ++
    "supply                               -- to see your supplies.\n" ++
    "quit game                            -- to end the game and quit."

--A Sequence of actions can be combined as a single composite action using the keyword do--
--Derived primitives where it reads a string from the keyboard--
--This function is the main loop, which requests and process user input until you die--
play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
        else do
            putStr "command> "
            command <- getLine
            if command == "quit game"
                then return (paths, locations, "Quitting.")
                else  play_game $ return (do_command command paths locations)
--This action declares you dead--
game_over :: LocationMap -> Bool
game_over locations =
    let my_location = get "myself" locations
        fuel_location = get "fuel" locations
    in my_location == "dead" || (my_location == "area" && fuel_location == "holding")
    
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "area" "north" _ locations= get "lantern" locations == "holding"
can_move "building" "east" _ locations = get "key" locations == "holding"
can_move from direction paths _ =
    elem (from, direction) keys 
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because "area" "north" = "It's too dark, you need to find a light source, search the area in the south"
cannot_move_because "building" "east" = "You walk into a room with a locked case, you need a key."
cannot_move_because _ _ = "This direction is not an option"

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "north" paths locations = go "north" paths locations
do_command "east" paths locations = go "east" paths locations
do_command "south" paths locations = go "south" paths locations
do_command "west" paths locations = go "west" paths locations
do_command "up" paths locations = go "up" paths locations
do_command "down" paths locations = down_from_zomb "down" paths locations
do_command "search" paths locations = search paths locations
do_command "kill" paths locations = kill paths locations
do_command "supply" paths locations = (paths, locations, supply locations)
do_command "quit" paths locations = (paths, locations, "quit")
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "This is not a valid command: " ++ cmd)

game_take :: Thing -> PathMap -> LocationMap -> World          
game_take thing paths locations =
    let here = get "myself" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "holding" locations), "item picked up.")
       else if there == "holding"
            then (paths, locations, "You are already holding it.")
            else (paths, locations, "I don't see it here.")
        
game_drop :: Thing -> PathMap -> LocationMap -> World          
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "myself" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), "item dropped.")
        else (paths, locations, "You aren't holding it.")

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "myself" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, cannot_move_because my_location direction)

down_from_zomb :: String -> PathMap -> LocationMap -> World
down_from_zomb direction paths locations =
    if get "myself" locations == "zomb" &&
       get "zomb" locations == "alive" &&
       get "fuel" locations == "holding"
           then (paths, put "myself" "dead" locations, description "tunnel3")
           else go direction paths locations 

search :: PathMap -> LocationMap -> World
search paths locations =
    if things == []
        then (paths, locations, describe my_location locations)
        else (paths, locations, describe my_location locations ++ "\n\n" ++ things)
    where my_location = get "myself" locations
          things = items_here locations

kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "myself" locations of
        "cage" -> (paths,
                   put "myself" "dead" locations,
                   "There are too many zombies and they overwhelm you with their strength, you are DEAD!!")
        "tunnel" -> (paths, locations,
                   "Your too far to do anything, you need a better vantage point, use the boxes next to you")
        "zomb" ->
            if get "bow and arrow" locations == "holding"
                then (paths,
                      put "zomb" "dead" locations,
                      "You draw the bow, line up and take the shot, the arrow goes\n" ++
                     "through the zombies head, blood gushes everywhere, the zombie is dead\n" ++
                     "SEARCH the room and take the fuel the car")
                else (paths,
                      locations,
                      "You have no weapon, head to the factory and find one, then come back\n" ++
                      "")
        _ -> (paths, locations, "There is nothing to kill here")
        
supply :: LocationMap -> Response
supply locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    in if my_stuff == []
        then "You aren't holding anything."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "myself" locations
        things = ["There is " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "myself"]
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "myself" locations
        zomb_status = get "zomb" locations
        fuel_location = get "fuel" locations
    in describe_helper here zomb_status fuel_location  locations 

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "area" "dead" "holding" locations = description "area2"
describe_helper "tunnel" "alive" "holding" locations = description "tunnel3"
describe_helper "tunnel" "dead" _ locations = description "tunnel2"
describe_helper "zomb" "dead" _ locations = description "zomb2"
describe_helper here _ _ locations = description here

--Below are all the description--
description :: Location -> String
description "area" =
    "You are in an abandoned town infested by zombies.\n" ++
    "You have a vehicle to escape but there is no fuel.\n" ++
    "You must head north into the tunnel and find it.\n" ++
    "There looks to be a factory full of tools in the south\n" ++
    "which could help you with your journey."

description "area2" = "Congrats!!! you have won the game, the car now has fuel and you have escaped.....for now"

description "building" =
    "You are in the factory, you hear noises, you better search the area quickly.\n" ++
    "There is door to the east and a closet to the west"
 

description "cage" =
    "You opened the door and there are several zombies!!!! you can kill them\n" ++
    "or leave, I would leave if I was you"
    
description "closet" =
    "There is a case, you have unlocked it with the key, search to see whats in it."

description "tunnel entrance" =
    "You are in the dark tunnel, cars and belongings left behind,\n" ++
    "There is an opening to the east."


description "tunnel" =
    "The FUEL is there at the back of the room, but it's being guarded by a zombie\n" ++
    "you cant just run and get it, you need a weapon, if you have one now is the time to use it!!."
   
    
description "tunnel2" =
    "You climb down the boxes"

description "tunnel3" =
     "The zombie sees you, and kills you"
   

description "zomb" =
    "You climb up the boxes to get a better angle,\n" ++
    "the boxes are not stable, go for the KILL!!"

description "zomb2" =
    "You are on top of boxes looking at a dead zombie"
description someplace = someplace ++ ", and you can't see anything."
