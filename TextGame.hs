import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("spider", "down"), "cave"),
    (("cave", "up"), "spider"),
    (("cave", "west"), "cave entrance"),
    (("cave entrance", "east"), "cave"),
    (("cave entrance", "south"), "meadow"),
    (("meadow", "south"), "building"),
    (("meadow", "north"), "cave entrance"),
    (("building", "west"), "cage"),
    (("building", "north"), "meadow"),
    (("building", "east"), "closet"),
    (("cage", "east"), "building"),
    (("closet", "west"), "building")
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("fuel", "spider"),
    ("key", "cave entrance"),
    ("lantern", "building"),
    ("bow and arrow", "closet"),
    ("myself", "meadow"),
    -- This is a hack, so I don't have to add more data to the "World" state
    ("spider", "alive")
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
