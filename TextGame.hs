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

