import argparse
from enum import Enum
import re

## TODO:
## - Add something smarty pants to figure out what kind of wall it should be
## - There's likely a million ways to crash this script with wrong args
## - Weirdly shaped mazes are not caught
## - There is zero user feedback

## Customization with args
parser = argparse.ArgumentParser(
    description="Builds mazes from text files",
)
parser.add_argument(
    'filename', help="path to the file that needs to be parsed"
)
parser.add_argument(
    '--output', '-o', help="change the default output location/name (default: Out.hs)"
)
parser.add_argument(
    '--function', '-f', help="custom function name to use in the output (default: customMaze)"
)   
args = parser.parse_args()

## All the tiles go here
class Tile(Enum):
    WALL = "X"
    FLOOR = " "

## Read lines
xss = []
with open(args.filename) as file:
    xss = file.readlines()

## Remove unwanted characters, like \n
xss = [xs.strip() for xs in xss]

tiles = []
coordinates = []
for idy, xs in enumerate(xss):
    for idx, x in enumerate(xs):
        coordinates.append(f"({idx},{idy})")
        match x:
            case Tile.WALL.value:
                tiles.append("MkWall (MkWallShape Vertical)")
            case Tile.FLOOR.value:
                tiles.append("MkFloor EmptyTile")

ls = "[" # Open the list
for coord, tile in zip(coordinates, tiles):
    ls = ls + f"({coord}, {tile}),"
ls = ls[:-1] # Remove the last comma
ls += "]" # Close the list

## Get the out name from args
output_path = "Out.hs" if args.output is None else args.output

## Regex hell to remove everything but the filename as module name
module_name = re.search(r"[^\\/]+(?=\.\w+$)", output_path)[0]

## Get the function name from args
func_name = "customMaze" if args.function is None else args.function

## Open (and create) a file and write some Haskell to it 
with open(output_path, 'w+') as file:
    file.write(f"""module {module_name} where

import qualified Data.Map as Map
               

{func_name} :: Maze
{func_name} = foldr f Map.empty xs
 where 
  f (k, v) = Map.insert k v
  xs = {ls}
""")