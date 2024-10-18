import argparse
from enum import Enum
import re
import sys

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
    '--module', '-m', help="custom module name (default: derived from filename)"
)
parser.add_argument(
    '--function', '-f', help="custom function name to use in the output (default: customMaze)"
)   
parser.add_argument(
    '--import', '-i', help="add a custom import to the top of the file (default: None)" 
)
parser.add_argument(
    '--verbose', '-v', help="prints more verbose output", action='store_const', const=True
)
args = vars(parser.parse_args())

verbose = args["verbose"]

## Print with pretty colors
def printInfo(s):
    print("[\033[94mINFO\033[00m] " + s)

## All the tiles go here
class Tile(Enum):
    WALL = "X"
    FLOOR = " "

## Write a nice comment
args_comment = " ".join(sys.argv)

if verbose:
    printInfo("Found flags:")
    printInfo(args_comment)

## Read lines
xss = []
with open(args["filename"]) as file:
    xss = file.readlines()

if verbose:
    printInfo("Maze data found")
    for idx, xs in enumerate(xss):
        print(f"{idx}:  {xs.strip()}")

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
if verbose: 
    printInfo("Parsed file sucessfuly")

ls = "    [" # Open the list
ls += f" ({coordinates[0]}, {tiles[0]})\n" 

for coord, tile in zip(coordinates[1:], tiles[1:]):
    ls += f"    , ({coord}, {tile})\n"
    
ls += "    ]" # Close the list

if verbose: 
    printInfo("Built String from data")

## Get the out name from args
output_path = "Out.hs" if args["output"] is None else args["output"]

## Regex hell to remove everything but the filename as module name
if args["module"] is None:
    module_name = re.search(r"[^\\/]+(?=\.\w+$)", output_path)[0]  
else:
    module_name = args["module"]

## Get the function name from args
func_name = "customMaze" if args["function"] is None else args["function"]

if verbose:
    printInfo(f"Set output path: {output_path}")
    printInfo(f"Set module name: {module_name}")
    printInfo(f"Set function name: {func_name}")

## Open (and create) a file and write some Haskell to it 
with open(output_path, 'w+') as file:
    file.write(f"module {module_name} where\n\n")
    
    if args["import"] is not None:
        file.write(f"import {args["import"]}\n\n")
               
    file.write(f"""import qualified Data.Map as Map
               
-- generated using:
-- python {args_comment}
{func_name} :: Maze
{func_name} = foldr f Map.empty xs
 where 
  f (k, v) = Map.insert k v
  xs = 
{ls}
""")