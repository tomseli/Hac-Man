from enum import Enum
from options import Options
import copy

class RawTile(Enum):
    WALL = "X"
    EMPTY = " "
    PELLET = "."
    SUPER = "O"

# NE means North and East are connected, etc
class TileCorner(Enum):
    NE = "└"
    SE = "┌"
    SW = "┐"
    NW = "┘"
    
class TileWall(Enum):
    HORIZONTAL = "-"
    VERTICAL   = "|"

class TileOther(Enum):
    EMPTY  = " "
    PELLET = "."
    SUPER  = "O"

class Other(Enum):
    NOT_FOUND = "E"

def main():
    options = Options()
    options.print_args()

    options.print_verbose("Loading input file...")
    raw_strings = []
    with open(options.args["filename"], 'r') as file:
        raw_strings = file.readlines()
    raw_strings = [s.strip() for s in raw_strings]
    options.print_verbose("Sucesfully loaded input file")

    options.print_verbose("Validating input shape")
    ncol = len(raw_strings[0])
    for row in raw_strings:
        if len(row) != ncol: 
            options.print_error(f"{len(row)} != {ncol}; shape is inconsistent")
    options.print_verbose("Successfully validated input shape")

    options.print_verbose("Parsing input...")
    parsed_input = [[]]
    for idy, row in enumerate(raw_strings):
        for char in row:
            parsed_input[idy].append(RawTile(char))
        parsed_input.append([])
    # remove last appended list
    parsed_input = parsed_input[:-1]
    options.print_verbose("Successfully parsed input")

    options.print_verbose("Solving shapes...")
    
    def is_wall(tile):
        return tile == RawTile.WALL
    
    output = copy.deepcopy(parsed_input)
    m_shape = (len(parsed_input)-1, len(parsed_input[0])-1)
    for idy, row in enumerate(parsed_input):
        for idx, _ in enumerate(row):
            current = parsed_input[idy][idx]

            # Check if empty, pellet or super pellet
            if current == RawTile.EMPTY:
                output[idy][idx] = TileOther.EMPTY
                continue
            if current == RawTile.PELLET:
                output[idy][idx] = TileOther.PELLET
                continue
            if current == RawTile.SUPER:
                output[idy][idx] = TileOther.SUPER
                continue

            # Check above
            if not (idy - 1) < 0:
                above = is_wall(parsed_input[idy-1][idx])
                # print(idy, idx, is_wall(parsed_input[idy-1][idx]), parsed_input[idy-1][idx])
            else:
                above = False 

            # Check below
            if not (idy + 1) > m_shape[0]:
                below = is_wall(parsed_input[idy+1][idx])
                # print(idy, idx, is_wall(parsed_input[idy+1][idx]), parsed_input[idy+1][idx])
            else:
                below = False

            # Check left
            if not (idx - 1) < 0:
                left = is_wall(parsed_input[idy][idx-1])
            else:
                left = False

            # Check left
            if not (idx + 1) > m_shape[1]:
                right = is_wall(parsed_input[idy][idx+1])
            else:
                right = False

            # We should only do one, continue after a hit
            # Complicated corners; inner corners
            # These are not guared for the edge, keep that in mind
            # But it is likely okay because above, below, left and right are true 
            if above and below and left and right:
                if not is_wall(parsed_input[idy-1][idx+1]):
                    output[idy][idx] = TileCorner.NE
                elif not is_wall(parsed_input[idy+1][idx+1]):
                    output[idy][idx] = TileCorner.SE
                elif not is_wall(parsed_input[idy+1][idx-1]):
                    output[idy][idx] = TileCorner.SW
                elif not is_wall(parsed_input[idy-1][idx-1]):
                    output[idy][idx] = TileCorner.NW
                # If none of the above apply, we're in the middle of
                # a block of walls. An empty tile will do
                else:
                    output[idy][idx] = TileOther.EMPTY
                
            # Straight walls
            elif below and above:
                output[idy][idx] = TileWall.VERTICAL
            elif left and right:
                output[idy][idx] = TileWall.HORIZONTAL

            # Easy corners
            elif above and right:
                output[idy][idx] = TileCorner.NE
            elif below and right:
                output[idy][idx] = TileCorner.SE
            elif below and left:
                output[idy][idx] = TileCorner.SW
            elif above and left:
                output[idy][idx] = TileCorner.NW
            else:
                # If we didnt continue yet, we failed to recognize the needed tile
                output[idy][idx] = Other.NOT_FOUND
                options.print_warning(f"Couldn't solve (y: {idy}, x: {idx})")

    options.print_verbose("Verifying if replaced all tiles...")
    print_maze(output)
    for idy, row in enumerate(output):
        for idx, col in enumerate(row):
            # Highest prio error, a bug caused not all tiles to be replaced
            if type(col) == RawTile:
                options.print_error("Failed to replace all tiles. Check for \"X\"")
            # Lower prio error, likely some illegal wall combination
            elif col == Other.NOT_FOUND:
                options.print_error("Failed to recognize a tile. Check for \"E\"")
    options.print_verbose("Successfully solved maze")

    options.print_verbose("Preparing output...")
    output_data = []
    for idy, row in enumerate(output):
        for idx, col in enumerate(row):
            wall = "MkWall"
            floor = "MkFloor"
            tile = ""
            if type(col) == TileWall:
                match col.value:
                    case TileWall.VERTICAL.value:
                        tile = f"{wall} (MkWallShape Vertical)"
                    case TileWall.HORIZONTAL.value:
                        tile = f"{wall} (MkWallShape Horizontal)"
            elif type(col) == TileCorner:
                match col.value:
                    case TileCorner.NE.value:
                        tile = f"{wall} (MkCorner NE)"
                    case TileCorner.SE.value:
                        tile = f"{wall} (MkCorner SE)"
                    case TileCorner.SW.value:
                        tile = f"{wall} (MkCorner SW)"
                    case TileCorner.NW.value:
                        tile = f"{wall} (MkCorner NW)"
            elif type(col) == TileOther:
                match col.value:
                    case TileOther.EMPTY.value:
                        tile = f"{floor} EmptyTile"
                    case TileOther.PELLET.value:
                        tile = f"{floor} (MkConsumable Pellet)"
                    case TileOther.SUPER.value:
                        tile = f"{floor} (MkConsumable SuperPellet)"
            else:
                options.print_error("Failed to match output to tile")
            # Note, idx and idy invert here!
            s = f"(({idx}, {idy}), {tile})"
            output_data.append(s)
    pretty_data = "\n    [ " + output_data[0] + "\n"
    for data in output_data[1:]:
        pretty_data += "    , " + data + "\n"
    pretty_data += "    ]"
    options.print_verbose("Successfully prepared output")

    options.print_verbose("Writing output...")
    path = options.args["output"]
    func = options.args["function"]
    module = options.args["module"]

    with open(path, 'w+') as file:
        file.write(f"module {module} where\n\n")
        
        if options.args["import"] is not None:
            file.write(f"import {options.args["import"]}\n\n")

        file.write(f"""import qualified Data.Map as Map

-- generated using:
-- python {options.get_argv()}
{func} :: Maze
{func} = foldr f Map.empty xs
 where 
  f (k, v) = Map.insert k v
  xs = {pretty_data}
""")
    options.print_verbose(f"Successfully wrote to {options.args["output"]}")
    
def print_maze(maze):
    print()
    print("    ", end="")
    print(*(f"{x//10}" if (x % 10) == 0 else " " for x in range(len(maze[0]))))

    print("    ", end ="")
    print(*(f"{x % 10}" for x in range(len(maze[0]))))
    for idy, row in enumerate(maze):

        if (idy % 10) == 0:
            leading = f"{idy // 10} "
        else:
            leading = "  " 
        print(leading, end="")
        print(f"{idy % 10} ", end="")
        
        for idx, _ in enumerate(row):
            print(f"{maze[idy][idx].value} ", end="")
        print()  
    print()

if __name__ == "__main__":
    main()
