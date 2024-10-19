from enum import Enum, auto
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
            # TODO: Complicated corners: 3-way corners, need a new sprite too


            # Complicated corners, inner corners
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
            if type(col) == RawTile:
                options.print_error("Failed to replace all tiles. Check for \"X\"")
            if col == Other.NOT_FOUND:
                options.print_error("Failed to recognize a tile. Check for \"E\"")

    options.print_verbose("Solved maze successfully")

    
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

# with open(output_path, 'w+') as file:
#     file.write(f"module {module_name} where\n\n")
    
#     if args["import"] is not None:
#         file.write(f"import {args["import"]}\n\n")
               
#     file.write(f"""import qualified Data.Map as Map
               
# -- generated using:
# -- python {args_comment}
# {func_name} :: Maze
# {func_name} = foldr f Map.empty xs
#  where 
#   f (k, v) = Map.insert k v
#   xs = 
# {ls}
# """)