import argparse
import sys

class Options():
    def __init__(self):
        self.parser = argparse.ArgumentParser(
            description="Builds mazes from text files",
        )

        self.parser.add_argument(
            'filename', help="path to the file that needs to be parsed"
        )
        self.parser.add_argument(
            '--output', '-o', help="change the default output location/name (default: Out.hs)"
        )
        self.parser.add_argument(
            '--module', '-m', help="custom module name (default: derived from filename)"
        )
        self.parser.add_argument(
            '--function', '-f', help="custom function name to use in the output (default: customMaze)"
        )   
        self.parser.add_argument(
            '--import', '-i', help="add a custom import to the top of the file (default: None)" 
        )
        self.parser.add_argument(
            '--verbose', '-v', help="prints more verbose output", action='store_const', const=True
        )
        self.args = vars(self.parser.parse_args())

        self.verbose = self.args["verbose"]
    
    def print_verbose(self, s):
        if self.verbose:
            print("[\033[94mINFO\033[00m] " + s)

    def print_warning(self, s):
        print("[\033[93mWARNING\033[00m] " + s)

    def print_error(self, s):
        print("[\033[91mERROR\033[00m] " + s)
        exit()

    def print_args(self):
        self.print_verbose("Enabled verbose output")
        self.print_verbose(f"Received args: \"{" ".join(sys.argv)}\"")
        if self.args["output"] is not None:
            self.print_verbose(f"Set output path: {self.args["output"]}")
        if self.args["module"] is not None:
            self.print_verbose(f"Set module name: {self.args["module"]}")
        if self.args["function"] is not None:
            self.print_verbose(f"Set function name: {self.args["function"]}")