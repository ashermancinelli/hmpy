#! /usr/bin/env python3
import ast
import argparse
import sys

def main():
    parser = argparse.ArgumentParser(description="Compiler for the Tiny language")
    parser.add_argument("input_file", type=str, help="The input file to compile")
    args = parser.parse_args()

    with open(args.input_file, "r") as file:
        code = file.read()

    tree = ast.parse(code)
    print(ast.dump(tree, indent=2))

if __name__ == "__main__":
    sys.exit(main())
