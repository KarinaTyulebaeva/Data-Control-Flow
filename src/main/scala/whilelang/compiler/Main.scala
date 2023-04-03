package whilelang.compiler

import whilelang.util.Runner
import whilelang.parser.{DoesNotChangeAvailability, Node}

def action = Runner(program => List.empty)

@main def main(file: String) = action(file)
