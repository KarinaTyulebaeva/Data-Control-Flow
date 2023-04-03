package whilelang.interpreter

import whilelang.util.Runner

def action = Runner(program => program.checkAvailability(program.execute))

@main def main(file: String) = println(action(file))
