package io.punkt0.ast

import io.punkt0.ast.Trees.Tree

object PrettyPrinter:

    private val TAB_SIZE = 4

    // Note: StringLit print without the white spaces character
    def apply(input: Tree): String =
        val res   = new StringBuilder
        var level = 0
        input.toString.foreach:
            case '(' =>
              level += 1
              res.append("\n")
              res.append(("|" + " " * (TAB_SIZE - 1)) * (level - 1))
              res.append("|" + "-" * (TAB_SIZE - 1))
            case ')' =>
              level -= 1
            case ',' =>
              res.append("\n")
              res.append(("|" + " " * (TAB_SIZE - 1)) * (level - 1))
              res.append("|" + "-" * (TAB_SIZE - 1))
            case ' ' =>
            case f   => res.append(f)
        res.toString()
