package io.punkt0

import java.io.File

class Reporter(file: File) {

  def info(msg: String, pos: Position): Unit =
    report(pos, s"[info] $msg")

  def warning(msg: String, pos: Position): Unit = {
    report(pos, s"[warning] $msg")
  }

  def error(msg: String, pos: Position): Unit = {
    report(pos, s"[error] $msg")
  }

  def fatal(msg: String, pos: Position): Nothing = {
    report(pos, s"[fatal] $msg")
    sys.exit(1)
  }

  def terminate: Unit = {
    Console.err.println("Errors reported.")
    sys.exit(1)
  }

  private def report(
      position: Position,
      message: String
  ): Unit = {
    val output = position match {
      case p: Position => // Add "p.show"
        s"""
           |${p.location}: $message
        """
      case _ => s"$message"
    }

    Console.err.println(output)
  }
}
