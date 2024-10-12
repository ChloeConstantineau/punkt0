package io.punkt0

import java.io.File

case class Context(
    file: Option[File] = None,
    outDir: Option[File] = None,
    doEval: Boolean = false,
    doHelp: Boolean = false,
    doPrintMain: Boolean = false,
    doTokens: Boolean = false,
    doAST: Boolean = false,
    doPrintPrettyTree: Boolean = false,
    doSymbolIds: Boolean = false,
    doTypePrint: Boolean = false,
)
