package io.punkt0

abstract class Phase[-F, +T]:
    self =>

    infix def andThen[G](next: Phase[T, G]): Phase[F, G] = new Phase[F, G]:
        def run(v: F)(ctx: Context): G =
            val result = self.run(v)(ctx)
            next.run(result)(ctx)

    def run(v: F)(ctx: Context): T
