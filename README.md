Cats Reflect
============
This project provides support for monadic reflection (Filinski [1994](https://dl.acm.org/citation.cfm?id=178047), [2010](https://dl.acm.org/citation.cfm?id=1706354))
to integrate monadic code with direct style code.

The implementation makes use of [`cats/StackSafeMonad.tailRecM`](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/StackSafeMonad.scala) for stack safety. That is, for the user program to be stack safe all reflected monads have to override `tailRecM` in a stack safe way.

Usage
-----
We collected some [examples](src/main/scala/cats/reflect/examples.scala) of how to use cats-reflect.

Dependencies
------------

## Dotty
The current cats-reflect API makes use of implicit function types of [Dotty](http://dotty.epfl.ch).

## Runtime
To implement monadic reflection we require some implementation of
(delimited) continuations. At the moment, cats-reflect only runs on
a JVM branch called [project loom](http://cr.openjdk.java.net/~rpressler/loom/Loom-Proposal.html) with runtime support for coroutines / delimited continuations.

To build the custom JVM, clone the repository
```
hg clone http://hg.openjdk.java.net/loom/loom
```

and checkout the continuation branch `cont`:
```
hg checkout cont
```

Detailed instructions on how to build the JDK can be found in the
file `doc/building.md`, in short those are:
```
bash configure
make images
```

Finally, run sbt with the newly built JVM. Assuming you checked out
loom into `PATH` and built on a mac, run:
```
sbt -java-home $PATH/build/macosx-x86_64-normal-server-release/images/jdk
```

Obviously the path needs to be adjusted for other operating systems.

## Libraries
The implementation uses [cats-core](https://typelevel.org/cats) for its [Monad](https://typelevel.org/cats/typeclasses/monad.html) typeclass.

The examples also use [cats-effect](https://typelevel.org/cats-effect).
