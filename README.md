Idris to PHP back end
---------------------

Please, don't ever use this.

Example
-------

    beaker:edwin$ cat pythag.idr 
    module Main

    pythag : Int -> List (Int, Int, Int)
    pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                              x * x + y *y == z * z]

    main : IO ()
    main = print (pythag 50)

    beaker:edwin$ idris pythag.idr --codegen php -o pythag.php
    beaker:edwin$ php pythag.php 
    [(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15)), (8, (15, 17)), 
     (12, (16, 20)), (15, (20, 25)), (7, (24, 25)), (10, (24, 26)), (20, (21, 29)), 
     (18, (24, 30)), (16, (30, 34)), (21, (28, 35)), (12, (35, 37)), (15, (36, 39)), 
     (24, (32, 40)), (9, (40, 41)), (27, (36, 45)), (30, (40, 50)), (14, (48, 50))]

Why?
----

I'm glad you asked. While I'd never recommend using this for anything real, there
is a reason why making this has been an interesting exercise (other than being
a bit of fun.)

These days, there are lots of platforms on which we might run our software,
such as JVM, .NET, JavaScript in the browser and so on. It's a shame that the
choice of platform limits the languages we can use to develop on that platform,
so if there is a way of implementing a language such that it can be easily
retargetted to another platform, that would be a good thing.  With Idris, one
thing we're trying to do is make it easy to write new backends to do just that. 
Hopefully, developers of other languages will steal our ideas here.

This, therefore, has been an exercise in using an Idris intermediate
representation to target another platform, PHP. Many hosting providers, for
example, still support PHP but don't necessarily allow you to write in anything
nicer (maybe they'll let you write Node.js, but it's debatable how much nicer
that is...).  Despite PHP's lack of support for algebraic data types, pattern
matching, closures, laziness, etc, Idris translates to an intermediate
representation that compiles all of these things away.

I've found a thing or two that wasn't as nice as it should have been, which I
will fix so that it will be an easier job for anyone writing a back end for any
future platform which I haven't thought of, or maybe hasn't been invented yet.

Nevertheless, it still comes in at under 200 lines for getting a basic thing up
and running (only a couple of primitives and basic operators implemented but
the main work of compiling tricky program structures is done) which means that
it shouldn't be much harder to write a back end for a target you'd actually
want to run things on!



