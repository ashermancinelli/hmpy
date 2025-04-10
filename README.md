# Hindley Milner Type Checking

The AST types are a basic DSL for constructing ASTs that the type checker understands.

This is entirely based on three resources: 

1. [This paper by Luca Cardelli](http://lucacardelli.name/Papers/BasicTypechecking.pdf)
2. [This perl script by Nikita Borisov](https://web.archive.org/web/20050420002559/http://www.cs.berkeley.edu/~nikitab/courses/cs263/hm.pl)
3. [This python script by Robert Smallshire](https://raw.githubusercontent.com/rob-smallshire/hindley-milner-python/refs/heads/master/inference.py)
https://www.cs.utexas.edu/~bornholt/courses/cs345h-24sp/lectures/8-system-f/
https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf



The paper contains an implementation in modula2, and the perl/python scripts are rewrites of the
modula2 code. At times I found either script to be more useful, but I most heavily referenced the modula2
version.

The script contains FileCheck directives, so piping the output of this script to FileCheck as seen
in the RUN line at the top of the file will test the implementation, in case you want to add more
test cases.
