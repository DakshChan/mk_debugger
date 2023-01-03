# miniKanren with a first-order representation

This implementation of miniKanren decouples the search strategy from the representation of the search space.  This makes it possible to use a variety of strategies, perform program transformations (even while a program is running), and implement tools such as a debugger.

Learn more from the [2019 miniKanren Workshop paper](http://minikanren.org/workshop/2019/minikanren19-final2.pdf).
