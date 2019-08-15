# Comparison of an Evolutionary Algorithm and Ant Colony Optimization as applied to a Path-finding Problem

Denton Cockburn, Victor Parmar

School of Computer Science, University of Windsor

cockbu3@uwindsor.ca, [vic@smalldata.tech](vic@smalldata.tech)

## Abstract

There are currently several Nature-Inspired Computing (NIC) methods. These include Genetic Algorithms (Holland, 1975), Particle Swarm Algorithms (Kennedy and Eberhart, 1995) and Ant Colony Optimization (Dorigo et al, 1991). These methods are able to provide very good solutions when applied to some very difficult problems. In this paper, we are looking in the domain of Path-finding. These algorithms have varying degrees of success in this area. What they have in common is an a priori knowledge of the search space, as well as a heuristic to guide their search towards goal states. These algorithms work well in situations where these requirements can be met, but are not applicable when they cannot. We seek to provide an evolutionary algorithm (HMA) that can provide good results without having a priori knowledge of the search space, as well as not having an initial heuristic with which to drive the algorithm's search.

## Execution

Execute `ants.lisp` in the REPL (loads `nomad.lisp` for the graph). Thereafter:

```lisp
* (INITIALIZE-ANT-WORLD)
NIL
* (DISPATCH-ANTS)
...
* (PRINT-ANTS-HISTORIES)
...
```
