# day8

- distances: 1000x1000 
  - really: only n(n-1)/2 distances needed to compute
- connections
  - only need to store the 1000 min pairs (priority queue)
- but how to keep track of the components?

first thought: map of box -> component (initially all singleton components)
when connecting a pair, components are "merged" into one, then updated in the map?
But... how are two components merged?

pointer to mutable Comp
IORef (IORef Comp)


  1 2 3 4 5
1 . x . . .
2 - . . . . 
3 - - . . x
4 - - - . .
5 - - - - . 