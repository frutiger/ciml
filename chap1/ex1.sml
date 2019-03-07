(*
Exercise 1.1

This simple program implements persistent functional binary search trees so that
if 'tree2 = insert(x, tree1)', then 'tree1' is available for lookups even when
'tree2' can be used.
*)

type key = string

datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert(key, LEAF) = TREE(LEAF, key, LEAF)
  | insert(key, TREE(l, k, r)) = if key < k
                                 then TREE(insert(key, l), k, r)
                                 else if key > k
                                      then TREE(l, k, insert(key, r))
                                      else TREE(l, key, r)

(*
a. Implement a 'member' function that returns 'true' if the item is found, else
'false'.
*)

fun member(key, LEAF) = false
  | member(key, TREE(l, k, r)) = if key < k
                                 then member(key, l)
                                 else if key > k
                                      then member(key, r)
                                      else key=k

(*
b. Extend the program to include not just membership, but mappings of keys to
bindings.
*)

datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

fun insert(LEAF, key, value) = TREE(LEAF, key, value, LEAF)
  | insert(TREE(l, k, v, r), key, value) =
      if key < k
      then TREE(insert(l, key, value), k, v, r)
      else if key > k
           then TREE(l, k, v, insert(r, key, value))
           else TREE(l, key, value, r)

exception KeyNotFound of key

fun lookup(LEAF, key) = raise KeyNotFound(key)
  | lookup(TREE(l, k, v, r), key) = if key < k
                                    then lookup(l, key)
                                    else if key > k
                                         then lookup(r, key)
                                         else v

(*
c. These trees are not balanced; demonstrate the behavior on the following
two sequences of insertions

(a) t s p i p f b s t

t
|\
s 0
|\
p 0
|\
i 0
|\
f 0
|\
b 0
|\
0 0

(b) a b c d e f g h i

a
|\
0 b
|\
0 c
|\
0 d
|\
0 e
|\
0 f
|\
0 g
|\
0 h
|\
0 i

d. Research balanced search trees in [Sedgewick 1997] and recommend a balanced-
tree data structure for functional symbol tables.  Hint: to preserve a
functional style, the tree should be one that rebalances on insertion but not on
lookup so a data structure such as splay trees is not appropriate.

A red-black tree could be appropriate.
*)
