
type key = string

(*** binary tree ***)
datatype 'a tree = LEAF
                 | TREE of 'a tree * (key * 'a) * 'a tree

val empty = LEAF

fun insert (pair, LEAF) = TREE (LEAF, pair, LEAF)
  | insert (pair as (key, _), TREE (l, pair' as (key', _), r)) = 
      if key < key' then 
        TREE (insert (pair, l), pair', r)
      else if key > key' then 
        TREE (l, pair', insert (pair, r))
      else
        TREE (l, pair, r)

fun member (_, LEAF) = NONE
  | member (key, TREE (l, (k, v), r)) = 
      if key < k then
        member (key, l)
      else if key > k then
        member (key, r)
      else
        SOME v

val t = foldl insert empty [
  ("1", "a"), ("5", "e"), ("10", "j"), ("8", "h"), ("17", "q"), 
  ("23", "w"), ("2", "b"), ("24", "x"), ("6", "f")]