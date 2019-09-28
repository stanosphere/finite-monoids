# Finite Monoids

This repo is for me to play around with finite monoids as a vehicle for learning about scala, functional programming, and group theory.
My inspiration to create this project came from the excellent book: "Functional Programming in Scala" (https://github.com/fpinscala/fpinscala)

## What is a Monoid?

## What is Isomorphism?

## Isomorphism Detection

Suppose we have to monoids of type `A` and type `B`

In general how can one tell if a pair of such monoids are isomorphic?

The answer is by comparing Cayley Tables!
Any Magma can be represented by a Cayley Table.
The Cayley Table captures the _structure_ of the Magma.
A Magma is simply the name for a set of elements equipped with a single binary operation that is closed over those elements.
There is no escape from a Magma.
All Monoids are Magmas.
A Monoid is a special case of a Magma.
- By comparing the Cayley tables!
- 

### An interesting motivating example
TODO: Make this into a nice mark down table
```$scala
//0, 1, 2
//1, 1, 1
//2, 1, 1
//
//0, 1, 2
//1, 2, 2
//2, 2, 2
```

## How Many Monoids?

- Can I generate a monoid instance directly from a cayley table?
- Can I generate all unique cayley tables of a given size?
- Can I then use this to count monoids?
- Can I tell if a monoid is a group?
- Can I use this informtion to count groups?
- https://en.wikipedia.org/wiki/Finite_group#Table_of_distinct_groups_of_order_n