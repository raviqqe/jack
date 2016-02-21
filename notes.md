# Notes

## Projection?

They may be syntax sugar.

```
foo.bar == foo |> bar
```

when `bar : A -> B -> C`,

```
bar a b c == a.bar b c
```

but, but...

```
(bar . foo) x == bar (foo x)
```

where `foo : D -> A`

### Candidate 1

```
foo.bar == foo .bar == bar foo
```

Parse `.bar` as an operator and then,

```
baz foo.bar == baz foo .bar === baz (foo.bar)
```

Of course,

```
EXPRESSION.bar == EXPRESSION .bar == bar EXPRESSION
```

### Candidate 2

function composition by `*`

```
foo : A -> B
bar : B -> C
bar * foo
```

Then,

```
foo.bar == foo .bar == foo . bar == foo. bar
```


## The lefthand sides of type definitions

Every lefthand side of type definitions has one unique syntax of;

```
lefthand_side = identifier { identifier }
```

For example,

```
Monad a = ...
Car a b c = ...
Robot x y = ...
Vegetable = ...

```

So, matching parts of concrete types is easy
when checking inclusion relations of record types is done.
(`~=` indicates the left one matches with the right one.)

```
Monad a ~= Car a b ~= Robot x
```
