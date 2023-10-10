## Extra Hardcore Optional Math

If you are intrested in the common structure between Integers, MultiSets and Booleans, it is called a [ring](https://en.wikipedia.org/wiki/Ring_(mathematics)) in mathematics. A ring is any set A with the following:
* A binary operation called + and a binary operation called *
* A unary negation operation (unary = with one argument) called -. For example in Integers, you can negate 3 to get -3.
* An element called 0 and element called 1

Such that the following rules hold:
* A with `+` and `0` form a monoid (additive monoid)
* A with `*` and `1` also forms a monoid (multiplicative monoid)
* `+` is commutative: `a + b = b + a` forall a b in A
* Multiplication is distributive with respect to addition, that is
    * `a * (b + c) = (a * b) + (a * c)` for all a, b, c in A
    * `(b + c) * a = (b * a) + (c * a)` for all a, b, c in A
* `-a` is the additive inverse of a. That is: `a + (-a) = 0` forall a.
You can then define a binary operation - as `a - b = a + (-b)`

For Integers, `+`,`*`,`-`,`0` and `1` are defined (unsuprisingly) as `+`,`*`,`-`,`0` and `1`. For Multisets:
* `0` = Empty multiset
* `1` = Multiset containing everything once 
* `+` = Elementwise `+` 
* `*` = Elementwise `*`
* Negation = Negating the multiplicity of all elements. For example, the multiset `{a -> 2, b -> 3}` becomes the multiset `{a -> -2, b -> -3}`. For this you need to allow multisets with negative occurances.

In the exercise, we define Multiset `*` differently, namely as elementwise `min` instead of elementwise `*`. With this definition of `*`, with elementwise `min`, the Multiset does not form a ring: distributivity is broken (and 1 is not a multiplicative unit, which could be solved by setting 1 to the multiset containing everything infinitely often). However, this definition does add the rule `e * e = e`, which is a rule that holds for multisets but nothing else. In this exercise it is important that the set of rules for Multisets and Integers is not the same, as this means you need to use dynamic dyspatch to share (and override) the simplification code. It would be nicer if the Multiset definition also gives rise to a ring, but I do not see how that could lead to also some special simplification rule for Multisets which is not present for booleans.

For Booleans, `+`,`*`,`-`,`0` and `1`  are defined as for the ring (actually field) modulo 2. That is:
* `0` = False
* `1` = True
* `+` = XOR (not OR)
* `*` = AND
* `Negation` = Identity function. `-a = a` 
(You can check that the rules above actually hold for this.)
