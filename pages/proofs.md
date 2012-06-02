-----
title: Propositions and Predicates
date: 16th Febuary 2011
prev: <a href="/pages/peano.html">← Hello, Peano</a>
next: <a href="#">Peano Proofs →</a>
-----

“Logic is the art of going wrong with confidence”
=================================================

Now that we've defined the natural numbers, we're going to do some simple example proofs of some basic mathematical properties. Before we do this, though, I'm going to
introduce you to a bit of logic. If you're already familiar with predicate logic, you may still want to read this section, as it clarifies its relationship with Agda
programming.

At a fundamental level, logic is the mathematics of *propositions*. Propositions are judgements that may be true, false, proven, or unproven. They are simply statements
in some mathematical language.

Logic also introduces a bunch of connectives to relate propositions together, to form new propositions. Perhaps the simplest one is **implication**, usually written `A ⇒ B`. 
It is considered true if, whenever `A` is true, `B` is true as well. In other words, this proposition states that if we know `A` is true, then we automatically know `B` is true.
Note that implication is a *right-associative* connective, which means that `A ⇒ B ⇒ C` means `A ⇒ (B ⇒ C)`, not `(A ⇒ B) ⇒ C`. 

Another connective is called **conjunction**, usually written `A ∧ B`, which is true if and only if *both* `A` and `B` are true. Unlike implication, conjunction is freely
associative and commutative, meaning that the order you put terms in doesn't matter, and it doesn't matter where you put parentheses in the terms either.

Finally, there is **disjunction**, usually written `A ∨ B`, which is true if *either* `A` or `B` are true. Once again, it is freely associative and commutative.

How does this all relate to Agda?
=================================

Agda's types correspond to propositions. If we can construct a value of a certain type, we have constructed a proof that the proposition encoded by that type holds. For example, here is a proof in Agda that there exists a natural number somewhere in the universe:

~~~~~{.agda}
-- \_1 to type ₁
proof₁ : ℕ
proof₁ = suc (suc (suc (suc (suc zero))))

~~~~~

Not a very interesting proof, I'll grant, but it does illustrate an important concept. The *type* of a binding corresponds to our *proof obligation* - in this case,
the type is saying "Prove that there exists a value in `ℕ`!". The *value* of the binding is our proof. We give it the natural number 5, to say "Here, 5 is a natural number,
therefore natural numbers exist!".

This is called a proof by *construction*, because we demonstrate that a proposition holds by constructing an example that shows it to be true.


Implication
-----------

What if we wanted to prove a more complicated proposition than the one above? If we wanted to show an implication, for example? In Agda, an implication proposition corresponds to 
a *function type*. 

When we prove an implication, say `A ⇒ B`, we assume the premise (`A`) to be true, and from there try and derive the conclusion (`B`). If we think in terms of proof by construction, this is 
the same as implementing a function - a function, when given an argument of type `A`, constructs an return value of type `B`. In other words, when given a proof (by construction) of 
some proposition `A`, our function will produce a proof of some proposition `B` - This is exactly the same as our proof method for implication! If we can write such 
a function (and Agda checks it), then simply by writing that function we have constructed a proof of `A ⇒ B`.

Consider a simple statement that is tautologically true: `A ⇒ A` (or, if A is true, then A is true). Let's prove this obvious statement in Agda! 

First, let's prove it just for the proposition that natural numbers exist (i.e if natural numbers exist, then natural numbers exist).

~~~~~{.agda}

proof₂ : ℕ → ℕ 
proof₂ ν = ν      -- \nu to type ν.

~~~~~

So, this tautology proof corresponds to the identity function. The reason for this is fairly clear: Given a proof of `A`, there's only one real way to produce a proof of `A`, and 
that is to produce the proof you were just given.

Universal Quantification
------------------------

It would be nice though, to make this proof about *all* propositions, not merely the proposition that natural numbers exist - after all, the proof is the same regardless
of the proposition involved!

To do this, we have to exploit Agda's flexible type system a little. We make our identity function take an additional parameter - a type. Given a type, we then return an identity
function, instantiated for that type. 

~~~~~{.agda}
proof₂′ : (A : Set) → A → A
proof₂′ _ x = x
~~~~~

The new type signature here means: Given some value of type `Set` (i.e a type), called `A`, this returns a function from `A` to `A`. Alternatively, we could view it as a logical
statement: For any proposition `A`, `A ⇒ A`. In logic, we use the symbol `∀` to mean "for any" or "for all". So, the above type signature could be written in logic as: 

    ∀A ⇒ A ⇒ A 

Making propositions about *all* members of a set (or universe) is called *Universal Quantification*, and it corresponds to parametric polymorphism (including Java generics and C++ templates) in 
type system lingo.

Now we can implement our special case proof in terms of the more general one:

~~~~{.agda}
proof₂ : ℕ → ℕ 
proof₂ = proof₂′ ℕ
~~~~

<div class="aside">

Implicit Parameters
-------------------

It can be annoying, though, to have to pass explicit type terms around - languages that implement parametric polymorphism such as Haskell do not require you to manually specify
each type you are using in place of the type variables in a polymorphic function, and indeed it can be quite cumbersome to do so in Agda.

Agda has a solution for this problem, however, called *implicit parameters*. Lots of features in other languages are called "implicit parameters", but forget all of those, as
Agda's are likely to be different. 

In a situation where some arguments to a function can be completely *inferred* from the other arguments, then you can define those inferrable
arguments to be *implicit* and Agda will infer the correct argument for you at compile-time. This example above is a classic use case. If we simply change our round parentheses
to curly braces, like so:

~~~~{.agda}
id : { A : Set } → A → A
id a = a
~~~~{.agda}

Then we no longer need to match on the type argument, nor do we even need to specify it in the function call - `id zero` does the trick, giving us `zero`. Agda infers that
the implicit argument is `ℕ` by examining the type of the first real argument (`zero`).

If you still want to specify (or pattern match) on an implicit argument, this can still be done, simply by surrounding it with curly braces: `id {ℕ} zero`.

</div>

Conjunction
-----------

Unlike universal quantification or implication, Conjunction and disjunction do not correspond to built-in types in Agda, however they are fairly straightforward to define.

When we prove a conjunction on pen and paper, we simply prove both of the two components of the conjunction. If we have a proof for both components, then we automatically
have a proof of a conjunction. This means that conjunction corresponds to a *pair* or a *tuple* (more formally known as a *product type*) in Agda. 

~~~~~{.agda}
data _∧_ (P : Set) (Q : Set) : Set where -- \and for ∧
   ∧-intro : P → Q → (P ∧ Q)
~~~~~

Here we've defined a new data type, this time it is *parameterized* by two types/propositions, which make up the components of the conjunction. Conjunction itself is also a 
proposition, so we give it the type Set.

Notice how the `∧-intro` constructor can only produce a proof of `P ∧ Q` if it is passed both a proof of `P` and a proof of `Q`. This is how conjunction is demonstrated by *construction*
 - it is impossible to create a conjunction that is not supported by proofs for both components.

Using this we can prove some simple properties about conjunctions, such as: `P ∧ Q ⇒ P`:

~~~~~{.agda}
proof₃ : {P Q : Set} → (P ∧ Q) → P
proof₃ (∧-intro p q) = p
~~~~~

Bijection
---------
                                                     
Now that we have defined conjunction and implication, we can define a notion of logical *equivalence*. Two propositions are *equivalent* if both propositions can be considered to
be the same. This is defined as: if one is true, the other is also true. In logic, this is called *bijection* and is written as `A ⇔ B`. Bijection can be expressed simply as a conjunction of two
implications: If A is true then B is true, and if B is true then A is true.

~~~~{.agda}
_⇔_ : (P : Set) → (Q : Set) → Set -- \<=> to type ⇔
a ⇔ b = (a → b) ∧ (b → a)
~~~~~

Serious Proofs
--------------

Using this we can come up with some proofs of the algebraic properties of conjunction. The commutative property says that `A ∧ B ⇔ B ∧ A`, i.e the order of arguments does not 
matter. Let's prove it:

~~~~~{.agda}
∧-comm′ : {P Q : Set} → (P ∧ Q) → (Q ∧ P)
∧-comm′ (∧-intro p q) = ∧-intro q p

∧-comm : {P Q : Set} → (P ∧ Q) ⇔ (Q ∧ P)
∧-comm = ∧-intro (∧-comm′ {P} {Q}) (∧-comm′ {Q} {P}) -- implicits provided for clarity only. 
~~~~~

Let's also prove associativity (i.e that brackets don't matter):

~~~~~{.agda}
∧-assoc₁ : { P Q R : Set } → ((P ∧ Q) ∧ R) → (P ∧ (Q ∧ R))
∧-assoc₁ (∧-intro (∧-intro p q) r) = ∧-intro p (∧-intro q r)

∧-assoc₂ : { P Q R : Set } → (P ∧ (Q ∧ R)) → ((P ∧ Q) ∧ R)
∧-assoc₂ (∧-intro p (∧-intro q r)) = ∧-intro (∧-intro p q) r

∧-assoc : { P Q R : Set } → ((P ∧ Q) ∧ R) ⇔  (P ∧ (Q ∧ R))
∧-assoc = ∧-intro ∧-assoc₁ ∧-assoc₂
~~~~~

Disjunction
-----------

If conjunction is a *pair*, because it requires *both* proofs to hold, then disjunction is a *sum type* (also known as an `Either` type), because it only requires one proof 
in order to hold. In order to model this in Agda, we add *two* constructors to the type, one for each possible component of the disjunction.

~~~~~{.agda}
data _∨_ (P Q : Set) : Set
   ∨-intro₁ : P → P ∨ Q
   ∨-intro₂ : Q → P ∨ Q
~~~~~

Using this, we can come up with some interesting proofs. The simplest one to prove is *disjunction elimination*, which is: `∀A B C ⇒ ((A ⇒ C) ∧ (B ⇒ C) ∧ (A ∨ B))⇒ C`. In plain 
English, this means "If I know that `C` is true if `A` is true, and that `C` is true if `B` is true, and that `A` or `B` is true, then I know `C` is true".

~~~~~~{.agda}
∨-elim : {A B C : Set} → (A → C) → (B → C) → (A ∨ B) → C
∨-elim ac bc (∨-intro₁ a) = ac a
∨-elim ac bc (∨-intro₂ b) = bc b
~~~~~~~

We can also prove the algebraic properties of disjunction, such as commutativity:

~~~~~
∨-comm′ : {P Q : Set} → (P ∨ Q) → (Q ∨ P)
∨-comm′ (∨-intro₁ p) = ∨-intro₂ p
∨-comm′ (∨-intro₂ q) = ∨-intro₁ q

∨-comm : {P Q : Set} → (P ∨ Q) ⇔ (Q ∨ P)
∨-comm = ∧-intro ∨-comm′ ∨-comm′
~~~~~

The associativity proof is left as an exercise to the reader[^1]. 

Negation
--------

You have probably noticed if you're familiar with boolean logic that I've avoided mentioning *false* throughout this entire chapter. Unlike boolean logic, Agda's *intuitionistic*
logic does not have a well-defined notion of "false". In *classical* and boolean logics, all propositions are considered to be either true or false. Intuitionistic logic, by
contrast, is purely *constructive*. You can either construct a proof for a proposition, making it true, or you fail to construct a proof, making you feel bad.

The only "false" values that exist in intuitionistic logic, therefore, are values for which *there can exist no proof*. In Agda, this corresponds to a type that contains no values.
We call this type `⊥`, pronounced "bottom". We define it like so:

~~~~~{.agda}
data ⊥ : Set where -- nothing

~~~~~

That's right. No, it's not a mistake. There are no constructors for `⊥`. It is a type for which it is *impossible* to produce a value. 

Having such a value allows us to define negation (`¬A`) as true if `A` being true would mean bottom is true (which is impossible). Or, in more formal terms:  `¬A ⇔ (A ⇒ ⊥) `

~~~~~{.agda}
¬ : Set → Set -- for ¬ type \neg
¬ A = A → ⊥
~~~~~

The Curry Howard Correspondence
===============================

This chapter has taught you how to encode propositional logic into Agda's type system. The correspondences discussed here between disjunction and sum types, conjunction and
product types, functions and implication, and propositions and types are the fundamentals behind the Curry Howard correspondence. Using these tools, you can encode any
constructive proof in Agda, which covers a vast range of possible proofs, including the vast majority of proofs encountered in program verification.

The next chapter will introduce relational equality, and begin proving some theorems about the Peano numbers we introduced in the previous chapter. Happy Hacking!







[^1]: Muahahaha.
