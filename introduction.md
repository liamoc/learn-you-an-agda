Introduction
============

About this tutorial
-------------------

Welcome to *Learn You an Agda and Achieve Enlightenment!*. If you're reading this, 
you're probably curious as to what Agda is, why you want to learn it, and in general what 
the big deal is about dependently typed, purely functional programming.

Inspired by BONUS, the writer of [Learn You a Haskell](learnyouahaskell.com), I decided
that I should write an approachable Agda tutorial that would introduce dependently typed
programming to ordinary people rather than Ivory Tower Academics. Of course, seeing as
*I* am one of those Ivory Tower Academics, this might not be easy. I am, however, prepared
to give it a try. Learning Agda was a very rewarding but very difficult process for me. It
is my hope that, by writing this tutorial, it will become a little bit easier for everyone
else.

This tutorial is not aimed at anyone completely new to programming. Agda is similar on
a basic level to typed functional languages such as Haskell and ML, and so knowing a 
language in the ML family will certainly make learning Agda a great deal easier, however
those languages are not a prerequisite for this tutorial. Solid understanding of 
imperative programming (C, Scala, Ruby..) as well as the logical constructs that go with
it is assumed.

It took a long time for everything in Agda to fall into place in my head. Agda is *hard*.
After some time, though, Agda's inherent awesomeness comes to the fore, and it all just
'clicked'. If you encounter obstacles in your Agda learning, don't be discouraged! Keep
working, and eventually you will be a master of Agda fu.

What is Agda, anyway?
---------------------

Agda is a *dependently typed*, *purely functional* programming language based on *intuitionistic
logic* or, more accurately, *Martin-Löf type theory*. Because of these things, it is also
designed to be used as a programming *proof assistant*.

That probably didn't explain much, so I'll break down each of those terms below, in no
particular order:

### Purely Functional

Like [Haskell](), Agda doesn't describe what stuff *does*, but instead provides 
definitions of what stuff *is*. The difference is subtle, but very
important. Instead of providing essentially a list of commands for the computer to 
execute, Agda's functions resemble mathematical functions. For instance, they are 
*referentially transparent*. Referential transparency means that for a given input,
a function will only ever produce one output. Addition is a referentially transparent
function (2 + 2 always equals 4), whereas a random number generator is not.

This might seem strange - If Agda *only* allows referentially transparent functions, how
do you do things like generate random numbers? How do you mutate data structures? Doesn't
this rule out a huge class of programs?

The answer is yes. It does rule out many implementations of some algorithms. So, Agda 
*does* include a "way out" for those occasions when you *really really* need to do some
side effects. But, most of the time, you don't. Even when you think you do, you don't.

Haskell includes a similar way out, and its use is frowned upon in the Haskell world. In
Agda, the use of side-effects presents a far greater problem - It impacts
drastically on the properties that you can easily prove about your program. For this 
reason, I won't be talking about side-effects, monads, or other such things until quite
a bit later in this tutorial.

### Dependently Typed

Unlike [Haskell]() or any imperative language, Agda is dependently
typed. The typical definition of dependent types is that "Not only do values depend on
types, but types can also depend on values". This doesn't really give much of an 
explanation, so I'll attempt to provide a better one:

In most statically typed languages, there is a clear distinction between *value level*
and *type level*. Values are things that you actually use in the computation of your
program, like `4` or `true`, whereas types are tags that classify these values, like
`Int` or `Bool` or `AbstractFactoryFactory`. 

If we were to take a *generic* (or *parametrically polymorphic*) type, such as Haskell's
`[a]` or Java's `ArrayList<A>`, the only parameter you can provide to the list type
constructor is another type, for example `Float` could be provided 
for `ArrayList<Float>` or `[Float]` and so on. We can't pass values as type parameters -
`ArrayList<12>` makes no sense.

It would be nice, though, if we could do something like that. Returning to our list 
example, we know that adding an item to a list increases its length by one. We also know
that the initial empty list has a length zero. Therefore, it should be possible for the
compiler to reason (at least partially) about the length of the list in our program. 
Trying to take an item out of an empty list is, however, a *runtime* error. Why? If our
compiler is sufficiently smart, it should be able to notice the error at compile time.

With Agda, we can parameterize our types with values. This allows us to include the length 
of the list in its type, and specify the length-changing properties of the various list 
operations in their type definitions. With this information, Agda can check at *compile*
time whether or not we try and take an item out of an empty list.

Agda goes even further than this and just about eliminates the distinction between types
and values altogether.

~~~~{.haskell}
4 : Int
~~~~

The above line of Agda says that the value `4` is of type `Int`. Not too shabby. But what
about `Int`?

~~~~~{.haskell}
Int : Set
~~~~~~

`Int` is of type `Set`. In Agda, most familiar types such as `Int`, `Bool` and 
so on are of type `Set`. So, types have types. Or, perhaps more accurately, types *are 
values*, of type `Set`. 

`Set` too has a type, written `Set₁`. That in turn has type `Set₂`. This heirarchy of
types never ends. It's turtles (or types) all the way down (or up).

> The more mathematically inclined of you might have noticed that this
> is Agda's way of resolving [Russell's Paradox](), which comes directly from Martin-Löf
> and others' work. 

Don't panic too much if that doesn't make a great deal of sense to you. All will be 
made clear over the next few chapters.

### Proof Assistant

Because it is dependently typed, Agda allows us to encode *any*
property of data in our program in its type. This means that we can statically reason
about any property of our program, enabling us to use Agda's type checker as a proof
checker. What we wish to prove about our program is precisely what we choose to
encode in the type system.

Proving things about your program might sound scary at first, but as you learn Agda
it will come across more as a logical extension of the type system, rather than grade 
school math homework.

In this way Agda is really "living the dream" of the [Curry-Howard Isomorphism](), an
essential realisation that types and intuitionistic logic (and cartesian closed categories)
are the same thing. Programs *are* proofs, and termination is soundness.

### Intuitionistic Logic

Most programmers have been exposed to simple boolean logic. You
might even remember boolean algebra symbols such as `∧` (and), `∨` (or), and `¬`(not).

This boolean algebra can be extended into a system of *propositional logic*, which is 
essentially a system of statements, which can be considered true or false. Some of these
systems include *quantifiers* such as ∀ (for all) and ∃ (there exists). 

Some of these logical systems are called *classical logic* systems. These systems say 
that all statements are either true or false. So, if I made a statement about which
I have no knowledge, for example:

> There exists a teapot between the Earth and Mars that cannot be observed by humans in
> any way.

Classical logic says that this statement is either true, or it is false, even though I
will never have any way to prove it in either direction. This is called the *law of the
excluded middle*, shown below:

   ∀t. t ∨  ¬t

*Intuitionistic logic*, by contrast, rejects this notion. This might seem strange. After
all, if something is true, then it's clearly not false. And if something is false, then
it must not be true. The problem, however, arises when we have something that is neither
false nor true[^1] :

>   Pirates are still the most skilled mathematicians.

Let us assume that pirates have never been the most skilled mathematicians. If our 
statement is true, then we have an outright contradiction. Pirates are not skilled
mathematicians, and this statements says they are. If our statement is false, however,
we *still* have a contradiction, as the statement implies that our Pirates were *once*
skilled at mathematics, even if they are skilled no longer. 

Intuitionistic logic only accepts something as true or false if there is a proof of 
its truthhood or falsehood. More importantly, it makes existence proofs *constructive*,
that is, an object can only be shown to exist by demonstrating an algorithm that
produces it. This has deep implications for the way we write programs in light of
the Curry Howard isomorphism.

If I have an existence assertion such as the teapot example above, I could only prove
that the teapot exists by presenting evidence for it. If I can prove that it *cannot*
be proven true, then that is a proof that the assertion is false.    

Proof by contradiction is, therefore, not allowed in intuitionistic logic, or indeed
in Agda. 

[^1]: Some people use the example of "Are you still beating your wife?" for this 
sort of statement. I don't like that one as I'd rather be cheerful than discuss domestic 
violence in a programming textbook.
















