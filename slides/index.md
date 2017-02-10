- title : Scala and OOP vs FP
- description : Introduction to Scala and functional programming
- author : Andreas Vilinski
- theme : solarized
- transition : default

***

# Scala vs Java and <BR7> What the ... FP ?

***

# Scala vs Java

- statically typed
- strong(er) type inference
- Functional + Object oriented
- everything is an object, even functions
- every function is a value - including methods


***

# Scala vs Java
Java

    [lang=java]
    Pair p = new Pair<Integer, String>(1, "Scala");

Scala

    [lang=scala]
    val p1 = new MyPair(1, "Scala")
    val p2 = (1, "Scala")
    val (nr, name) = p2


***

# What the ... [FP]() ?

- **OOP** -- objects everywhere
- **FP** - functions everywhere
- **OOP** Design patterns
- **Functional** design...

    <section>
    <p class="fragment" style="text-decoration: line-through">Patterns</p>
    <p class="fragment" style="text-decoration: line-through">Tips</p>
    <p class="fragment" style="text-decoration: line-through">Approaches</p>
    </section>

***

### OOP Design Patterns

<table style="textAlign: left">
<tr><td>

- Factory
- Strategy
- Decorator
- Visitor

</td><td>

- ```def factory()```
- `def sort(f)`
- `def decorate(x)`
- `def rec visit(x)`

</td></tr>
</table>

***

### SOLID principes

<table style="textAlign: top">
<tr><td>

- [**S**]()ingle Responsibiity (SRP)
- [**O**]()pen/Closed (OCP)
- [**L**]()iskov Substitution (LSP)
- [**I**]()nterface segregation (ISP)
- [**D**]()ependency inversion (DIP)


</td><td>

- **E**ach one is just a...
- Function
- Function
- Function too
- [FP patterns are different]()

</td></tr>
</table>

***

## And now the Functional patterns

- Apomorphism, Isomorphism, Sigomorphism
- Functor, Bifunctor
- Monoid, Monade, Comonade

***

### Was a joke, relax ;)

- Core principles:
    - functions are things
    - composition everywhere
    - types are not classes
- Functions as parameters
    - interface -> function
    - dependency injection -> partial application
    - chaining, continuation

***

### Function is a thing

- not attached to a class
- always **one** input, always **one** output
    - `apple => banana`
    - `(apple,banana) => ftuitSalad`
    - `apple => banana => fruitSalad`

***

### Function is a thing

    [lang=scala]
    def add(x:Int)(y:Int) : Int = x + y // Int => Int => Int
    def add5 = add(5)                   // Int => Int
    def add5mul2(f : Int => Int, x : Int) = f(x) * 2

***

### Composition everywhere

    [lang=scala]
    (apple => banana) andThen (banana => cherry)
    (apple => cherry) // new function

***

#### Function composition

OP

- functions in the small
- objects in the large

FP

- functions in the small
- functions in the large

***

#### Function composition

    [lang=scala]
    // low level operation (micro service)
    String --> toUpper --> String

    // service
    --> lowLevel --> lowLevel --> lowLevel -->
    MdmProduct --> FeatureCleanup --> EncodexProduct
    // Matrjoschka

service is just like a microservice but without "micro"

***

### Types are not classes

- Data (List)
    - set of valid inputs
    - set of valid outputs
- Behavior (map, collect, filter, etc...)
    - not methods - functions
- Composition
    - product types - case class
    - algebraic data types (ADT)

***

### ADT - algebraic data types

- Simple representation for complex cases
- Make invalid state impossible
- Each case is independent in scala
- Pattern matching ensures all cases handled
    - (actually in scala not always)
    - use sealed trait + final case class

***

### ADT - algebraic data types

scala

    [lang=scala]
    type FeatureValueId = Int
    sealed trait FeatureValue
    object FeatureValue {
        final case class DiscreteFeatureValue(value: FeatureValueId) extends FeatureValue
        final case class NumericFeatureValue(value: Double) extends FeatureValue
        final case class LiteralFeatureValue(value: String) extends FeatureValue
        final case class DateFeatureValue(value: LocalDate) extends FeatureValue
    }

the same in F#

    type FeatureValueId = int
    type FeatureValue =
        | Discrete of FeatureValueId
        | Numeric of decimal
        | Literal of string
        | Date of System.DateTime

***

### Scala beginner Tipps

Things to avoid if not necessary. What to use instead

- `var` ==> `val`
    - prefer immutable
    - but mutable if performance critical
- `def` without parameters is also a function
- `foreach` ==> `map`, `flatMap`, `filter`, `fold`
    - scala's `for` is just syntactic shugar for `flatMap`
- Inheritance
    - ADTs
    - Composition over inheritance
    - but CakePattern wit `trait`s - popular in scala

***

### Scala beginner Tipps

`Option`: `Some(value)` or `None`

- Option is new `null`
- gives a meaning to `null`, not eliminates it
- **ADT** - must be handled on the place
- can cause NRE if comes from java
    - `new Option(javaObject)`
- slightl performance drawback - heap instead of stack
- not overuse it, not propagate if not necessary
    - `flatMap` it away early or `getOrElse`
    - don't use `get` - runtime exception!

***

### Scala beginner Tipps

    [lang=scala]
    val name: Option[String] = request getParameter "name"
    val upper = name map { _.trim } filter { _.length != 0 } map { _.toUpperCase }
    println(upper getOrElse "")

the same with `for`:

    [lang=scala]
    val upper = for {
        name <- request getParameter "name"
        trimmed <- Some(name.trim)
        upper <- Some(trimmed.toUpperCase) if trimmed.length != 0
    } yield upper
    println(upper getOrElse "")

***

### Scala beginner Tipps

`import scala.util.{Try, Success, Failure}`

- Avoid throwing exceptions
- `Try` to catch them
- Better alternative to exceptions
- is also an **ADT**
- [ROP - Railway oriented programming](http://www.slideshare.net/ScottWlaschin/railway-oriented-programming)
- Happy way programming with flatmap

***

### Scala beginner Tipps

    [lang=scala]
    def getURLContent(url: String): Try[Iterator[String]] =
    for {
        url <- parseURL(url)
        connection <- Try(url.openConnection())
        is <- Try(connection.getInputStream)
        source = Source.fromInputStream(is)
    } yield source.getLines()

***

### IntelliJ Tipps

- Ctrl+Shfit+A - search for commands
- Ctl+Q - Quick documentation. Shows which type infers scala if not explicit
- Ctl+Shift+P - Implicit parameters
- Scala Worksheet - REPL with interactive mode

***

(45)
weiter mit Kraken?


*code from [NashFP/rosalind](https://github.com/NashFP/rosalind/blob/master/mark_wutka%2Bhaskell/FIB/fib_ziplist.hs)*


**Bayes' Rule in LaTeX**

$ \Pr(A|B)=\frac{\Pr(B|A)\Pr(A)}{\Pr(B|A)\Pr(A)+\Pr(B|\neg A)\Pr(\neg A)} $

***

### The Reality of a Developer's Life

**When I show my boss that I've fixed a bug:**

![When I show my boss that I've fixed a bug](http://www.topito.com/wp-content/uploads/2013/01/code-07.gif)

**When your regular expression returns what you expect:**

![When your regular expression returns what you expect](http://www.topito.com/wp-content/uploads/2013/01/code-03.gif)

*from [The Reality of a Developer's Life - in GIFs, Of Course](http://server.dzone.com/articles/reality-developers-life-gifs)*

### Links

- http://www.slideshare.net/ScottWlaschin/fp-patterns-ndc-london2014
- https://fsharpforfunandprofit.com/fppatterns/
