- title : Scala and OOP vs FP
- description : Introduction to Scala and functional programming
- author : Andreas Vilinski
- theme : solarized
- transition : default
- links :
    http://www.slideshare.net/ScottWlaschin/fp-patterns-ndc-london2014
    https://fsharpforfunandprofit.com/fppatterns/

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

### ADT

scala

    [lang=scala]
    trait PaymentMethod
    case object Cash extends PaymentMethod
    case class Cheque(nr: ChequeNumber) extends PaymentMethod
    case class CreditCard(
            cardType: CardType,
            nr: CardNumber) extends PaymentMethod

compare with F#

    [lang=fsharp]
    type PaymentMethod =
        | Cash
        | Cheque of ChequeNumber
        | Card of CardType * CardNumber

***

(45)

#### F# (with tooltips)

    let a = 5
    let factorial x = [1..x] |> List.reduce (*)
    let c = factorial a

---

#### C#

    [lang=cs]
    using System;

    class Program
    {
        static void Main()
        {
            Console.WriteLine("Hello, world!");
        }
    }

---

#### JavaScript

    [lang=js]
    function copyWithEvaluation(iElem, elem) {
        return function (obj) {
            var newObj = {};
            for (var p in obj) {
                var v = obj[p];
                if (typeof v === "function") {
                    v = v(iElem, elem);
                }
                newObj[p] = v;
            }
            if (!newObj.exactTiming) {
                newObj.delay += exports._libraryDelay;
            }
            return newObj;
        };
    }


---

#### Haskell

    [lang=haskell]
    recur_count k = 1 : 1 :
        zipWith recurAdd (recur_count k) (tail (recur_count k))
            where recurAdd x y = k * x + y

    main = do
      argv <- getArgs
      inputFile <- openFile (head argv) ReadMode
      line <- hGetLine inputFile
      let [n,k] = map read (words line)
      printf "%d\n" ((recur_count k) !! (n-1))

*code from [NashFP/rosalind](https://github.com/NashFP/rosalind/blob/master/mark_wutka%2Bhaskell/FIB/fib_ziplist.hs)*

---

### SQL

    [lang=sql]
    select *
    from
    (select 1 as Id union all select 2 union all select 3) as X
    where Id in (@Ids1, @Ids2, @Ids3)

*sql from [Dapper](https://code.google.com/p/dapper-dot-net/)*

---

### Paket

    [lang=paket]
    source https://nuget.org/api/v2

    nuget Castle.Windsor-log4net >= 3.2
    nuget NUnit

    github forki/FsUnit FsUnit.fs

---

### C/AL

    [lang=cal]
    PROCEDURE FizzBuzz(n : Integer) r_Text : Text[1024];
    VAR
      l_Text : Text[1024];
    BEGIN
      r_Text := '';
      l_Text := FORMAT(n);

      IF (n MOD 3 = 0) OR (STRPOS(l_Text,'3') > 0) THEN
        r_Text := 'Fizz';
      IF (n MOD 5 = 0) OR (STRPOS(l_Text,'5') > 0) THEN
        r_Text := r_Text + 'Buzz';
      IF r_Text = '' THEN
        r_Text := l_Text;
    END;

***

**Bayes' Rule in LaTeX**

$ \Pr(A|B)=\frac{\Pr(B|A)\Pr(A)}{\Pr(B|A)\Pr(A)+\Pr(B|\neg A)\Pr(\neg A)} $

***

### The Reality of a Developer's Life

**When I show my boss that I've fixed a bug:**

![When I show my boss that I've fixed a bug](http://www.topito.com/wp-content/uploads/2013/01/code-07.gif)

**When your regular expression returns what you expect:**

![When your regular expression returns what you expect](http://www.topito.com/wp-content/uploads/2013/01/code-03.gif)

*from [The Reality of a Developer's Life - in GIFs, Of Course](http://server.dzone.com/articles/reality-developers-life-gifs)*

