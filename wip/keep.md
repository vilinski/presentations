# Träume über C# Zukunft

Imaginäre PL, Zukunft der C#
Mehrere Schritte wo man was abkürzen kann

---

# Warum warten?

__DU__ hast es jetzt in deimen Visula studio!

Einfach umbenennen:
- `*.cs` -> `*.fs`
- `*.csproj` -> `*.fsproj`
- `*.csx` -> `*.fsx`

Noch nichts von `*.csx` gehört? Kein Wunder, nicht sehr nützlich

---

# Kurze Syntax ist nicht Grund genug

Was hat C# und F# gleich

---

# Was hat C# und F# gleich

## Lambda

```csharp
    // vorher
    var byAge = personen.Sort(new ComparerPersonByAge());
    // nachher
    var byLastName = personen.OrderBy(x => x.LastName);
```

```fsharp
    let byAge  = personen |> Seq.sortBy(fun p -> p.Age)
    let byName = personen |> Seq.sortBy(fun p -> p.LastName)
```

---

# Was hat C# und F# hat es besser

## Type Inference

```csharp
    Dictionary<string,int> dict1 = new Dictionary<string,int>();
    var dict2 = new Dictionary<string,int>();
    dict2.Add("",1);
```

```fsharp
    let dict1 = Dictionary<_,_>()
    let dict2 = Dictionary() // dict2 : Dictionary<string,int>
    dict.Add("",1)

    let addTwo i = // addTwo : int -> int
        i + 2
    let addTwoAndPrint i = // addTwoAndPrint : int -> unit
        printfn "%i" (addTwo i)
```

---

# Was hat C# und F# hat es besser

## REPL
- C# Interactive
- FSI - F# Interactive

## Scripting

- C# - `*.csx`
  - kein IDE
- F# - `*.fsx`
  - von anfang für Scripts vorbereitet
  - Visual Studio, Visual Studio Code, JetBrains Rider, vim

---

# Was hat C# und F# hat es besser

## Async programming

```fsharp
    let getRecommendations sku =
        async {
            let! ware = store.GetAllAsync sku
            let! similar = ml.CalculateSimilar ware.Features
            let recommendations = similar |> List.take 10
            return recommendations // not your usual 'return' keyword
        }
```

---

# Was hat C# (von F#)

 Feature                | F#   | C#
------------------------|------|---------
Lambda                  | ✓    | 3.0 (2007)
Type inference          | +-   | 3.0 (2007) - var, LINQ only
REPL                    | +-   | 6.0 (2007)
Scripting               | +-   | 2017 `csharp-scripting` - cake, etc.
Async programming       | 2007 | 6.0 (2012)
Local functions         | +-   | 7.0 (2017)
Tuples                  | +-   | 7.0 (2017)
Pattern matching        | +-   | 7.0 (2017)


---

# Was hat F# mehr als C#

 Feature                | F# | C#
------------------------|----|---------
Records                 | ✓  | LINQ, 8.0?
`using` expression      | ✓  | 8.0
Not nullable by default | ✓  | 8.0
Markdown in code docu   | ✓  | nicht geplant
Immutability by default | ✓  | nicht geplant
Typesafe printf         | ✓  | nicht geplant
Algebraic types         | ✓  | nicht geplant
Type providers          | ✓  | nicht geplant
Computation expression  | ✓  | nicht geplant

---

# Markdown in code docu

```fsharp
/// # Args
/// **a** is an argument!
///
/// # Example
/// ```fsharp
/// f 1 //returns 1
/// Seq.map f //identity mapping
/// let rec a = f a //error!
/// ```
/// ![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 1")
let f a =
    a
```

![Markdown in code](images/md-comment.jpg)

# Syntax, Sprachfeatures... YOLO!

Neuen Syntax lernen
Neue Sprachfeatures lernen

## Ich habe keine zwei Leben. Was gibt es noch?

---

# Vorurteile

Was denkt man (zu) oft über FP

- FP ist für spezielle Nichen wie
    - Finanzen und Buchhaltung
    - Mathematische Berechnungen
    - AI
- FP ist akademisch, nichts für real world
  oder wie kann man damit programmieren:
    - Web Backend
    - Web Frontend
    - Mobile
    - Desktop apps
    - Tests
    - CI/Build scripte

---

# Vorurteil 1. F# ist nichts für Backend

<table><tr><td>

## ASP.NET Core

```fsharp
/// **Movies** Controller, `/movies` route
[<Route("movies")>]
type MoviesController() =
    inherit Controller()

    /// load movie list
    [<HttpGet>]
    member __.Get() =
        base.Ok MoviesMgr.getAll

    /// post a new movie
    [<HttpPost>]
    member __.Post(movie : Movie) =
        MoviesMgr.create movie
        base.Created (newUrl, newMovie)

    /// load popular movies
    [<HttpGen("popular")>]
    member __.PopularMovies() =
        base.Ok MoviesMgr.popularMovies
```

</td></tr><tr><td>

## Giraffe

```fsharp
let movies = // movies : HttpHandler
    choose [
      route "/movies" >=> choose [
          Log
          GET >=> MoviesMgr.getAll
          basicAuth
          POST >=> MoviesMgr.create
        ]
      route "/movies/popular"
        >=> GET >=> MoviesMgr.popularMovies
      NOT_FOUND
    ]
```

</td></tr></table>

---

# Vorurteil 2. F# ist nichts für Browsers.

<table><tr><td>

## SERVER

```fsharp
let movie (id : int) : HttpHandler =
    fun next ctx ->
        // Load, filter, serialize
    json movie
````

</td></tr><tr><td>

## BROWSER

```fsharp
let getMovie (id : int) : Promise<ClientMovie> =
    // Send request, handle response, deserialize
    return movie
```

</td></tr><tr>

## SHARED TYPE

```fsharp
type ClientMovie = {
    Id : int
    Name : string
    Year : int
    Rating : float
}
```

</tr></table>

---

# Ernsthaft. Was gibt es noch?

Was bringt mir das Lernaufwand

- Einfacher zu lernen als man denkt.
- Schwieriger schlechten code zu schreiben.
- Besseren Verständnis von OOP
- Leben danach viel einfacher.
- [Besser bezahlt!](top-payed)
- ~~Haare geschmeidiger~~

---

# Vorteile

- Stärkere Typsystem
- Bessere tools
- Testbarkeit
- Leichtere Refactoring
- Andere Bibliotheken
- Community

---

# SOLID Principles

|  OOP                        | FP
|-----------------------------|---
| Single Responsibility (SRP) | Function
| Open/Closed principle (OCP) | Function
| Liskoff Substitution (LSP)  | Ebenso
| Interface Segregation (ISP) | Ja, functions
| Dependency Inversion (DIP)  | Oh my, again!

---

# OOP Design Patterns

|  OOP      | FP
|-----------|---------------------------
| Builder   | Build()
| Strategy  | `List.Sort(x=>x.Name)`|
| Decorator | HttpMessageHandler(LoggingMessageHandler) - aus Chassy
| Visitor   | Visit()

https://fsharpforfunandprofit.com/fppatterns/

https://medium.com/@cscalfani/goodbye-object-oriented-programming-a59cda4c0e53

---

# OOP Claims

- Inheritance
- Encapsulation
- Polymorphism

Banana-Monkey-Jungle problem
![Banana-Gorilla-Jungle-Problem](images/banana-jungle-problem.jpg)
---

# OOP Reality

![OOP Realisty](images/oop-reality.jpg)
# F# features
- Computation expression (CE) `seq { for i in 1..10 -> i }`, async, query, CIL, expecto https://panesofglass.github.io/computation-expressions/#/10/3
- Algebraic Data Types (ADT) option, result, ?
- Pattern matching
- Better defaults

# SRTP aka Statically resolved Type Parameters

    open System

    type Circle = Circle of radius:double
    type Rectangle = Rectangle of width:double * length:double

    type CircleShape() =
        member this.Area(Circle(radius)) = Math.PI * Math.Pow(radius, 2.)
    let circleShape = CircleShape()

    type RectangleShape() =
        member this.Area(Rectangle(width, length)) = width * length
    let rectangleShape = RectangleShape()

    let inline areaOf shapeImpl shape =
        ( ^T : (member Area : 'A -> double) (shapeImpl, shape))

---

# Records

```fsharp
type Person =
    { FirstName : string
      LastName  : string
      Age       : int
    }
let Bonjovi =
    { FirstName = "John"
      LastName  = "Bon Jovi"
      Age       = 57
    }
```

---

# Records (anonym)

```fsharp
    let xa   = {| A = 1 |}
    let xafu = {| xa with F = 1; U = "wow" |}
    let xaf  = {| x without U |}
    let sänger = {| BonJovi with OnTour = false|}
    let x = {| F = 1 |}
    let y = {| U = 1 |}
    let xy = {| include x; include y |}
```

# Quotes

>"Complexity is anything that makes software hard to understand or to modify." — John Outerhout

# Links
- [10 reasons not to use FPL][10-reasons]
- [Get started F# as C# dev][get-started-cs]
- [Top paying Technologies][top-payed]
- http://armlinux.ro/FablePresentation2018/#/
- https://www.slideshare.net/AntyaDev/adopting-f-at-sbtech


# Images
- [Всё еще пишешь на C#?]
https://twitter.com/gsomix/status/940159910070947841
- Elmish vs React:
  - https://twitter.com/mikebild/status/888042176738971649/photo/1
  - https://raw.githubusercontent.com/uanders/react-redux-cheatsheet/master/1440/react-redux-workflow-graphical-cheat-sheet_v110.png



[10-reasons]: https://fsharpforfunandprofit.com/posts/ten-reasons-not-to-use-a-functional-programming-language/
[why-learn]: https://dusted.codes/why-you-should-learn-fsharp#solid-made-easy-in-fsharp
[unlearn-oop]: https://dpc.pw/the-faster-you-unlearn-oop-the-better-for-you-and-your-software
[get-started-cs]: https://blogs.msdn.microsoft.com/dotnet/2017/07/24/get-started-with-f-as-a-c-developer/
[top-payed]: https://insights.stackoverflow.com/survey/2018#top-paying-technologies
[top-payed2]: https://insights.stackoverflow.com/survey/2018/#technology-what-languages-are-associated-with-the-highest-salaries-worldwide
[cheat-sheet]: (http://dungpa.github.io/fsharp-cheatsheet/)
