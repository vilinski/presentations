# gtk-intro

 Feature                | F# | C#
------------------------|----|---------
Type inference          | ✓  | 3.0 (2007) - var, LINQ only
Lambda                  | ✓  | 3.0 (2007)
REPL                    | ✓  | 6.0 (2007)
Scripting               | ✓  | 2017 `csharp-scripting`
Async programming       | 2007 | 6.0 (2012)
Local functions         | ✓  | 7.0 (2017)
Tuples                  | ✓  | 7.0 (2017)
Pattern matching        | ✓  | 7.0 (2017)
Null safety             | ✓  | 8.0
Records                 | ✓  | LINQ, 8.0?
Immutability by default | ✓  | nicht geplant
Typesafe printf         | ✓  | nicht geplant
Algebraic types         | ✓  | nicht geplant
Type providers          | 3.0 (2012)  | nicht geplant
Computation expression  | ✓  | nicht geplant


https://fsharpforfunandprofit.com/fppatterns/

|  OOP                                | FP
|-------------------------------------|---
| Single Responsibility (SRP) | Function
| Open/Closed principle (OCP) | Function
| Liskoff Substitution (LSP)  | also
| Interface Segregation (ISP) | Yes, functions
| Dependency Inversion (DIP)  | Oh my, again!
| Factory | Yepp, also
| Strategy | `List.Sort(x=>x.Name)`|
| Decorator | HttpMessageHandler(LoggingMessageHandler) - aus Chassy
| Visitor   | Rate mal

https://medium.com/@cscalfani/goodbye-object-oriented-programming-a59cda4c0e53

# Goodby OOP

- Inheritance
- Encapsulation
- Polymorphism

Banana-Monkey-Jungle problem

# F# features
- Computation expression (CE) `seq { for i in 1..10 -> i }`, async, query, CIL, expecto https://panesofglass.github.io/computation-expressions/#/10/3
- Algebraic Data Types (ADT) option, result, ?
- Pattern matching
- Better defaults


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
[get-started-cs]:
https://blogs.msdn.microsoft.com/dotnet/2017/07/24/get-started-with-f-as-a-c-developer/
[top-payed]:https://insights.stackoverflow.com/survey/2018#top-paying-technologies
