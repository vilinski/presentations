- title : FsCheck
- description : Introduction to FsCheck
- author : Andreas Vilinski
- theme : night
- transition : none

***

# Tests Tests Tests

***

## Test Coverage

![coverage badge](./images/coverage-badge.png)

' Coverage badge bei uns in jedem Projekt
' Einerseits - Lakmuspapier, andererseits nutzlos
' TestCoverage testet nur die Fleiß der Tester, nicht die Korrektheit

---

## Test Coverage

![coverage badge](./images/dotcover-code.png)

---

### Test Coverage ~0 %

    [lang=cs]
    public class Useless
    {
        public static List<(char, int)> Get5TopFrequentChars(
            string text)
        {
            return string.IsNullOrEmpty(text)
                ? new List<(char, int)>()
                : text.GroupBy(x => x)
                    .Select(g => (g.Key, g.Count()))
                    .OrderByDescending(x => x.Item2)
                    .Take(5)
                    .ToList();
        }
    }
    public class UselessTests {
        [Fact]
        public void Coverage0() {
            Assert.Empty(Useless.GetCharsFrequency(null));
        }
    }

' hier ist coverage nah zum null
' deklarativen code aus Bibliothekfunktionen zu testen? Würde ich nicht

---

### Test Coverage 100 %

    [lang=cs]
    public class Useless
    {
        public static double Sin(double i)
        {
            return i;
        }
    }

    public class UselessTests
    {
        [Fact]
        public void Coverage100()
        {
            Assert.Equal(0, Useless.Sin(0));
        }
    }

' entgegenrichtung 100% coverage, method ist fehlerhaft
' braucht mehr als einen Test-Input

---

### More tests

    [lang=cs]
    public class UselessTests
    {
        [Fact]
        public void Sin0Test()
        {
            Assert.Equal(0.0, Useless.Sin(0.0));
        }
        public void Sin30Test()
        {
            Assert.Equal(30.0, Useless.Sin(0.5));
        }
        public void Sin90Test()
        {
            Assert.Equal(90.0, Useless.Sin(1.0));
        }
    }

' eine Variante für jede Testeingabe ein Method
' das hier sind Einzeiler, aber kann complexer werden
' wird durch copy/paste gelöst, was die tests brüchig macht

***

## Data-driven Tests or Raw tests

    [lang=cs]
    public class UselessTests
    {
        [Fact]
        [InlineData( 0.0, 0.0)]
        [InlineData(30.0, 0.5)]
        [InlineData(45.0, 0.70711)]
        [InlineData(60.0, 0.86603)]
        [InlineData(90.0, 1.0)]
        public void SinTest(double input, double expected)
        {
            Assert.Equal(expected, Useless.Sin(input));
        }
    }

' Testframeworks anbieten hier die Raw Test, DataSource, oder InlineData
' So können wir weiter treiben. Mühsam, kann immer noch nicht alles abgedeckt sein

---

### Data-driven Tests or Raw tests

Schon besser, aber...

- Erlaubt sind nur primitive Typen (`string`, `int`, `double`, etc.)
- Werte müssen vorberechnet werden
- Immer noch nicht alle Werte Abgedeckt
- Test coverage bleibt groß

***

## Property-based tests

- Auch genannt: Fuzzy-Tests, Ungenaue Tests
- Kein "Neuland"
- In Haskell seit 1990's [QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck2)
- Portiert in viele Programmiersprachen
    - JS [JsVerify](https://github.com/jsverify/jsverify#documentation)
    - .NET (F#, C#) [FsCheck](https://fscheck.github.io/FsCheck/)
- Randomisierte Eingabe (100 mal)

' Portierungen haben in allen bekannten Sprachen
' Mit ähnlichen Namen

---

### Property-based tests

Hier sind C# Properties

<section>
<p class="fragment roll-in">

    [lang=cs]
    public class MyProperties {
        public Guid Id { get; set; }
        public string StringProperty { get; set; }
        public int AnotherProperty { get; set; }
        public int AnotherProperty { get; set; }
    }

</p><p class="fragment roll-in">

Nur diese haben damit nichts zu tun 😀

</p></section>

---

### Property

"Ist die liste sortiert?"

    [lang=cs]
    var list = List<string> {
        "Alles", "kann", "man", "nicht", "testen"
    };

    public bool IsOrdered(List<string> list) {
        for(var i; i++; i < list.Count - 1)
            Assert.LessThan(list[i], list[i+1]);
    }

' grob gesagt Property ist nur eine funktion
' bool Rückgabe bedeutet ob es erfüllt ist

---

### Property

    [lang=cs]
    [Fact]
    //[InlineData(list,orderedList)]
    public void MySortTest() {
        var list = List<string> {
            "Alles", "kann", "man", "nicht", "testen"
        };
        Assert.Equal(list.OrderBy(x => x), MySort(list));
    }

' jetzt implementiere ich eine Sortierfuntkon und möchte die testen
' InlineData funktioniert mit Listen nicht

---

### Property

    [lang=cs]
    using FsCheck;

    [Fact]
    public void MySortTest() {
        Prop.ForAll<List<string>>(list => {
            var expected = list.OrderBy(x => x).ToList();
            var actual = MySort(list);
            // return (expected == actual);
            // return IsOrdered(actual);
            Assert.True(expected.SequenceEquals(actual));
        }).QuickCheck("MySort");
    }

<section><p class="fragment roll-in">

Zwei Wege zum Ziel + Resultat vergleichen

</p></section>

---

### Property

    [lang=cs]
    [Fact]
    public void JsonSerializerTest() {
        Prop.ForAll<AlarmViewModel>(vm =>
            vm.ToJson().FromJson<AlarmViewModel>() == vm
            // in C# hier asserts pro Property nötig
            // oder vm.ToJson() nur nicht in diesem Fall
        ).QuickCheck("MySort");
    }

Oder eine Schleife bauen (serializer, converter, o.Ä.)

' um zwei instanzen einer klasse zu vergleichen kann json verwendet werden

---

### Property

    [lang=cs]
    [Fact]
    public void TicketPostTest() {
        Prop.ForAll<TicketViewModel>(ticket => {
            var controller = ...
            Assert.DoesNotThrow(() => controller.Post(ticket));
        }).QuickCheck("MySort");
    }

---

### FsCheck.XUnit

    [lang=cs]
    [Property]
    public void TicketPostTest(TicketViewModel ticket) {
        var controller = ...
        Assert.DoesNotThrow(() => controller.Post(ticket));
    }

' es gibt unterstützung für Testframeworke, XUnit auch
' PropertyAttribute

---

### FsCheck.XUnit

    [lang=cs]
    [Property]
    public bool Increment_Twice_Is_The_Same_As_Adding_Two(int x)
    {
        return
            (Add(1, Add(1, x)) == Add(x, 2))
            .Classify(x > 10, "Bigger than '10'")
            .Classify(x < 1000, "Smaller than '1000'");
    }

<pre>
    Ok, passed 100 tests.
    63% Smaller than ‚1000‘.
    37% Smaller than ‚1000‘, Bigger than ’10‘.
</pre>

---

### FsCheck.XUnit output

    [lang=cs]
    public class Test
    {
        private readonly ITestOutputHelper _TestOutputHelper;
        public Test(ITestOutputHelper testOutputHelper)
        {
            _TestOutputHelper = testOutputHelper;
        }

        [Fact]
        public void Test1()
        {
            Prop
                .ForAll(...)
                .VerboseCheckThrowOnFailure(_TestOutputHelper);
        }
    }

---

### FsCheck generator

    [lang=cs]
    [Property]
    public bool SomeProviderTest1()
    {
        var str50 = Arb
            .Generate<string>()
            .Where(s => s != null && s.Length <= 50);
        Prop.ForAll(str50, description =>
        {
            var isSet = alarmProvider.SetDescription(alarmId, description);
            Assert.True(isSet);
        }).QuickCheckThrowOnFailure();
    }

---

### FsCheck generator

    [lang=cs]
    var arbAddress =
        from city in Arb.Generate<string>()
                        .Where(s.Length <= 50)
        from street in Arb.Generate<string>()
                          .Where(s.Length <= 50)
        select (city, street);

    Prop.ForAll(arbAddress, (city, street) =>
    {
        var isUpdated = updater.SetAddress(city, street);
        Assert.True(isUpdated);
    }).QuickCheckThrowOnFailure();

---

### FsCheck generator

    [lang=cs]
    public class TicketBodyGenerator
    {
        public static Arbitrary<ITicketBody> TicketBody()
        {
            var genMaintenance = Arb.Generate<Maintenance>().Select(x => (ITicketBody) x);
            var genLimitViolation = Arb.Generate<LimitViolation>().Select(x => (ITicketBody) x);
            var genTimeout = Arb.Generate<Timeout>().Select(x => (ITicketBody) x);
            var gen = Gen.OneOf(genMaintenance, genLimitViolation, genTimeout);
            return Arb.From(gen);
        }
    }

    ...

    Arb.Register<TicketBodyGenerator>();
    Prop.ForAll((Ticket t) => {
        ...
        });

---

### FsCheck Links

- [Dokumentation](https://fscheck.github.io/FsCheck/)
- [Code Beispiele in C# und F#](https://github.com/fscheck/FsCheck/tree/master/examples)
- [FsReveal](http://fsprojects.github.io/FsReveal/)
    - [Reveal.js](https://revealjs.com/#/)
    - [Markdown](https://de.wikipedia.org/wiki/Markdown)
    - [F#](https://fsharpforfunandprofit.com)

***

# Questions?

<table>
<tr><td>

![Donald Knut](./images/donald.knuth.jpg)

</td><td>

> Beware of bugs in the above code; I have only proved it correct, not tried it.

Donald Knuth

</td></tr>
</table>

