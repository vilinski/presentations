- title : FsCheck
- description : Introduction to FsCheck
- author : Andreas Vilinski
- theme : night
- transition : none

***

# Tests Tests Tests

***

## Test Coverage

![test coverage](./images/test-coverage.png)

' Coverage badge bei uns in jedem Projekt
' Einerseits - Lakmuspapier, andererseits nutzlos
' TestCoverage testet nur die Fleiß der Tester, nicht die Korrektheit

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
        [InlineData(0.0, 0.0)]
        [InlineData(30.0, 0,5)]
        [InlineData(45.0, 0,70711)]
        [InlineData(60.0, 0,86603)]
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
- Immer noch nicht alle Werte Abgedeckt
- Erinnerung: wir haben 100% test coverage

***

## Property-based tests

- Andere Name: Fuzzy-Tests,
- Original entstanden in Haskell's [QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck2)
- Portiert in viele Programmiersprachen
- Auch .NET F# [FsCheck](https://fscheck.github.io/FsCheck/)
- In C# auch nützlich

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

Ist die liste sortiert?

    [lang=cs]
    var list = List<string> {
        "Alles", "kann", "man", "nicht", "testen"
    };

    public bool IsTested(List<string> list) {
        for(var i; i++; i < list.Count - 1)
            Assert.LessThan(list[i], list[i+1]);
    }

---

### Property

    [Fact]
    //[InlineData(list,orderedList)]
    public void MySortTest() {
        var list = List<string> {
            "Alles", "kann", "man", "nicht", "testen"
        };
        Assert.Equal(list.OrderBy(x => x), MySort(list));
    }

---

### Property

Zwei Wege zum Resultat vergleichen

    [Fact]
    public void MySortTest() {
        Prop.ForAll<List<string>>(list => {
            var expected = list.OrderBy(x => x).ToList();
            var actual = MySort(list);
            Assert.True(expected.SequenceEquals(actual));
        }).QuickCheck("MySort");
    }

---

### Property

Oder eine Schleife bauen (serializer, converter, o.Ä.)

    [Fact]
    public void JsonSerializerTest() {
        Prop.ForAll<AlarmViewModel>(vm => {
            vm.ToJson().FromJson<AlarmViewModel>() == vm;
            // in C# hier asserts pro Property nötig
        }).QuickCheck("MySort");
    }

---

### Property


    [Fact]
    public void TicketPostTest() {
        Prop.ForAll<TicketViewModel>(ticket => {
            var controller = ...
            Assert.DoesNotThrow(() => controller.Post(ticket));
        }).QuickCheck("MySort");
    }

---

### FsCheck.XUnit

    [Property]
    public void TicketPostTest(TicketViewModel ticket) {
        var controller = ...
        Assert.DoesNotThrow(() => controller.Post(ticket));
    }

---

### FsCheck.XUnit

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

### FsCheck

[Dokumentation](https://fscheck.github.io/FsCheck/)
[Code Examples in C# und F#](https://github.com/fscheck/FsCheck/tree/master/examples)
