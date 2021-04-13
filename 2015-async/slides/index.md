---
title : Async
description : Introduction to async programming
author : Andreas Vilinski
theme : night
transition : default
verticalSeparator: ^***$
---

## Asynchronous Programming
#### Tips and Tricks

---


### Async programming

- WWW - world wide waiting
- APM - IAsyncResult
- TPL - Task Parallell Library
- async/await
- Rx
- Message queues
- Actor - Erlang/Akka/Akka.NET/...

<aside class="notes">
First short about different approaches, mostly in C# just because
</aside>

---

### Traditional synchronous approach

```csharp
    public class MyReader
    {
        var fs = File.OpenRead(@"c:\somefile.txt");
        byte[] buffer = new byte[fs.Length];
        public int Read(byte [] buffer, int offset, int count);
    }
```

wasting user time {.fragment .fade-in}

--

### APM

```csharp
static void Main(string[] args) {
    byte[] readBuffer;
    var fs = File.OpenRead(@"c:\somefile.txt");
    readBuffer = new byte[fs.Length];
    var result = fs.BeginRead(readBuffer, 0, (int)fs.Length,
      OnReadComplete, fs);
    //do other work here while file is read...
    Console.ReadLine();
}
private static void OnReadComplete(IAsyncResult result) {
    var stream = (FileStream)result.AsyncState;
    var bytesRead = stream.EndRead(result);
    Console.WriteLine("Read {0} bytes successfully.", bytesRead);
    stream.Dispose();
}
```

- callback hell{.fragment .fade-in}
- two methods: `BeginRead`, `EndRead`

---

### APM

- Asynchronous Programming Model
- Separate XYBegin und XYComplete methods - readability
- No way to cancel
- No error handling
  + No [using](https://msdn.microsoft.com/en-us/library/yh598w02.aspx) statement for resources possible - create and dispose in different methods
- No synchronization between callback and caller thread
- No coordination for multiple async operations

---

### APM

APM with lambda indstead of separate method

    [lang=csharp]
    static void Main(string[] args)
    {
      byte[] readBuffer;
      var fs = File.OpenRead(@"c:\somefile.txt");
      readBuffer = new byte[fs.Length];
      var result = fs.BeginRead(readBuffer, 0, (int)fs.Length,
          asyncResult =>
          {
              var bytesRead = fs.EndRead(asyncResult);
              Console.WriteLine("Read {0} bytes successfully.", bytesRead);
              fs.Dispose();
          }, null);

      //do other work here while file is read...

      Console.ReadLine();
    }

' instead of second method you can write a lambda. Other disadvantages are still here

***

### TAP

- Task-based Asynchronous Programming
- [System.Threading.Tasks.Task<TResult>](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task(v=vs.110).aspx)
- Promice or Future in other languages

---

### TAP - Task-based Async Programming

    [lang=csharp]
    private static Task<int> ReadFileAsync(string filePath)
    {
        var fs = File.OpenRead(filePath);
        var readBuffer = new byte[fs.Length];
        var readTask = fs.ReadAsync(readBuffer, 0, (int)fs.Length);
        readTask.ContinueWith(task =>
        {
            if (task.Status == TaskStatus.RanToCompletion)
                Console.WriteLine("Read {0} bytes from file {1}",
                  task.Result, filePath);
            else
                Console.WriteLine("Error with file {0}.", filePath);
            fs.Dispose();
        });
        return readTask;
    }

' Now you can chain computations, cancel them
' Continuation can be fired on special computation results like success or error
' Code clutter with lambdas or separate methods for each continuation

---

### TAP - Task-based Async Programming

    [lang=csharp]
    static void Main(string[] args)
    {
        var read1 = ReadFileAsync(@"c:\somefile.txt");
        var read2 = ReadFileAsync(@"c:\someotherfile.txt");
        Task.WhenAll(read1, read2)
            .ContinueWith(task => Console.WriteLine("All done."));
        //do other work here while files are read...
    }

' Can handle multiple computations a once

---

### TAP - Promise in JavaScript

    [lang=js]
    asyncThing1().then(function() {
      return asyncThing2();
    }).then(function() {
      return asyncThing3();
    }).catch(function(err) {
      return asyncRecovery1();
    }).then(function() {
      return asyncThing4();
    }, function(err) {
      return asyncRecovery2();
    }).catch(function(err) {
      console.log("Don't worry about it");
    }).then(function() {
      console.log("All done!");
    });

---

### TAP - Future in scala

    [lang=scala]
    var rateQuote = Future {
      exchange.getCurrentValue(USD)
    }

    val purchase = rateQuote map { quote =>
      if (isProfitable(quote)) exchange.buy(amount, quote)
      else throw new Exception("not profitable")
    }

    purchase onSuccess {
      case _ => println("Purchased " + amount + " USD")
    }

***

### Async/Await - sugar for TAP

    [lang=cs]
    private static async Task<int> ReadFileAsync(string filePath)
    {
        var bytesRead = 0;
        try
        {
            using (var fs = File.OpenRead(filePath))
            {
                var readBuffer = new Byte[fs.Length];
                bytesRead = await fileStream.ReadAsync(readBuffer, 0, (int)fs.Length);
                Console.WriteLine("{0} bytes from file {1}", bytesRead, filePath);
                return bytesRead;
            }
        }
        catch(Exception)
        {
            Console.WriteLine("Error while reading file {0}.", filePath);
            return bytesRead;
        }
    }

---

### whats wrong with asyn/await in c#

- [C# async gotchas](http://tomasp.net/blog/csharp-async-gotchas.aspx/)
- Avoid a void
- Simple deadlocks by newbies
- Issues with nesting
- Not obvious where runs the code - `` Await.Configure() ``
  - Which part runs synchronously

---

### Compare to originals - async in F#

    let urlList = [
        "http://www.microsoft.com"
        "http://www.google.com"
        "http://www.amazon.com" ]

    let fetchHtmlAsync url = async {
        let uri = new System.Uri(url)
        let webClient = new System.Net.WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        return html
    }

    let getHtmlList =
        Seq.map fetchHtmlAsync    // Build an Async<'a> for each site
        >> Async.Parallel         // each Async<'a> across different threads
        >> Async.RunSynchronously // Run each Async<'a>, non-blocking wait

    let htmlList = urlList |> getHtmlList

***

### [Reactive Extensions](http://reactivex.io) - another approach to asynchronity

<table>
<th><td>sync/pull</td><td>async/push</td></th>
<tr><td>one</td><td>

    TResult

</td><td>

    Task<TResult>

</td></tr>
<tr><td>multiple</td><td>

    IEnumerable<TResult>

</td><td>

    IObservable<TResult>

</td></tr>
</table>

- The Observer pattern done right
- Invented in C# by [Erik Meyer](https://twitter.com/headinthebox)
- Choose your plattform:
  + [Rx.NET](https://github.com/Reactive-Extensions/Rx.NET), [RxScala](https://github.com/ReactiveX/RxScala), RxJava, RxJS, RxSwift, ...

---

### Rx - [Your Mouse is a Database](https://queue.acm.org/detail.cfm?id=2169076)

    TextChanges(input)
      .DistinctUntilChanged()
      .Throttle(TimeSpan.FromMilliSeconds(10))
      .Select(word=>Completions(word))
      .Switch()
      .Subscribe(ObserveChanges(output));

---

### Rx - reactive Paint in WPF

    [language=csharp]
    var mouseDown = Observable
      .FromEvent<MouseButtonEventArgs>(image, "MouseDown")
      .Select(evt => evt.EventArgs.GetPosition(image));
    var mouseUp = Observable
      .FromEvent<MouseButtonEventArgs>(image, "MouseUp");
    var mouseMove = Observable
      .FromEvent<MouseEventArgs>(image, "MouseMove")
      .Select(evt => evt.EventArgs.GetPosition(this));
    var q = mouseDown
      .SelectMany(imageOffset => mouseMove.TakeUntil(mouseUp),
        (imageOffset, pos) => new Point
        {
          X = pos.X - imageOffset.X,
          Y = pos.Y - imageOffset.Y
        })
        .Subscribe(Observer.Create<Point>(p =>
        {
          Canvas.SetLeft(image, p.X);
          Canvas.SetTop(image, p.Y);
        }));

***

### Message queues

<table style="width:100%">
<tr>
<td>

Implementations:

- [Kafka](kafka.apache.org)
- ZeroMQ
- [RabbitMQ](https://rabbitmq.com)
- ActiveMQ
- Apache Qpid
- IronMQ

</td>
<td>

Advantages:

+ [Fast](http://www.kuntalganguly.com/2014/08/message-queue-comparision.html) - [some](http://bravenewgeek.com/dissecting-message-queues/) [benchmarks](http://blog.x-aeon.com/2013/04/10/a-quick-message-queue-benchmark-activemq-rabbitmq-hornetq-qpid-apollo/)
+ Asynchronous - Queue it now, run later
+ Clients in any language,
+ AMPQ - common protocol
+ Decoupling - Separates app logic
+ Resilience: Won't crash whole app
+ Redundancy: Retry failed jobs
+ Guarantees: message survives restart
+ Scalable: workers per job
+ Free monitoring tools


</td>
</tr>
</table>

***

### RAM footprint per unit of concurrency (approx)

<table id="concurrency-table">
<tr class="haskell">
    <td class="num">1.3KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>Haskell ThreadId + MVar (GHC 7.6.3, 64-bit)</span></td>
</tr>
<tr class="erlang">
    <td class="num">2.6 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>Erlang process (64-bit)</span></td>
</tr>
<tr class="go">
    <td class="num">4.0 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>Go goroutine</span></td>
	<!-- http://tip.golang.org/doc/go1.3#stack_size -->
</tr>
<tr class="c-min">
    <td class="num">9.0 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>C pthread (minimum, 64-bit Mac OS X)</span></td>
    <!-- http://opensource.apple.com/source/Libc/Libc-594.9.5/include/limits.h -->
</tr>
<tr class="java-min">
    <td class="num">64.0 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>Java thread stack (minimum)</span></td>
    <!-- http://www.oracle.com/technetwork/java/hotspotfaq-138619.html#threads_oom -->
</tr>
<tr class="placeholder"><td colspan="2"><hr/></td></td>
<tr class="c">
    <td class="num">513 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>C pthread (default, 64-bit Mac OS X)</span></td>
    <!-- https://developer.apple.com/library/mac/documentation/cocoa/conceptual/Multithreading/CreatingThreads/CreatingThreads.html#//apple_ref/doc/uid/10000057i-CH15-SW7 -->
</tr>
<tr class="java">
    <td class="num">1024 KB</td>
    <td class="name"><div class="bar-ctr"><div class="bar"></div></div><span>Java thread stack (default)</span></td>
    <!-- http://www.oracle.com/technetwork/java/hotspotfaq-138619.html#threads_oom -->
</tr>
</table>

***

### Akka

- Inspired from [Erlang](http://www.erlang.org) processes
- Originally not type safe. Erlang is dynamic
- Async OOP over messaging - "share nothing"
- In-process usable too - spray
- Failure safe
- Supervision trees
- Back-pressure - Publisher/Subscriber
- Low level - extra programming effort
- Own communication protocol

---

### [spray](spray.io)/akka-http

- Both
  + Scala-Frameworks
  + Actor-Based - Akka
  + Performant enough - at least for us
  + Routing DSL
  + Test-Libraries - akka-testkit, spray-testkit, akka-http-testkit

---

### [spray](spray.io)
  + Production-Ready
  + Well documented
  + spray-testkit
  + No more features - only support
  + No WebSockets - most requested feature ever

---

### akka-http
  + Successor development of Spray
    + [1.0](http://akka.io/news/2015/07/15/akka-streams-1.0-released.html) (Jul 15 2015)
      + Much slower than spray
    + [2.0-M1](http://akka.io/news/2015/11/05/akka-streams-2.0-M1-released.html) - (Nov 05 2015)
      + API unification - routing, marshalling, http
    + 2.0 - soon?
  + Not yet complete docs - fallback to spray docs
  + Additional features:
    - WebSockets
    - Streams

---

### akka-http - WebSockets

- No long polling
- Persistent Client-Server connection
- Full-Duplex
- Much smaller header - efficient communication


    [language=scala]
    path("ws") {
      val handler = Flow.fromSinkAndSource(
          Sink.ignore,
          Source.single(TextMessage("Hello World!"))
        )
      handleWebsocketMessages(handler)
    }

---

### akka-streams

    [lang=scala]
    //types:
    Source[Out,Mat]
      Flow[In, Out, Mat]
      Sink[In, Mat]

    val ready = Source(someSource)
        .via(flow).map(_ * 2)
        .to(sink)

    val mat: Mat = ready.run()

    val f: Future[String] = Source.single(1)
        .map (_ .toString)
        .runWith(Sink.head)

' web request as a stream already in other FP frameworks too - Haskell, F#

---

### akka-streams

    [lang=scala]
    //types:
    Source[Int,Unit]
      Flow[Int, String, Unit]
           Sink[String, Future[String]]
***

### Scala Vor/Nachteile

* **PRO**
  + FP + OOP
  + Reach type system
  + JVM
  + big community -> a lot of libs
    * Hadoop, Spark, etc.
  + Type classes

---

* **CONTRA**
   + Jar/SBT hell
   + Implicits hell - drop IDEA to Sublime
   + Slow compiler, IDEs
   + Not a better Java
      + Wannabe Haskell for JVM, really - C# alike
      + Verbose
      + Complex type system => weaker type inference
      + Too much of OOP - inheritance hell
   + Lessons learnt on [Akka/Scala/Spray](https://github.com/janm399/akka-patterns/wiki/Lessons-Learnt---Fommil)
   + [We're Doing It All Wrong](https://www.youtube.com/watch?v=TS1lpKBMkgg) by Paul Phillips
   + [We need less powerful languages](http://lukeplant.me.uk/blog/posts/less-powerful-languages/) by [Luke Plant](http://lukeplant.me.uk)

---

### Scala - Powerfull type system

    [language=scala]
    // a fictional idealized version of the genuine method
    def map[B](f: (A) => B) : Map[B]

<div class="fragment">

    // the laughably labeled "full" signature
    def map[B, That](f: ((A,B)) => B)
      (implicit bf: CanBuildFrom[Map[A,B], B, That]) : That

</div>
<div class="fragment">has FOUR distinct identifiers</div>
<div class="fragment">neither has any basis in reality!</div>

---

### Scala - predictable

- [A scala corrections library](http://de.slideshare.net/extempore/a-scala-corrections-library) by Paul Phillips
- One of them returns 2, other returns never: Feeling lucky?


    [language=scala]
    (Stream from 1) zip (Stream from 1)
      map { case (x,y) => x + y } head
    (Stream from 1, Stream from 1).zipped
      map { case (x,y) => x + y } head

***

### F# compared to Scala

- Simple, conciser syntax - less ``{}();`` more ``|> >> >>= >=>``
- Async and Actor included
- Powerful Hindley-Milner type inference
- [No circular dependencies](http://fsharpforfunandprofit.com/posts/cycles-and-modularity-in-the-wild/)
- No implicit conversions
- More FP than OOP
- Good, fast, free IDEs with intellisense
  + VS Community Edition, VS-Code, Atom, Emacs, Vim, ...
- Full stack libraries
  + [Web](https://panesofglass.github.io/TodoBackendFSharp/#/), [Cloud](http://mbrace.io), [GPU](http://www.aleagpu.com), Desktop, Mobile, ...
- Scripting and REPL

' circular dependencies

***

### CLR - better JVM
It's a moot point, but:

- Real generics
- Real tile recursion optimization
- Value types
- Easier native calls - c/c++
- Runs on linux too +-

***

### F#-only features

- Computation expressions:
  * ***async***, query, seq, cloud, log, mongo, ...
- Type providers - for any structured data
  * sql, csv, xml, json, wmi, odata, wsdl, hadoop/hive, typescript, excel, swagger, registry, chess...
- Measure-types - currency, physical units
- Discriminated unions - compare to inheritance hell
- Active patterns - compare to scala unapply

***

### F# Mongo driver

    let collection : IMongoCollection<Item> = ...

    mongo {
        for x in collection do
        update
        set x.Price 0.99
        inc x.Quantity 1
    }

***

### Akka.NET

- Akka for .NET
- Ported from Scala
- Written in C#
- API for F#
- Portable - mono on Unix
- Production ready

***
