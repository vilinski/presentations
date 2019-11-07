- title : Async
- description : Introduction to async programming
- author : Andreas Vilinski
- theme : night
- transition : default

***

### Async programming

First about different approaches

- WWW - world wide waiting
- APM - IAsyncResult
- TPL - Task Parallell Library
- async/await
- Rx
- Message queues
- Actor - Erlang/Akka/Akka.NET

' First about different approaches, mostly in C# just becauses

---

### Traditional synchronous approach

```csharp
static void Main(string[] args)
{
    var fs = File.OpenRead(@"c:\Some\Big\DataFile.txt");
    byte[] buffer = new byte[fs.Length];
    public int Read(byte [] buffer, int offset, int count);
    ...
}
```

' wasting user time where OS makes io-thread in background anyway

***

### APM

```csharp
static void Main(string[] args)
{
    byte[] readBuffer;
    var fs = File.OpenRead(@"c:\somefile.txt");
    readBuffer = new byte[fs.Length];
    var result = fs.BeginRead(readBuffer, 0, (int)fs.Length, OnReadComplete, fs);

    //do other work here while file is read...

    Console.ReadLine();
}
private static void OnReadComplete(IAsyncResult result)
{
    var stream = (FileStream)result.AsyncState;
    var bytesRead = stream.EndRead(result);
    Console.WriteLine("Read {0} bytes successfully.", bytesRead);
    stream.Dispose();
}
```

' callback hell
' two entcopled methods Begin and End

---

### APM

- Asynchronous Programming Model
- Separate XYBegin und OnXYComplete methods - readability
- No way to cancel
- No error handling
  + No [using](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-statement) statement for resources possible - create and dispose in different methods
- No synchronization between callback and caller thread
- No coordination for multiple async operations

---

### APM

APM with lambda indstead of separate method

```csharp
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
```

' instead of second method you can write a lambda. Other disadvantages are still here

***

### TAP

- Task-based Asynchronous Programming
- [System.Threading.Tasks.Task<TResult>](https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks.task)
- Promice or Future in other languages

---

### TAP - Task-based Async Programming

```csharp
private static Task<int> ReadFileAsync(string filePath)
{
    var fs = File.OpenRead(filePath);
    var readBuffer = new byte[fs.Length];
    var readTask = fs.ReadAsync(readBuffer, 0, (int)fs.Length);
    // var result = readTask.Result;
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
```

' Now you can chain computations, cancel them
' Continuation can be fired on special computation results like success or error
' Code clutter with lambdas or separate methods for each continuation

---

### TAP - Task-based Async Programming

```csharp
static void Main(string[] args)
{
    var read1 = ReadFileAsync(@"c:\somefile.txt");
    var read2 = ReadFileAsync(@"c:\someotherfile.txt");
    Task.WhenAll(read1, read2)
        .ContinueWith(task => Console.WriteLine("All done."));
    //do other work here while files are read...
}
```

' Can handle multiple computations a once

---

### TAP - Promise in JavaScript

```js
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
```

---

***

### Async/Await - sugar for TAP

```csharp
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
```

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

```fsharp
    let urlList = [
        "https://fsharpforfunandprofit.com/"
        "https://safe-stack.github.io/"
        "https://freya.io/" ]

    let fetchHtmlAsync url = async {
        let uri = new System.Uri(url)
        let webClient = new System.Net.WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        return html
    }

    let htmlList =
        urlList
        |> Seq.map fetchHtmlAsync // Build an Async<'a> for each site
        |> Async.Parallel         // each Async<'a> across different threads
        |> Async.RunSynchronously // Run each Async<'a>, non-blocking wait
```

***

### [Reactive Extensions](http://reactivex.io) - another approach to asynchronity

<table>
<th><td>sync/pull</td><td>async/push</td></th>
<tr><td>one</td><td>

```cs
TResult
```

</td><td>

```cs
Task<TResult>
```

</td></tr><tr><td>multiple</td><td>

```cs
IEnumerable<TResult>
```

</td><td>

```cs
IObservable<TResult>
```

</td></tr></table>

- The Observer pattern done right
- Invented in C# by [Erik Meyer](https://twitter.com/headinthebox)
- Choose your plattform:
  + [Rx.NET](https://github.com/Reactive-Extensions/Rx.NET), [RxScala](https://github.com/ReactiveX/RxScala), RxJava, RxJS, RxSwift, ...

---

### Rx - [Your Mouse is a Database](https://queue.acm.org/detail.cfm?id=2169076)

```cs
TextChanges(input)
  .DistinctUntilChanged()
  .Throttle(TimeSpan.FromMilliSeconds(10))
  .Select(word => Completions(word))
  .Switch()
  .Subscribe(ObserveChanges(output));
```

---

### Rx - reactive Paint in WPF

```cs
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
```cs

***

### Message queues

<table style="width:100%">
<tr>
<td>

Implementations:

- ActiveMQ
- ZeroMQ
- [RabbitMQ](https://rabbitmq.com)
- [Kafka](kafka.apache.org)
- Apache Qpid
- Apache Thrift
- ...

</td>
<td>

Advantages:

+ [Fast](http://web.archive.org/web/20160316023831/http://www.kuntalganguly.com/2014/08/message-queue-comparision.html) - [some](http://bravenewgeek.com/dissecting-message-queues/) [benchmarks](http://blog.x-aeon.com/2013/04/10/a-quick-message-queue-benchmark-activemq-rabbitmq-hornetq-qpid-apollo/)
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

| 1ps    | 10-12e | Switching time of transistor |
|--------|--------|------------------------------|
| 330ps  |        | 1 cycle on modern CPU        |
| 4ns    | 10-9e  | Light traveling 1m of fiber  |
| 50μs   | 10-6e  | Internal latency of Nasdaq   |
| 18.5ms | 10-3e  | Light traveling from NYC to LDN |
| 300ms  |        | Blink of an eye              |

---

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
- In-process usable too
- Failure safe
- Supervision trees
- Back-pressure - Publisher/Subscriber
- Low level - extra programming effort
- Own communication protocol

---

### Akka.NET

- Akka for .NET
- Ported from Scala
- Written in C#
- API for F#
- .NET core
- Production ready sinse ... years

---

### Orleans

- Virtual actor
-


### Concurrency vs Parallelizm

---

### Concurrency

- Processing multiple tasks interleaved
- Does NOT require more than one thread to work

![Concurrency](images/concurrency.mp4)

---

### Parallelizm

- Executing multiple tasks at the same time to finish them faster

![Parallelizm](images/parallelizm.mp4)

---

### Thread

    Basic unit of CPU utilization
    Typically part of Operating System
    Threads can share memory
    Processors can usually run up to 1 thread per CPU's core at the same time

### Thread on .NET

- 1:1 OS Thread
- Each takes 2MB for switching context
  - Context switch: save state, reload old, drop cache
- Thread pools
  - Take care of managing threads for us
  - Can reuse threads
  - Number is bigger than CPU count, but limited
  - More threads != faster

---

# Blocking Threads

- `Task.Wait()` - Don't
- Thread is being wasted, might prevent other tasks from being scheduled if the thread pool is limited
- Use timeouts
- Use `await` -
  - Task waits for certain signal before it can complete
  - Doesn't really block any threads, other tasks can execute in the meantime



