# LINQ Alternatives

| C# | Scala | F# | Haskell |
| -- | ----- | -- | ------- |
| ``` xs.Aggregate(accumFunc)``` | ```xs.reduceLeft(accumFunc)``` | ``` Seq.reduceLeft accumFunc xs``` | ```;``` |
| ```xs.Aggregate(seed, accumFunc)``` | ```xs.foldLeft(seed)(accumFunc)``` | ```Seq.fold seed accumFunc xs``` | ```;``` |
| ```xs.Aggregate(seed, accumFunc, trans)``` | ```trans(xs.foldLeft(seed)(accumFunc))``` | ```Seq.mapFold``` | ```;``` |
| ```xs.All(pred)``` | ```xs.forall(pred)``` | ```Seq.forall pred xs``` | ```;``` |
| ```xs.Any()``` | ```xs.nonEmpty``` | ```Seq.isEmpty >> not``` | ```;``` |
| ```xs.Any(pred)``` | ```xs.exists(pred)``` | ```Seq.exists pred xs ``` | ```;``` |
| ```xs.AsEnumerable()``` | ```xs.asTraversable // roughly``` | ```List.toSeq xs``` | ```;``` |
| ```xs.Average()``` | ```xs.sum / xs.length``` | ```Seq.average xs``` | ```;``` |
| ```xs.Average(trans)``` | ```trans(xs.sum / xs.length)``` | ``` Seq.averageBy trans xs ``` | ```;``` |
| ```xs.Cast<A>()``` | ```xs.map(_.asInstanceOf[A])``` | ```Seq.cast xs // the type is inferred by usage``` | ```;``` |
| ```xs.Concat(ys)``` | ```xs++ ys``` | ```xs @ ys ``` | ```;``` |
| ```xs.Contains(x)``` | ```xs.contains(x)``` | ```Seq.contains x xs``` | ```;``` |
| ```xs.Contains(x, eq)``` | ```xs.exists(eq(x, _))``` | ```;``` | ```;``` |
| ```xs.Count()``` | ```xs.size``` | ```Seq.count xs``` | ```;``` |
| ```xs.Count(pred)``` | ```xs.count(pred)``` | ```Seq.countBy pred xs``` | ```;``` |
| ```xs.DefaultIfEmpty()``` | ```if (xs.isEmpty) List(0) else``` | ```;``` | ```;``` |
| ```xs // Use `mzero` (from Scalaz) instead of 0 for more genericity``` | `````` | ```;``` | ```;``` |
| ```xs.DefaultIfEmpty(v)``` | ```if (xs.isEmpty) List(v) else``` | ```;``` | ```;``` |
| ```xs``` | `````` | ```;``` | ```;``` |
| ```xs.Distinct()``` | ```xs.distinct``` | ```Seq.distinct xs; Seq.distinctBy trans xs``` | ```;``` |
| ```xs.ElementAt(i)``` | ```xs(i)``` | ```Seq.item i xs``` | ```;``` |
| ```xs.ElementAtOrDefault(i)``` | ```xs.lift(i).orZero // `orZero` is from Scalaz``` | ```;``` | ```;``` |
| ```xs.Except(ys)``` | ```xs.diff(ys)``` | ```;``` | ```;``` |
| ```xs.First()``` | ```xs.head``` | ```Seq.head xs``` | ```;``` |
| ```xs.First(pred)``` | ```xs.find(pred) // returns an `Option``` | ```Seq.find pred xs``` | ```;``` |
| ```xs.FirstOrDefault()``` | ```xs.headOption.orZero``` | ```Seq.tryHead xs``` | ```;``` |
| ```xs.FirstOrDefault(pred)``` | ```xs.find(pred).orZero``` | ```;``` | ```;``` |
| ```xs.GroupBy(f)``` | ```xs.groupBy(f)``` | ```Seq.groupBy f xs``` | ```;``` |
| ```xs.GroupBy(f, g)``` | ```xs.groupBy(f).mapValues(_.map(g))``` | ```;``` | ```;``` |
| ```xs.Intersect(ys)``` | ```xs.intersect(ys)``` | ```;``` | ```;``` |
| ```xs.Last()``` | ```xs.last``` | ```Seq.last xs``` | ```;``` |
| ```xs.Last(pred)``` | ```xs.reverseIterator.find(pred) // returns an `Option``` | ```;``` | ```;``` |
| ```xs.LastOrDefault()``` | ```xs.lastOption.orZero``` | ```Seq.tryLast xs``` | ```;``` |
| ```xs.LastOrDefault(pred)``` | ```xs.reverseIterator.find(pred).orZero``` | ```;``` | ```;``` |
| ```xs.Max()``` | ```xs.max``` | ```Seq.max xs``` | ```;``` |
| ```xs.Max(f)``` | ```xs.maxBy(f)``` | ```Seq.maxBy f xs``` | ```;``` |
| ```xs.Min()``` | ```xs.min``` | ```Seq.min xs``` | ```;``` |
| ```xs.Min(f)``` | ```xs.minBy(f)``` | ```Seq.minBy f xs``` | ```;``` |
| ```xs.OfType<A>()``` | ```xs.collect { case x: A => x }``` | ```;``` | ```;``` |
| ```xs.OrderBy(f)``` | ```xs.sortBy(f)``` | ```;``` | ```;``` |
| ```xs.OrderBy(f, comp)``` | ```xs.sortBy(f)(comp) // `comp` is an `Ordering`.``` | ```;``` | ```;``` |
| ```xs.OrderByDescending(f)``` | ```xs.sortBy(f)(implicitly[Ordering[A]].reverse)``` | ```;``` | ```;``` |
| ```xs.OrderByDescending(f, comp)``` | ```xs.sortBy(f)(comp.reverse)``` | ```;``` | ```;``` |
| ```Enumerable.Range(start, count)``` | ```start until start +count``` | ```[start .. step .. until]``` | ```;``` |
| ```Enumerable.Repeat(x, times)``` | ```Iterator.continually(x).take(times)``` | ```Seq.replicate times x``` | ```;``` |
| ```xs.Reverse()``` | ```xs.reverse``` | ```Seq.rev xs``` | ```;``` |
| ```xs.Select(trans)``` | ```xs.map(trans) // For indexed overload, first `zipWithIndex` and then `map`.``` | ```Seq.map trans xs``` | ```;``` |
| ```xs.SelectMany(trans)``` | ```xs.flatMap(trans)``` | ```Seq.collect trans xs``` | ```;``` |
| ```xs.SequenceEqual(ys)``` | ```xs.sameElements(ys)``` | ```;``` | ```;``` |
| ```xs.Skip(n)``` | ```xs.drop(n)``` | ```Seq.skip n xs``` | ```;``` |
| ```xs.SkipWhile(pred)``` | ```xs.dropWhile(pred)``` | ```Seq.dropWhile pred xs``` | ```;``` |
| ```xs.Sum()``` | ```xs.sum``` | ```Seq.sum xs``` | ```;``` |
| ```xs.Sum(f)``` | ```xs.map(f).sum // or `xs.foldMap(f)`. Requires Scalaz.``` | ```;``` | ```;``` |
| ```xs.Take(n)``` | ```xs.take(n)``` | ```Seq.take n xs``` | ```;``` |
| ```xs.TakeWhile(pred)``` | ```xs.takeWhile(pred)``` | ```Seq.takeWhile pred xs``` | ```;``` |
| ```xs.OrderBy(f).ThenBy(g)``` | ```xs.sortBy(x => (f(x), g(x))) // Or: xs.sortBy(f &&& g). `&&&` is from Scalaz.``` | ```Seq.sortBy (fun x -> f x, g x) xs``` | ```;``` |
| ```xs.ToArray()``` | ```xs.toArray // Use `xs.toIndexedSeq` for immutable indexed sequence.``` | ```Seq.toArray xs``` | ```;``` |
| ```xs.ToDictionary(f)``` | ```xs.map(f.first).toMap // `first` is from Scalaz. When f = identity, you can just write `xs.toMap`.``` | ```Map.fromSeq xs``` | ```;``` |
| ```xs.ToList()``` | ```xs.toList // This returns an immutable list. Use `xs.toBuffer` if you want a mutable list.``` | ```Seq.toList xs``` | ```;``` |
| ```xs.Union(ys)``` | ```xs.union(ys)``` | ```;``` | ```;``` |
| ```xs.Where(pred)``` | ```xs.filter(pred)``` | ```Seq.filter pred xs``` | ```;``` |
| ```xs.Zip(ys, f)``` | ```(xs, ys).zipped.map(f) // When f = identity, use `xs.zip(ys)`.``` | ```Seq.zip f ys xs``` | ```;``` |
