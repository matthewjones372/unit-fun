# unit-fun

A small Scala exercise in converting between units from a list of facts.

Given facts such as:

```scala
List(
  Fact("hr", Conversion("min", 60)),
  Fact("min", Conversion("second", 60)),
  Fact("second", Conversion("ms", 1000))
)
```

a query such as `UnitQuery("hr", "ms", 5)` follows the chain hr to min to second to ms and returns
`QueryResult(18000000, "ms")`. Each fact also works backwards, so the same facts convert ms to hr without a second
fact for the reverse direction.

```scala
UnitConverter(facts).convert(UnitQuery("hr", "ms", 5))
// Right(QueryResult(18000000, "ms"))

UnitConverter(facts).convert(UnitQuery("ms", "hr", 18000000))
// Right(QueryResult(5, "hr")), give or take rounding

UnitConverter(facts).convert(UnitQuery("hr", "kg", 1))
// Left(ConversionFactDoesNotExist(...))
```

## How it works

The facts become two lookup tables, one for each direction, keyed by the unit being converted from. A query walks one
table, multiplying by each rate, until it reaches the target unit or runs out of facts. It tries the forward table
first, then the backward one. Values are `BigDecimal`, so going backwards divides without losing much precision.

That keeps it simple, but it means the facts have to form a single chain. Each unit can have only one conversion out
of it and one into it, and a later fact replaces an earlier one. With facts km to m and mile to m, there's no answer
for km to mile, because that would mean going forwards to m and then backwards to mile.

## Running the tests

```
sbt test
```

The tests use ZIO Test, including a property test that converts random values from decades to nanoseconds and back
and checks they come back within rounding. They run on JDK 17.
