## Scratch Project having fun with unit conversions


Given a list of facts such as;

```
- ("hr", "min", 60)
- ("min", "second", 60)
- ("second", "ms", 1000)
```
and a query such as ("hr", "ms", 5) we can convert 
between units. 

##### Examples
```
query ("hr", "ms", 5) -> 3.6e+6 (approximately)
```
##### Note there's no need to repeat a fact, we should be able to convert backwards

Given facts: 
```
- ("hr", "min", 60)
- ("min", "second", 60)
- ("second", "ms", 1000)
```
we can convert ms to hr