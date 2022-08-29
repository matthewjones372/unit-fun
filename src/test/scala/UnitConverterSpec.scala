import zio.test.Assertion._
import zio.test._

object UnitConversionSpec extends ZIOSpecDefault {

  import UnitConverter._

  def spec = suite("fact tests")(
    test("can convert a type to itself")(
      {
        assert(UnitConverter(List.empty).convert(UnitQuery("m", "m", 12)))(isRight(equalTo(QueryResult(12, "m"))))
      }
    ),
    test("can convert a 1-to-1 fact")({
      assert(UnitConverter(List(Fact("m", Conversion("ft", 12)))).convert(UnitQuery("m", "ft", 2)))(
        isRight(equalTo(QueryResult(24, "ft")))
      )
    }),
    test("returns an error if the type cannot be converted")({
      val query = UnitQuery("m", "min", 10)
      assert(UnitConverter(List.empty).convert(query))(isLeft(equalTo(ConversionFactDoesNotExist(query))))
    }),
    test("can solve nested facts")({
      val facts = List(Fact("m", Conversion("ft", 3.28)), Fact("ft", Conversion("in", 12)))
      val query = UnitQuery("m", "in", 3)
      assert(UnitConverter(facts).convert(query))(isRight(equalTo(QueryResult(118.08, "in"))))
    }),
    test("can solve nested facts backwards")({
      val facts  = List(Fact("m", Conversion("ft", 3.28)), Fact("ft", Conversion("in", 12)))
      val query  = UnitQuery("in", "m", 118.08)
      val result = UnitConverter(facts).convert(query)
      assert(result.map(_.value))(isRight(approximatelyEquals(BigDecimal(3), BigDecimal(0)))) &&
      assert(result.map(_.unit))(isRight(equalTo("m")))
    }),
    test("round-trips result in the original value with some rounding")({
      check(Gen.bigDecimal(Long.MinValue, Long.MaxValue)) { fromValue =>
        val facts =
          List(
            Fact("decade", Conversion("year", 10)),
            Fact("year", Conversion("day", 365)),
            Fact("day", Conversion("hr", 24)),
            Fact("hr", Conversion("min", 60)),
            Fact("min", Conversion("second", 60)),
            Fact("second", Conversion("ms", 1000)),
            Fact("ms", Conversion("ns", 100_00_00)),
          )
        val forwardQuery = UnitQuery("decade", "ns", fromValue)
        val forwardResult = UnitConverter(facts).convert(forwardQuery)
        val roundTrip = forwardResult.map(_.value).flatMap { res =>
          UnitConverter(facts).convert(UnitQuery("ns", "decade", res))
        }

        assert(roundTrip.map(_.value))(isRight(approximatelyEquals(fromValue, BigDecimal(2))))
      }
    })
  )
}
