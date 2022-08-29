import UnitConverter._

import scala.annotation.tailrec

trait UnitConverter {
  def convert(query: UnitQuery): Either[UnitConversionError, QueryResult]
}

class UnitConverterDefaultImpl(facts: => List[Fact]) extends UnitConverter {
  override def convert(query: UnitQuery): Either[UnitConversionError, QueryResult] =
    if (query.isTarget)
      QueryResult(query.value, query.fromUnitType).right
    else
      forward(query)
        .orElse(backward(query))
        .toRight(ConversionFactDoesNotExist(query))

  @tailrec
  protected final def queryFrom(table: => Map[String, Conversion])(query: UnitQuery): Option[QueryResult] =
    table.get(query.fromUnitType) match {
      case None =>
        None
      case Some(Conversion(unitType, rate)) if query.targetUnit == unitType =>
        QueryResult(query.value * rate, unitType).option
      case Some(Conversion(nestedType, nestedRate)) =>
        queryFrom(table)(UnitQuery(nestedType, query.targetUnit, query.value * nestedRate))
    }

  private lazy val forward = queryFrom(facts.map { case Fact(from, conversion) =>
    from -> conversion
  }.toMap)(_)

  private lazy val backward = queryFrom(
    facts
      .map(_.invert)
      .map { case Fact(from, conversion) =>
        from -> conversion
      }
      .toMap
  )(_)
}

object UnitConverter {
  final case class Conversion(toUnitType: String, conversionRate: BigDecimal)

  final case class Fact(fromUnitType: String, fact: Conversion) {
    def invert: Fact = Fact(fact.toUnitType, Conversion(fromUnitType, 1.0 / fact.conversionRate))
  }

  final case class UnitQuery(fromUnitType: String, targetUnit: String, value: BigDecimal) {
    def isTarget: Boolean = fromUnitType == targetUnit
  }

  final case class QueryResult(value: BigDecimal, unit: String) {
    def right: Either[Nothing, QueryResult] = Right(this)

    def option: Option[QueryResult] = Option(this)
  }

  sealed class UnitConversionError(errorMessage: String) extends RuntimeException(errorMessage) {
    self =>
    override def toString: String = Option(self.getLocalizedMessage).getOrElse("Could not convert type")
  }

  final case class ConversionFactDoesNotExist(query: UnitQuery)
      extends UnitConversionError(
        s"Fact for conversion from type: ${query.fromUnitType} to type: ${query.targetUnit} does not exist"
      )

  def apply(facts: List[Fact]) = new UnitConverterDefaultImpl(facts)
}
