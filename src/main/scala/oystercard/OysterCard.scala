package oystercard

object OysterCard extends App {

  val oysterCard = new OysterCard(BigDecimal("30.00"))

  val trip1 = Trip(Tube, Holborn, EarlsCourt, oysterCard)
  trip1.doTrip()
  println(oysterCard.balance)

  val trip2 = Trip(Bus, EarlsCourt, Chelsea, oysterCard)
  trip2.doTrip()
  println(oysterCard.balance)

  val trip3 = Trip(Tube, EarlsCourt, Hammersmith, oysterCard)
  trip3.doTrip()
  println(oysterCard.balance)
}

case class OysterCard(var balance: BigDecimal) { // OysterCard class having only balance as the property

  def chargeMaxFare(): Unit = { // to charge the max fare
    val newBalance = balance - BigDecimal("3.20")
    try {
      if (newBalance < BigDecimal("0.00")) throw new BalanceTooLowException()
      else this.setBalance(newBalance)
    } catch {
      case e: BalanceTooLowException => throwBalanceTooLowException
    }
  }

  def reloadMaxCharge(): Unit = { // to reload the initial charging of the max fare
    val newBalance = balance + BigDecimal("3.20")
    this.setBalance(newBalance)
  }

  def chargeZone1(): Unit = { // to charge trips inside Zone1
    val newBalance = balance - BigDecimal("2.50")
    this.setBalance(newBalance)
  }

  def chargeOtherZones(): Unit = { // to charge trips inside a zone excluding Zone1
    val newBalance = balance - BigDecimal("2.00")
    this.setBalance(newBalance)
  }

  def chargeTwoZonesIncZone1(): Unit = { // to charge trips between two zones including Zone1
    val newBalance = balance - BigDecimal("3.00")
    this.setBalance(newBalance)
  }

  def chargeTwoZonesExcZone1(): Unit = { // to charge trips between two zones excluding Zone1
    val newBalance = balance - BigDecimal("2.25")
    this.setBalance(newBalance)
  }

  def chargeMoreThanTwoZones(): Unit = { // to charge trips crossing more than two zones
    val newBalance = balance - BigDecimal("3.20")
    this.setBalance(newBalance)
  }

  def chargeBus(): Unit = { // to charge any bus journey
    val newBalance = balance - BigDecimal("1.80")
    this.setBalance(newBalance)
  }

  def loadCredit(amount: BigDecimal): Unit = { // to load credit for OysterCard
    this.setBalance(amount + balance)
  }

  def setBalance(amount: BigDecimal): Unit = { // mutator method for balance property
    this.balance = amount
  }

  def getBalance(amount: BigDecimal): BigDecimal = { // accessor method for balance property
    this.balance
  }

  def throwBalanceTooLowException: Unit = { // to throw exception on low balance and exit the program
    println("Balance is too low, you cannot travel!")
    throw new BalanceTooLowException
    System.exit(-1) // this is just to exit the program in case of a low balance
  }
}

case class Trip(transportType: TransportType, enterStation: Station, exitStation: Station, card: OysterCard) { // This class has the logic for applying charging functions in OysterCard class
  def doTrip(): Unit = transportType match {
    case Bus => card.chargeBus()
    case Tube => // whole logic is only implemented for subway
      card.chargeMaxFare() // max fare is charged beforehand, because in real case we don't know the exit station
      if (enterStation != null && !enterStation.equals(exitStation)) {
        if (exitStation != null) // This null check also works for case that the user doesn't swipe out
          if (Math.abs(enterStation.zones.max - exitStation.zones.max) == 0 ||
            Math.abs(enterStation.zones.min - exitStation.zones.min) == 0) // This works under the assumption that a station could be belong to at most two zones
            if (enterStation.zones.max == 1) {
              card.reloadMaxCharge()
              card.chargeZone1()
            }
            else {
              card.reloadMaxCharge()
              card.chargeOtherZones()
            }
          else if (Math.abs(enterStation.zones.max - exitStation.zones.max) == 1)
            if (enterStation.zones.max > 1 && exitStation.zones.max > 1) {
              card.reloadMaxCharge()
              card.chargeTwoZonesExcZone1()
            } else {
              card.reloadMaxCharge()
              card.chargeTwoZonesIncZone1()
            }
          else if (Math.abs(enterStation.zones.max - exitStation.zones.max) > 1) { // This case might seem unnecessary, but implemented to present the flow
            card.reloadMaxCharge()
            card.chargeMoreThanTwoZones()
          } else {
            card.reloadMaxCharge() // reloading charge of the max fare in case of an non defined fare rule to favour the customer
            throwNonDefinedFareRuleException
          }
        else {
          try {
            throw new UserNotSwipeOutException
          } catch {
            case e: UserNotSwipeOutException => println("User didn't swipe out, max fare charged")
          }
        }
      } else {
        card.reloadMaxCharge() // reloading charge of the max fare in case of an non defined fare rule to favour the customer
        throwNonDefinedFareRuleException
      }

  }

  def throwNonDefinedFareRuleException(): Unit = { // to throw the exception and exit the program
    println("Non defined fare rule encountered")
    throw new NonDefinedFareRuleException
    System.exit(-1) // this is just to exit the program in case of a non defined fare condition
  }
}

class Station(val name: String, val zones: Set[Int]) // The base class for stations, all stations extended from this as an Object

case object Holborn extends Station("Holborn", Set(1))

case object Aldgate extends Station("Aldgate", Set(1))

case object EarlsCourt extends Station("Earl’s Court", Set(1, 2))

case object Hammersmith extends Station("Hammersmith", Set(2))

case object Chelsea extends Station("Chelsea", Set(2)) // In the task document, there are both Arsenal and Chelsea names for the same station

case object Wimbledon extends Station("Wimbledon", Set(3))

class TransportType // The base class for transport type

case object Bus extends TransportType

case object Tube extends TransportType

class BalanceTooLowException extends Exception // to handle exception for low balance

class UserNotSwipeOutException extends Exception // to handle exception for non defined fare rule

class NonDefinedFareRuleException extends Exception // to handle exception for non defined fare rule