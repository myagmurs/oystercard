import oystercard._
import org.scalatest.FlatSpec

class OysterCardTest extends FlatSpec {

  "Initially loaded OysterCard" should "be possible" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    assert(oysterCard.balance === BigDecimal("30.00"))
  }

  "Loading an OysterCard" should "be possible credit anytime" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.loadCredit(BigDecimal("10"))
    assert(oysterCard.balance === BigDecimal("40.00"))
  }

  "A trip inside Zone 1" should "charged by 2.50" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeZone1()
    assert(oysterCard.balance === BigDecimal("27.50"))
  }

  "A trip inside the same zone but not in Zone 1" should "charged by 2.00" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeOtherZones()
    assert(oysterCard.balance === BigDecimal("28.00"))
  }

  "A trip between two zones excluding Zone 1" should "charged by 2.25" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeTwoZonesExcZone1()
    assert(oysterCard.balance === BigDecimal("27.75"))
  }

  "A trip between two zones including Zone 1" should "charged by 3.00" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeTwoZonesIncZone1()
    assert(oysterCard.balance === BigDecimal("27.00"))
  }

  "A trip crossing more than two zones" should "charged by 3.20" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeMoreThanTwoZones()
    assert(oysterCard.balance === BigDecimal("26.80"))
  }

  "Max fare and trip crossing more than two zones" should "be charged the same" in {
    val credit = BigDecimal("30.00")
    val oysterCard1 = new OysterCard(credit)
    oysterCard1.chargeMaxFare()
    val oysterCard2 = new OysterCard(credit)
    oysterCard2.chargeMoreThanTwoZones()
    assert(oysterCard1.balance === oysterCard2.balance)
  }

  "A bus journey " should "always be charged by 1.80" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    oysterCard.chargeBus()
    assert(oysterCard.balance === BigDecimal("28.20"))
  }

  "A trip from Holborn to Aldgate " should "be charged by 2.50" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Holborn, Aldgate, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("27.50"))
  }

  "A trip from Holborn to Earl's Court " should "be charged by 2.50" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Holborn, EarlsCourt, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("27.50"))
  }

  "A trip from Hammersmith to Arsenal " should "be charged by 2.00" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Hammersmith, Chelsea, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("28.00"))
  }

  "A trip from Earl's Court to Hammersmith " should "be charged by 2.00" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, EarlsCourt, Hammersmith, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("28.00"))
  }

  "A trip from Hammersmith to Wimbledon " should "be charged by 2.25" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Hammersmith, Wimbledon, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("27.75"))
  }

  "A trip from Earl's Court to Wimbledon " should "be charged by 2.25" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, EarlsCourt, Wimbledon, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("27.75"))
  }

  "A trip from Holborn to Wimbledon " should "be charged by 3.20" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Holborn, Wimbledon, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("26.80"))
  }

  "A bus trip from Holborn to Wimbledon " should "be charged by 1.80" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Bus, Hammersmith, Chelsea, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("28.20"))
  }

  "A trip from Holborn to Earl's Court " should "be charged by 1.80" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Bus, Holborn, EarlsCourt, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("28.20"))
  }

  "A bus trip from Hammersmith to Arsenal " should "be charged by 1.80" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Bus, Hammersmith, Chelsea, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("28.20"))
  }

  "A trip without an exit station " should "be charged by 3.20" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Hammersmith, null, oysterCard)
    trip1.doTrip()
    assert(oysterCard.balance === BigDecimal("26.80"))

  }

  "At least a balance as much as max fare on OysterCard" should "be loaded to have a trip" in {
    val credit = BigDecimal("3.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Hammersmith, null, oysterCard)
    assertThrows[BalanceTooLowException] {
      trip1.doTrip()
    }
  }

  "A trip from a station to the same station" should " not be charged" in {
    val credit = BigDecimal("30.00")
    val oysterCard = new OysterCard(credit)
    val trip1 = Trip(Tube, Hammersmith, Hammersmith, oysterCard)
    assertThrows[NonDefinedFareRuleException] {
      trip1.doTrip()
      assert(oysterCard.balance === BigDecimal("30.00"))
    }
  }

}
