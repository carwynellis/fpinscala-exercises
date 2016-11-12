package fpinscala.testing

object GenSpecifications extends App {

  // Example property from section 8.4.1 - Some simple examples
  val smallInt = Gen.choose(-10, 10)

  val maxProp = Prop.forAll(
    SGen.listOfAtLeastOne(smallInt),
    "max of list should be greater than or equal to every other element") { ns: List[Int] =>
      val max = ns.max
      !ns.exists(_ > max)
    }

  Prop.run(maxProp)
}
