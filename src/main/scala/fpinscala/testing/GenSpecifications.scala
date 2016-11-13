package fpinscala.testing

object GenSpecifications extends App {

  def maxOfListExampleProperty() = {
    // Example property from section 8.4.1 - Some simple examples
    val smallInt = Gen.choose(-10, 10)

    val maxProp = Prop.forAll(
      SGen.listOfAtLeastOne(smallInt),
      "max of list should be greater than or equal to every other element") {
        ns: List[Int] =>
          val max = ns.max
          !ns.exists(_ > max)
      }
    Prop.run(maxProp)
  }

  def sortedListProperty() = {
    val ints = Gen.choose(-100, 100)

    val sortedProp = Prop.forAll(
      SGen.listOfAtLeastOne(ints),
      "List.sorted should always yield a sorted list") { l: List[Int] =>
        val sorted = l.sorted
        // Sorted list should be of the same size as the original input list
        (l.size == sorted.size) &&
        // Sorted list head should be less than or equal to sorted list tail
        (sorted.head <= sorted.last) &&
        // Successive pairs of elements should be in sorted order
        ! (sorted.zip(sorted.tail) exists { case (a: Int, b: Int) => a > b })
      }

    Prop.run(sortedProp)
  }

  def simpleCheckExample() = {
    val falseOrTrueCheck = Prop.check({false || true }, "f || t yields t")
    Prop.run(falseOrTrueCheck)
  }

  maxOfListExampleProperty()
  sortedListProperty()
  simpleCheckExample()
}
