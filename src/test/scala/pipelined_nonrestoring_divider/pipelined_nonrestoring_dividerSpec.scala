package pipelined_nonrestoring_divider

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import scala.collection.mutable.Queue

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly gcd.GCDSpec
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly gcd.GCDSpec'
  * }}}
  * Testing from mill:
  * {{{
  * mill chisel_nonrestoring_intdiv.test.testOnly gcd.GCDSpec
  * }}}
  */
class pipelined_nonrestoring_dividerSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  def run_test_case(dut: non_restoring_divider, dividends: Seq[UInt], divisors: Seq[UInt]) = {
    var sent, received, cycles: Int = 0
    val dividends_queue = Queue[Int]()
    val divisors_queue = Queue[Int]()
    while (sent < num_cases || received < num_cases) {
      assert(cycles <= 1000, "timeout reached")

      dut.io.dividend.valid.poke(false.B)
      dut.io.divisor.valid.poke(false.B)
      dut.io.quotient.ready.poke(false.B)
      dut.io.remainder.ready.poke(false.B)

      if (sent < num_cases) {
        dut.io.dividend.valid.poke(true.B)
        dut.io.divisor.valid.poke(true.B)

        dut.io.dividend.bits.poke(dividends(sent))
        dut.io.divisor.bits.poke(divisors(sent))
        if (dut.io.dividend.ready.peek().litToBoolean && dut.io.divisor.ready.peek().litToBoolean) {
          dividends_queue.enqueue(dividends(sent).litValue.toInt)
          divisors_queue.enqueue(divisors(sent).litValue.toInt)
          sent += 1
        }
      }

      if (received < num_cases) {
        dut.io.quotient.ready.poke(true.B)
        dut.io.remainder.ready.poke(true.B)
        if (dut.io.quotient.valid.peek().litToBoolean && dut.io.remainder.valid.peek().litToBoolean) {
          val dividend = dividends_queue.dequeue()
          val divisor = divisors_queue.dequeue()
          val quotient = dut.io.quotient.bits.peek().litValue.toInt
          val remainder = dut.io.remainder.bits.peek().litValue.toInt
          withClue(s"Dividend $dividend and Divisor $divisor: Quotient: ${quotient} Remainder: ${remainder}\n") {
              quotient shouldBe (dividend / divisor)
              remainder shouldBe (dividend% divisor)
          }
          received += 1
        }
      }
      //println(s"Sent $sent and Recieved $received")

      // Step the simulation forward.  
      dut.clock.step()
      cycles += 1
    }
  }

  val width = 32;
  val num_cases = 100;


  behavior of "pipelined nonrestoring divider"
  it should "perform pipelined nonrestoring integer division" in {
    test(new non_restoring_divider())
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.reset.poke(true.B)
      dut.clock.step(width)
      dut.reset.poke(false.B)
      dut.clock.step()

      println("Positive ints dividend > divisor")
      val dividends_1  = Seq.fill(num_cases)((BigInt(width, Random) + BigInt("8000000", 16)).mod(BigInt("80000000", 16)).U(width.W))
      val divisors_1   = Seq.fill(num_cases)(BigInt(width, Random).mod(BigInt("8000000", 16)).U(width.W))
      run_test_case(dut, dividends_1, divisors_1)

      println("Positive ints")
      val dividends_2  = Seq.fill(num_cases)(BigInt(width, Random).mod(BigInt("80000000", 16)).U(width.W))
      val divisors_2   = Seq.fill(num_cases)(BigInt(width, Random).mod(BigInt("80000000", 16)).U(width.W))
      run_test_case(dut, dividends_2, divisors_2)

      println("Negative dividends")
      val dividends_3  = Seq.fill(num_cases)((BigInt(width-1, Random) + BigInt("80000000", 16)).U(width.W))
      val divisors_3   = Seq.fill(num_cases)(BigInt(width, Random).mod(BigInt("8000000", 16)).U(width.W))
      run_test_case(dut, dividends_3, divisors_3)

      println("Negative divisors")
      val dividends_4  = Seq.fill(num_cases)((BigInt(width-1, Random)).U(width.W))
      val divisors_4   = Seq.fill(num_cases)((BigInt(width-1, Random) + BigInt("80000000", 16)).U(width.W))
      run_test_case(dut, dividends_4, divisors_4)

      println("Random")
      val dividends_5  = Seq.fill(num_cases*10)(BigInt(width, Random).U(width.W))
      val divisors_5   = Seq.fill(num_cases*10)(BigInt(width, Random).U(width.W))
      run_test_case(dut, dividends_5, divisors_5)

    }
  }
}

