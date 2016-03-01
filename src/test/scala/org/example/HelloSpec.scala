import org.specs2.mutable._

import scala.concurrent._
import scala.util.{ Success, Failure }
import ExecutionContext.Implicits.global

import org.scalatest.FunSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time._
import org.scalatest.time.SpanSugar._
import org.scalatest.concurrent.Timeouts._

class HelloSpec extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("Basic Object") {

    it("Basic Function") {
      assert(1 == 1)
    }

    it("Basic Promise") {
      val nums: List[Int] = List(0, 1, 2, 3)
      var nums2: Future[List[Int]] = Future {
        nums.map(num => { num + 1 })
      }.map {
        _.map(num => { num * 10 })
      }.map {
        _.map(num => { num + 1 })
      }

      whenReady(nums2, timeout(Span(10, Seconds))) { result =>
        assert(result == List(11, 21, 31, 41))
      }
    }

  }
}