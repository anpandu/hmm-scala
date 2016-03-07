package com.anpandu.hmm

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

import play.api.libs.json._

class UniGramModelTest extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("UniGramModel") {

    it("Basic") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val ex_memory = Map("IN" -> 1, "PRP" -> 3, "." -> 5, "NN" -> 7, "MD" -> 4, "_START_" -> 6, "CC" -> 1, "VBT" -> 2, "NNP" -> 2, "VBI" -> 3)
      val ex_memory_json = """{"IN":1,"PRP":3,".":5,"NN":7,"MD":4,"_START_":6,"CC":1,"VBT":2,"NNP":2,"VBI":3}"""

      val unigram: UniGramModel = UniGramModel.create(ex_sentences)
      assert(unigram.memory == ex_memory)
      assert(unigram.toJSON() == ex_memory_json)

      val unigram2: UniGramModel = UniGramModel.createFromJSON(ex_memory_json)
      assert(unigram2.memory == ex_memory)
      assert(unigram2.toJSON() == ex_memory_json)
    }

    it("countTag") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val unigram: UniGramModel = UniGramModel.create(ex_sentences)
      assert(unigram.countTag("NN") == 7)
      assert(unigram.countTag("WRONGXXX") == 0)
    }
  }
}