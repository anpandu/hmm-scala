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

class BiGramModelTest extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("BiGramModel") {

    it("Basic") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val ex_memory = Map("NN_NN" -> 2, "MD_VBI" -> 2, "VBI_IN" -> 1, "NNP_CC" -> 1, "_START__NN" -> 2, "NNP_VBI" -> 1, "VBI_." -> 2, "_START___START_" -> 6, "MD_VBT" -> 1, "_START__PRP" -> 3, "_START__NNP" -> 1, "PRP_VBT" -> 1, "PRP_MD" -> 2, "VBT_NN" -> 2, "IN_NN" -> 1, "NN_." -> 3, "NN_MD" -> 2, "CC_NNP" -> 1)
      val ex_memory_json = """{"CC_NNP":1,"NN_NN":2,"_START__NN":2,"MD_VBI":2,"PRP_MD":2,"VBT_NN":2,"NNP_VBI":1,"_START__NNP":1,"NN_MD":2,"_START___START_":6,"NN_.":3,"VBI_.":2,"VBI_IN":1,"IN_NN":1,"_START__PRP":3,"PRP_VBT":1,"MD_VBT":1,"NNP_CC":1}"""

      val bigram: BiGramModel = BiGramModelFactory.create(ex_sentences)
      assert(bigram.memory == ex_memory)
      assert(bigram.toJSON() == ex_memory_json)

      val bigram2: BiGramModel = BiGramModelFactory.createFromJSON(ex_memory_json)
      assert(bigram2.memory == ex_memory)
      assert(bigram2.toJSON() == ex_memory_json)
    }

    it("countTag") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val bigram: BiGramModel = BiGramModelFactory.create(ex_sentences)
      assert(bigram.countTag("NN", "NN") == 2)
      assert(bigram.countTag("NN", "WRONGXXX") == 0)
      assert(bigram.countTag("PRP", "PRP") == 0)
      assert(bigram.countTag("_START_", "NN") == 2)
      assert(bigram.countTag("_START_", "_START_") == 6)
    }
  }
}