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

class TriGramModelTest extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("TriGramModel") {

    it("Basic") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val ex_tags = List("PRP", "VBT", "NN", ".", "MD", "VBI", "NNP", "CC", "IN")
      val ex_memory = Map("CC_NNP_VBI" -> 1, "VBT_NN_." -> 1, "MD_VBI_." -> 2, "_START___START__NN" -> 2, "_START__NN_MD" -> 1, "NN_MD_VBI" -> 1, "_START__PRP_VBT" -> 1, "IN_NN_." -> 1, "PRP_VBT_NN" -> 1, "_START__NNP_CC" -> 1, "NN_MD_VBT" -> 1, "_START__NN_NN" -> 1, "NN_NN_MD" -> 1, "MD_VBT_NN" -> 1, "NNP_CC_NNP" -> 1, "_START___START__NNP" -> 1, "_START___START__PRP" -> 3, "NNP_VBI_IN" -> 1, "VBI_IN_NN" -> 1, "_START__PRP_MD" -> 2, "NN_NN_." -> 1, "PRP_MD_VBI" -> 1, "VBT_NN_NN" -> 1)
      val ex_memory_json = """{"_START___START__NNP":1,"NN_MD_VBI":1,"_START___START__PRP":3,"CC_NNP_VBI":1,"NN_NN_.":1,"_START__NN_NN":1,"MD_VBT_NN":1,"_START__PRP_MD":2,"_START__NNP_CC":1,"PRP_VBT_NN":1,"NNP_VBI_IN":1,"VBT_NN_NN":1,"VBI_IN_NN":1,"_START__PRP_VBT":1,"IN_NN_.":1,"MD_VBI_.":2,"_START__NN_MD":1,"NN_NN_MD":1,"PRP_MD_VBI":1,"_START___START__NN":2,"NN_MD_VBT":1,"VBT_NN_.":1,"NNP_CC_NNP":1}"""

      val trigram: TriGramModel = TriGramModelFactory.create(ex_sentences)
      assert(trigram.tags == ex_tags)
      assert(trigram.memory == ex_memory)
      assert(trigram.toJSON() == ex_memory_json)
    }

    it("countTag") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val trigram: TriGramModel = TriGramModelFactory.create(ex_sentences)
      assert(trigram.countTag("NN", "NN", "MD") == 1)
      assert(trigram.countTag("NN", "NN", "WRONGXXX") == 0)
      assert(trigram.countTag("PRP", "PRP", "PRP") == 0)
      assert(trigram.countTag("_START_", "_START_", "NN") == 2)
    }
  }
}