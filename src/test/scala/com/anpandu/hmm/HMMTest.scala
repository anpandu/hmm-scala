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

class HMMTest extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("HMM") {

    it("Basic") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val ex_tags = List("PRP", "VBT", "NN", ".", "MD", "VBI", "NNP", "CC", "IN")
      val ex_dict = Map("bisa" -> 5, "Budi" -> 1, "Saya" -> 1, "di" -> 1, "Bisa" -> 1, "ular" -> 2, "duduk" -> 1, "." -> 5, "tidur" -> 1, "bangku" -> 1, "membunuh" -> 1, "terbang" -> 1, "dan" -> 1, "Burung" -> 1, "orang" -> 1, "Kamu" -> 2, "terkena" -> 1, "Rani" -> 1)
      val ex_new_sentences = List(List(List("_firstWord_", "PRP"), List("_lowerCase_", "VBT"), List("bisa", "NN"), List("ular", "NN"), List(".", ".")), List(List("_firstWord_", "NN"), List("ular", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBT"), List("_lowerCase_", "NN"), List(".", ".")), List(List("Kamu", "PRP"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("Kamu", "PRP"), List("bisa", "MD")), List(List("_firstWord_", "NNP"), List("_lowerCase_", "CC"), List("_initCap_", "NNP"), List("_lowerCase_", "VBI"), List("_lowerCase_", "IN"), List("_lowerCase_", "NN"), List(".", ".")))

      val hmm: HMM = HMMFactory.create(ex_sentences)
      assert(hmm.tags == ex_tags)
      assert(hmm.dict == ex_dict)
      assert(hmm.sentences == ex_new_sentences)
    }

    it("transformWord") {
      assert("_twoDigitNum_" == HMMFactory.transformWord("69"))
      assert("_fourDigitNum_" == HMMFactory.transformWord("1999"))
      assert("_digitPeriod_" == HMMFactory.transformWord("1.000"))
      assert("_digitPeriod_" == HMMFactory.transformWord("1.000.000"))
      assert("_otherNum_" == HMMFactory.transformWord("123123"))
      assert("_allCaps_" == HMMFactory.transformWord("BBC"))
      assert("_capPeriod_" == HMMFactory.transformWord("M."))
      assert("_initCap_" == HMMFactory.transformWord("Sally"))
      assert("_lowerCase_" == HMMFactory.transformWord("can"))
      assert("_firstWord_" == HMMFactory.transformWord("Pertamax", 0))
      assert("_other_" == HMMFactory.transformWord(",", 1))
    }

    it("createFromFile") {
      val ex_tags = List("PRP", "VBT", "NN", ".", "MD", "VBI", "NNP", "CC", "IN")
      val ex_dict = Map("bisa" -> 5, "Budi" -> 1, "Saya" -> 1, "di" -> 1, "Bisa" -> 1, "ular" -> 2, "duduk" -> 1, "." -> 5, "tidur" -> 1, "bangku" -> 1, "membunuh" -> 1, "terbang" -> 1, "dan" -> 1, "Burung" -> 1, "orang" -> 1, "Kamu" -> 2, "terkena" -> 1, "Rani" -> 1)
      val ex_new_sentences = List(List(List("_firstWord_", "PRP"), List("_lowerCase_", "VBT"), List("bisa", "NN"), List("ular", "NN"), List(".", ".")), List(List("_firstWord_", "NN"), List("ular", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBT"), List("_lowerCase_", "NN"), List(".", ".")), List(List("Kamu", "PRP"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("Kamu", "PRP"), List("bisa", "MD")), List(List("_firstWord_", "NNP"), List("_lowerCase_", "CC"), List("_initCap_", "NNP"), List("_lowerCase_", "VBI"), List("_lowerCase_", "IN"), List("_lowerCase_", "NN"), List(".", ".")))

      val path = getClass.getResource("/corpus.crp.json").getFile
      val hmm: HMM = HMMFactory.createFromFile(path)
      assert(hmm.tags == ex_tags)
      assert(hmm.dict == ex_dict)
      assert(hmm.sentences == ex_new_sentences)
    }

    it("countTag, countWordTag, emission") {
      val path = getClass.getResource("/corpus2.crp.json").getFile
      val hmm: HMM = HMMFactory.createFromFile(path)
      assert(hmm.countUniGramTag("NN") == 35)
      assert(hmm.countBiGramTag("NN", "DT") == 7)
      assert(hmm.countTriGramTag("_START_", "NN", "DT") == 2)
      assert(hmm.countWordTag("bisa", "MD") == 4)
      assert(hmm.countWordTag("di", "IN") == 5)
      assert(hmm.countUniGramTag("IN") == 7)
      assert(hmm.emission("di", "IN") == 5 / 7)
    }
  }
}