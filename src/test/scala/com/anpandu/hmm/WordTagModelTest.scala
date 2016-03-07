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

class WordTagModelTest extends FunSpec with ScalaFutures with TimeLimitedTests {

  val timeLimit = 20000 millis

  describe("WordTagModel") {

    it("Basic") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val ex_memory = Map("di_IN" -> 1, "membunuh_VBT" -> 1, "tidur_VBI" -> 1, "._." -> 5, "Kamu_PRP" -> 2, "duduk_VBI" -> 1, "bisa_NN" -> 1, "orang_NN" -> 1, "terbang_VBI" -> 1, "Budi_NNP" -> 1, "Rani_NNP" -> 1, "ular_NN" -> 2, "bangku_NN" -> 1, "Bisa_NN" -> 1, "Burung_NN" -> 1, "terkena_VBT" -> 1, "bisa_MD" -> 4, "dan_CC" -> 1, "Saya_PRP" -> 1)
      val ex_memory_json = """{"._.":5,"Kamu_PRP":2,"bisa_NN":1,"bisa_MD":4,"tidur_VBI":1,"orang_NN":1,"Bisa_NN":1,"Budi_NNP":1,"bangku_NN":1,"Saya_PRP":1,"Burung_NN":1,"membunuh_VBT":1,"dan_CC":1,"ular_NN":2,"di_IN":1,"terkena_VBT":1,"terbang_VBI":1,"Rani_NNP":1,"duduk_VBI":1}"""

      val wordtag: WordTagModel = WordTagModel.create(ex_sentences)
      assert(wordtag.memory == ex_memory)
      assert(wordtag.toJSON() == ex_memory_json)

      val wordtag2: WordTagModel = WordTagModel.createFromJSON(ex_memory_json)
      assert(wordtag2.memory == ex_memory)
      assert(wordtag2.toJSON() == ex_memory_json)
    }

    it("countWordTag") {
      val ex_sentences = """[[["Saya","PRP"],["terkena","VBT"],["bisa","NN"],["ular","NN"],[".","."]],[["Bisa","NN"],["ular","NN"],["bisa","MD"],["membunuh","VBT"],["orang","NN"],[".","."]],[["Kamu","PRP"],["bisa","MD"],["tidur","VBI"],[".","."]],[["Burung","NN"],["bisa","MD"],["terbang","VBI"],[".","."]],[["Kamu","PRP"],["bisa","MD"]],[["Rani","NNP"],["dan","CC"],["Budi","NNP"],["duduk","VBI"],["di","IN"],["bangku","NN"],[".","."]]]"""
      val wordtag: WordTagModel = WordTagModel.create(ex_sentences)
      assert(wordtag.countWordTag(".", ".") == 5)
      assert(wordtag.countWordTag("Kamu", "PRP") == 2)
      assert(wordtag.countWordTag("Kamu", "NN") == 0)
      assert(wordtag.countWordTag(".", "WRONGXXX") == 0)
    }
  }
}