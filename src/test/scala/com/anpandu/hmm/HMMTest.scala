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
      val ex_new_sentences = List(List(List("_firstWord_", "PRP"), List("_lowerCase_", "VBT"), List("bisa", "NN"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "NN"), List("_lowerCase_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBT"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD")), List(List("_firstWord_", "NNP"), List("_lowerCase_", "CC"), List("_initCap_", "NNP"), List("_lowerCase_", "VBI"), List("_lowerCase_", "IN"), List("_lowerCase_", "NN"), List(".", ".")))

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

    it("createFromCorpus") {
      val ex_tags = List("PRP", "VBT", "NN", ".", "MD", "VBI", "NNP", "CC", "IN")
      val ex_dict = Map("bisa" -> 5, "Budi" -> 1, "Saya" -> 1, "di" -> 1, "Bisa" -> 1, "ular" -> 2, "duduk" -> 1, "." -> 5, "tidur" -> 1, "bangku" -> 1, "membunuh" -> 1, "terbang" -> 1, "dan" -> 1, "Burung" -> 1, "orang" -> 1, "Kamu" -> 2, "terkena" -> 1, "Rani" -> 1)
      val ex_new_sentences = List(List(List("_firstWord_", "PRP"), List("_lowerCase_", "VBT"), List("bisa", "NN"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "NN"), List("_lowerCase_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBT"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD")), List(List("_firstWord_", "NNP"), List("_lowerCase_", "CC"), List("_initCap_", "NNP"), List("_lowerCase_", "VBI"), List("_lowerCase_", "IN"), List("_lowerCase_", "NN"), List(".", ".")))

      val path = getClass.getResource("/corpus.crp.json").getFile
      val hmm: HMM = HMMFactory.createFromCorpus(path)
      assert(hmm.tags == ex_tags)
      assert(hmm.dict == ex_dict)
      assert(hmm.sentences == ex_new_sentences)
    }

    it("export + import") {
      val ex_tags = List("PRP", "VBT", "NN", ".", "MD", "VBI", "NNP", "CC", "IN")
      val ex_dict = Map("bisa" -> 5, "Budi" -> 1, "Saya" -> 1, "di" -> 1, "Bisa" -> 1, "ular" -> 2, "duduk" -> 1, "." -> 5, "tidur" -> 1, "bangku" -> 1, "membunuh" -> 1, "terbang" -> 1, "dan" -> 1, "Burung" -> 1, "orang" -> 1, "Kamu" -> 2, "terkena" -> 1, "Rani" -> 1)
      val ex_new_sentences = List(List(List("_firstWord_", "PRP"), List("_lowerCase_", "VBT"), List("bisa", "NN"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "NN"), List("_lowerCase_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBT"), List("_lowerCase_", "NN"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "NN"), List("bisa", "MD"), List("_lowerCase_", "VBI"), List(".", ".")), List(List("_firstWord_", "PRP"), List("bisa", "MD")), List(List("_firstWord_", "NNP"), List("_lowerCase_", "CC"), List("_initCap_", "NNP"), List("_lowerCase_", "VBI"), List("_lowerCase_", "IN"), List("_lowerCase_", "NN"), List(".", ".")))

      val ex_export = """{"sentences":[[["_firstWord_","PRP"],["_lowerCase_","VBT"],["bisa","NN"],["_lowerCase_","NN"],[".","."]],[["_firstWord_","NN"],["_lowerCase_","NN"],["bisa","MD"],["_lowerCase_","VBT"],["_lowerCase_","NN"],[".","."]],[["_firstWord_","PRP"],["bisa","MD"],["_lowerCase_","VBI"],[".","."]],[["_firstWord_","NN"],["bisa","MD"],["_lowerCase_","VBI"],[".","."]],[["_firstWord_","PRP"],["bisa","MD"]],[["_firstWord_","NNP"],["_lowerCase_","CC"],["_initCap_","NNP"],["_lowerCase_","VBI"],["_lowerCase_","IN"],["_lowerCase_","NN"],[".","."]]],"tags":["PRP","VBT","NN",".","MD","VBI","NNP","CC","IN"],"dict":{"duduk":1,"tidur":1,"orang":1,"Bisa":1,"ular":2,".":5,"Budi":1,"bisa":5,"dan":1,"terbang":1,"di":1,"Kamu":2,"membunuh":1,"Burung":1,"terkena":1,"Rani":1,"Saya":1,"bangku":1},"unigram":{"IN":1,"PRP":3,".":5,"NN":7,"MD":4,"_START_":6,"CC":1,"VBT":2,"NNP":2,"VBI":3},"bigram":{"CC_NNP":1,"NN_NN":2,"_START__NN":2,"MD_VBI":2,"PRP_MD":2,"VBT_NN":2,"NNP_VBI":1,"_START__NNP":1,"NN_MD":2,"_START___START_":6,"NN_.":3,"VBI_.":2,"VBI_IN":1,"IN_NN":1,"_START__PRP":3,"PRP_VBT":1,"MD_VBT":1,"NNP_CC":1},"trigram":{"_START___START__NNP":1,"NN_MD_VBI":1,"_START___START__PRP":3,"CC_NNP_VBI":1,"NN_NN_.":1,"_START__NN_NN":1,"MD_VBT_NN":1,"_START__PRP_MD":2,"_START__NNP_CC":1,"PRP_VBT_NN":1,"NNP_VBI_IN":1,"VBT_NN_NN":1,"VBI_IN_NN":1,"_START__PRP_VBT":1,"IN_NN_.":1,"MD_VBI_.":2,"_START__NN_MD":1,"NN_NN_MD":1,"PRP_MD_VBI":1,"_START___START__NN":2,"NN_MD_VBT":1,"VBT_NN_.":1,"NNP_CC_NNP":1},"wordtag":{"._.":5,"_lowerCase__NN":4,"bisa_NN":1,"_lowerCase__VBI":3,"bisa_MD":4,"_firstWord__PRP":3,"_firstWord__NNP":1,"_lowerCase__VBT":2,"_initCap__NNP":1,"_firstWord__NN":2,"_lowerCase__CC":1,"_lowerCase__IN":1}}"""
      val path = getClass.getResource("/corpus.crp.json").getFile
      val hmm: HMM = HMMFactory.createFromCorpus(path)
      assert(hmm.export == ex_export)
      assert(hmm.tags == ex_tags)
      assert(hmm.dict == ex_dict)
      assert(hmm.sentences == ex_new_sentences)

      val path2 = getClass.getResource("/corpus.hmm.json").getFile
      val hmm2: HMM = HMMFactory.createFromModel(path2)
      assert(hmm2.export == hmm.export)
      assert(hmm2.tags == ex_tags)
      assert(hmm2.dict == ex_dict)
      assert(hmm2.sentences == ex_new_sentences)
    }

    it("countTag, countWordTag") {
      val path = getClass.getResource("/corpus2.hmm.json").getFile
      val hmm: HMM = HMMFactory.createFromModel(path)
      assert(hmm.countUniGramTag("NN") == 35)
      assert(hmm.countBiGramTag("NN", "DT") == 7)
      assert(hmm.countTriGramTag("_START_", "NN", "DT") == 2)
      assert(hmm.countWordTag("bisa", "MD") == 4)
      assert(hmm.countWordTag("di", "IN") == 5)
      assert(hmm.countUniGramTag("IN") == 7)
    }

    it("emission, q") {
      val path = getClass.getResource("/corpus2.hmm.json").getFile
      val hmm: HMM = HMMFactory.createFromModel(path)

      assert(hmm.emission("di", "IN") == 0.7142857142857143)
      assert(hmm.q("VBI", "IN", "NN") == 0.5692186266771901)
      assert(hmm.q("_START_", "_START_", "NN") == 0.3403187908808027)
    }

    it("pi, getTagSequence") {
      val path = getClass.getResource("/corpus2.hmm.json").getFile
      val hmm: HMM = HMMFactory.createFromModel(path)

      val words = HMMFactory.preprocessWords(List("Kamu", "bisa", "tidur", "."), hmm.dict)
      assert(hmm.pi(-1, words, "", "") == (1.0, List()))
      assert(hmm.pi(0, words, "_START_", "PRP") == (0.08104718359052518, List("PRP")))
      assert(hmm.pi(1, words, "PRP", "MD") == (0.07688195553769328, List("PRP", "MD")))
      assert(hmm.pi(2, words, "MD", "VBI") == (0.04042320130020522, List("PRP", "MD", "VBI")))
      assert(hmm.pi(3, words, "VBI", ".") == (0.021723771408808535, List("PRP", "MD", "VBI", ".")))

      val ori_words = List("Kamu", "bisa", "tidur", ".")
      assert(hmm.getTagSequence(ori_words) == List("PRP", "MD", "VBI", "."))
    }
  }
}