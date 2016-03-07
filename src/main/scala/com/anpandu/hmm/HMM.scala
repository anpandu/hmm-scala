package com.anpandu.hmm

import com.anpandu.hmm.word._
import play.api.libs.json._
import scala.collection.immutable.{ Map, HashMap }
import scala.io.Source
import scala.util.control.Exception._

class HMM(val sentences: List[List[List[String]]],
    val tags: List[String],
    val dict: Map[String, Int],
    val threshold: Int,
    val unigram: UniGramModel,
    val bigram: BiGramModel,
    val trigram: TriGramModel,
    val wordtag: WordTagModel) {

  val sentences_length = sentences.flatten.length.toDouble

  def countUniGramTag(tag: String): Int = {
    unigram.count(tag)
  }

  def countBiGramTag(tag: String, tag2: String): Int = {
    bigram.count(tag, tag2)
  }

  def countTriGramTag(tag: String, tag2: String, tag3: String): Int = {
    trigram.count(tag, tag2, tag3)
  }

  def countWordTag(word: String, tag: String): Int = {
    wordtag.count(word, tag)
  }

  def emission(word: String, tag: String): Double = {
    countWordTag(word, tag).toDouble / countUniGramTag(tag).toDouble
  }

  def q(tag1: String, tag2: String, tag3: String): Double = {
    val x: Double = 1.toDouble / 3.toDouble
    val y: Double = 1.toDouble / 3.toDouble
    val z: Double = 1.toDouble / 3.toDouble

    val ct123 = countTriGramTag(tag1, tag2, tag3).toDouble
    val ct12 = countBiGramTag(tag1, tag2).toDouble
    val ct23 = countBiGramTag(tag2, tag3).toDouble
    val ct2 = countUniGramTag(tag2).toDouble
    val ct3 = countUniGramTag(tag3).toDouble
    val s = sentences_length

    val a: Double = if (ct12 == 0) 0 else ct123 / ct12
    val b: Double = if (ct2 == 0) 0 else ct23 / ct2
    val c: Double = if (s == 0) 0 else ct3 / s

    x * a + y * b + z * c
  }

  def pi(n: Int, words: List[String], tag1: String, tag2: String): (Double, List[String]) = {
    n match {
      case -1 => (1.0, List())
      case _ => {
        var valid_tags = if (n == 0) "_START_" :: tags else tags
        valid_tags = pruneTags(valid_tags, n, tag1, tag2, words)
        valid_tags.length match {
          case 0 => (0.0, List("_xxx_"))
          case _ => {
            val result: (Double, List[String]) = valid_tags
              .map((tag0) => {
                val score = q(tag0, tag1, tag2) * emission(words(n), tag2)
                val pi_res = pi(n - 1, words, tag0, tag1)
                val fin_score = pi_res._1 * score
                val tag_seq = pi_res._2 :+ tag2
                (fin_score, tag_seq)
              })
              .maxBy(_._1)
            result
          }
        }
      }
    }
  }

  def pruneTags(_tags: List[String], n: Int, tag1: String, tag2: String, words: List[String]): List[String] = {
    val tup_tag_score = _tags
      .map((tag) => {
        val prev_score = n match {
          case x if x < 1 => 1
          case _ => tags
            .map((tag_p1) => {
              val prev_prev_score = n match {
                case x if x < 2 => 1
                case _ => tags
                  .map((tag_p2) => { q(tag_p2, tag_p1, tag) * emission(words(n - 2), tag) })
                  .max
              }
              q(tag_p1, tag, tag1) * emission(words(n - 1), tag1) * prev_prev_score
            })
            .max
        }
        val score = q(tag, tag1, tag2) * emission(words(n), tag2) * prev_score
        (tag, score)
      })
      .filter((tup) => { tup._2 > 0 })
    tup_tag_score.length match {
      case 0 => List()
      case _ => {
        val max_score = tup_tag_score
          .sortBy((tup) => { -tup._2 })
          .map((tup) => { tup._2 })
          .take(1)
        val valid_tags = tup_tag_score
          .filter((tup) => { max_score.contains(tup._2) })
          .map((tup) => { tup._1 })
        valid_tags
      }
    }
  }

  def getTagSequence(_words: List[String]): List[String] = {
    val words = preprocessWords(_words)
    val n = words.length - 1
    var candidate_tags: List[(String, String, Double)] = List()
    tags.foreach((tag1) => {
      tags.foreach((tag2) => {
        tags.foreach((tag3) => {
          val score = countBiGramTag(tag1, tag2).toDouble * emission(words(n - 1), tag2) * countBiGramTag(tag2, tag3).toDouble * emission(words(n), tag3)
          if (score > 0)
            candidate_tags = candidate_tags :+ (tag2, tag3, score)
        })
      })
    })
    if (candidate_tags.isEmpty)
      throw NoAnswerException("no candidate tags found")
    val best_tags = candidate_tags
      .sortBy((tup) => { -tup._3 })
      .map((tup) => { (tup._1, tup._2) })
      .distinct
      .take(5)
    val candidate_result = best_tags
      .map((tup) => { pi(n, words, tup._1, tup._2) })
    val result = candidate_result
      .maxBy((tup) => { tup._1 })
      ._2
    if (result.contains("_xxx_"))
      throw NoAnswerException("no tag sequence found")
    result
  }

  def preprocessWords(_words: List[String]): List[String] = {
    val words = _words
      .zipWithIndex
      .map((e) => {
        val (word, idx) = e
        val result = if ((dict getOrElse (word, -1)) < threshold) HMM.transformWord(word, idx) else word
        result
      })
    words
  }

  def export(): String = {
    val json: JsValue = JsObject(Seq(
      "sentences" -> Json.toJson(sentences),
      "tags" -> Json.toJson(tags),
      "dict" -> Json.toJson(dict.toMap),
      "threshold" -> Json.toJson(threshold),
      "unigram" -> Json.toJson(unigram.memory.toMap),
      "bigram" -> Json.toJson(bigram.memory.toMap),
      "trigram" -> Json.toJson(trigram.memory.toMap),
      "wordtag" -> Json.toJson(wordtag.memory.toMap)
    ))
    Json.stringify(json)
  }

}

object HMM {

  def createFromModel(_path: String) = {
    val source = Source.fromFile(_path)
    val content = source.getLines.toList.mkString
    source.close()

    var json = Json.parse(content)
    var dict = (json \ "dict").as[Map[String, Int]]
    var sentences = (json \ "sentences").as[List[List[List[String]]]]
    var tags = (json \ "tags").as[List[String]]
    var threshold = (json \ "threshold").as[Int]
    var unigram = UniGramModel.createFromJSON(Json.stringify((json \ "unigram")))
    var bigram = BiGramModel.createFromJSON(Json.stringify((json \ "bigram")))
    var trigram = TriGramModel.createFromJSON(Json.stringify((json \ "trigram")))
    var wordtag = WordTagModel.createFromJSON(Json.stringify((json \ "wordtag")))
    new HMM(sentences, tags, dict, threshold, unigram, bigram, trigram, wordtag)
  }

  def createFromCorpus(_path: String, _threshold: Int = 5) = {
    val source = Source.fromFile(_path)
    val content = source.getLines.toList.mkString
    source.close()
    create(content, _threshold)
  }

  def create(_sentences: String, _threshold: Int = 5): HMM = {
    var dict = getDict(_sentences)
    var sentences = getSentences(_sentences, dict, _threshold)
    var tags = getTags(sentences)
    var unigram = UniGramModel.create(_sentences)
    var bigram = BiGramModel.create(_sentences)
    var trigram = TriGramModel.create(_sentences)
    var wordtag = WordTagModel.create(Json.stringify(Json.toJson(sentences)))
    new HMM(sentences, tags, dict, _threshold, unigram, bigram, trigram, wordtag)
  }

  def getDict(_sentences: String): Map[String, Int] = {
    var dict = new HashMap[String, Int]
    var sentences = Json.parse(_sentences).as[List[List[List[String]]]]
    sentences
      .flatten
      .foreach((tuple) => {
        var word = tuple(0)
        if (dict contains word) {
          dict += (word -> (dict(word) + 1))
        } else
          dict += (word -> 1)
      })
    dict
  }

  def getSentences(_sentences: String, _dict: Map[String, Int], _threshold: Int): List[List[List[String]]] = {
    var sentences = Json.parse(_sentences).as[List[List[List[String]]]]
    sentences = sentences
      .map((tuples) => {
        tuples
          .zipWithIndex
          .map((e) => {
            val (tuple, idx) = e
            var word = if (_dict(tuple(0)) < _threshold) transformWord(tuple(0), idx) else tuple(0)
            List(word, tuple(1))
          })
      })
    sentences
  }

  def getTags(sentences: List[List[List[String]]]): List[String] = {
    sentences
      .flatten
      .map((token) => { token(1) })
      .distinct
  }

  def transformWord(_word: String, _idx: Int = 1): String = {
    if (_idx == 0)
      "_firstWord_"
    else {
      var found = false
      var answer = "_other_"
      var regex_dict = List(
        ("""^\d\d$""", "_twoDigitNum_"),
        ("""^\d\d\d\d$""", "_fourDigitNum_"),
        ("""^(\d)+\.(\d)+(\.(\d)+)*$""", "_digitPeriod_"),
        ("""^\d+$""", "_otherNum_"),
        ("""^[A-Z]+$""", "_allCaps_"),
        ("""^[A-Z]\.$""", "_capPeriod_"),
        ("""^[A-Z][A-Za-z]+$""", "_initCap_"),
        ("""^[a-z]+$""", "_lowerCase_")
      )
      regex_dict
        .foreach(re => {
          val (k, v) = re
          if (_word.matches(k) && !found) {
            answer = v
            found = true
          }
        })
      answer
    }
  }
}

case class NoAnswerException(message: String) extends Exception(message)
