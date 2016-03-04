package com.anpandu.hmm

import play.api.libs.json._
import scala.collection.immutable.{ Map, HashMap }
import scala.io.Source

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
    unigram.countTag(tag)
  }

  def countBiGramTag(tag: String, tag2: String): Int = {
    bigram.countTag(tag, tag2)
  }

  def countTriGramTag(tag: String, tag2: String, tag3: String): Int = {
    trigram.countTag(tag, tag2, tag3)
  }

  def countWordTag(word: String, tag: String): Int = {
    wordtag.countWordTag(word, tag)
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
    if (n == -1)
      (1.0, List())
    else {
      val valid_tags = if (n == 0) "_START_" :: tags else tags
      val result = valid_tags
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

  def getTagSequence(_words: List[String]): List[String] = {
    val words = preprocessWords(_words)
    val n = words.length - 1
    var candidate_tags: List[(String, String)] = List()
    tags.foreach((tag2) => {
      tags.foreach((tag3) => {
        val score = countBiGramTag(tag2, tag3).toDouble * emission(words(n), tag3)
        if (score > 0)
          candidate_tags = candidate_tags :+ (tag2, tag3)
      })
    })
    var result = candidate_tags
      .map((tup) => { pi(n, words, tup._1, tup._2) })
      .maxBy((tup) => { tup._1 })
      ._2
    result
  }

  def preprocessWords(_words: List[String]): List[String] = {
    val words = _words
      .zipWithIndex
      .map((e) => {
        val (word, idx) = e
        val result = if ((dict getOrElse (word, -1)) < threshold) HMMFactory.transformWord(word, idx) else word
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

object HMMFactory {

  def createFromModel(_path: String) = {
    val source = Source.fromFile(_path)
    val content = source.getLines.toList.mkString
    source.close()

    var json = Json.parse(content)
    var dict = (json \ "dict").as[Map[String, Int]]
    var sentences = (json \ "sentences").as[List[List[List[String]]]]
    var tags = (json \ "tags").as[List[String]]
    var threshold = (json \ "threshold").as[Int]
    var unigram = UniGramModelFactory.createFromJSON(Json.stringify((json \ "unigram")))
    var bigram = BiGramModelFactory.createFromJSON(Json.stringify((json \ "bigram")))
    var trigram = TriGramModelFactory.createFromJSON(Json.stringify((json \ "trigram")))
    var wordtag = WordTagModelFactory.createFromJSON(Json.stringify((json \ "wordtag")))
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
    var unigram = UniGramModelFactory.create(_sentences)
    var bigram = BiGramModelFactory.create(_sentences)
    var trigram = TriGramModelFactory.create(_sentences)
    var wordtag = WordTagModelFactory.create(Json.stringify(Json.toJson(sentences)))
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