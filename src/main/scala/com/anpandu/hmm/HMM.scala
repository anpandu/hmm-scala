package com.anpandu.hmm

import play.api.libs.json._
import scala.collection.mutable.{ Map, SynchronizedMap, HashMap }

class HMM(val sentences: List[List[List[String]]],
    val tags: List[String],
    val dict: Map[String, Int],
    val unigram: UniGramModel,
    val bigram: BiGramModel,
    val trigram: TriGramModel,
    val wordtag: WordTagModel) {

}

object HMMFactory {

  def create(_sentences: String): HMM = {
    var dict = getDict(_sentences)
    var sentences = getSentences(_sentences, dict, 2)
    var tags = getTags(sentences)
    var unigram = UniGramModelFactory.create(_sentences)
    var bigram = BiGramModelFactory.create(_sentences)
    var trigram = TriGramModelFactory.create(_sentences)
    var wordtag = WordTagModelFactory.create(_sentences)
    new HMM(sentences, tags, dict, unigram, bigram, trigram, wordtag)
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