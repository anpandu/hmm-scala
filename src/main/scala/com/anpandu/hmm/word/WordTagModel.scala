package com.anpandu.hmm.word

import play.api.libs.json._
import scala.collection.immutable.{ Map, HashMap }

class WordTagModel(override val memory: Map[String, Int]) extends Memory(memory) {

  def count(word: String, tag: String): Int = {
    val index = word + "_" + tag
    get(index)
  }
}

object WordTagModel {

  def create(_sentences: String): WordTagModel = {
    var sentences = Json.parse(_sentences).as[List[List[List[String]]]]
    var tags = getTags(sentences)
    var memory = getMemory(sentences, tags)
    new WordTagModel(memory)
  }

  def createFromJSON(_json: String): WordTagModel = {
    var memory = Json.parse(_json).as[Map[String, Int]]
    new WordTagModel(memory)
  }

  def getTags(sentences: List[List[List[String]]]): List[String] = {
    sentences
      .flatten
      .map((token) => { token(1) })
      .distinct
  }

  def getMemory(sentences: List[List[List[String]]], tags: List[String]): HashMap[String, Int] = {
    var memory = new HashMap[String, Int]
    val words = sentences
      .flatten
      .map((tuple) => {
        tuple(0)
      })
      .distinct
    words.foreach((word) => {
      tags.foreach((tag) => {
        val index = word + "_" + tag
        val cwt = count(sentences, word, tag)
        if (cwt > 0)
          memory += (index -> cwt)
      })
    })
    memory
  }

  def count(sentences: List[List[List[String]]], word: String, tag: String): Int = {
    sentences
      .flatten
      .filter((token) => { (token(0) == word && token(1) == tag) })
      .length
  }
}