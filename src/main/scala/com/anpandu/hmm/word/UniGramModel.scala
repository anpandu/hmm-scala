package com.anpandu.hmm.word

import play.api.libs.json._
import scala.collection.immutable.{ Map, HashMap }

class UniGramModel(val memory: Map[String, Int]) {

  def countTag(tag: String): Int = {
    memory getOrElse (tag, 0)
  }

  def toJSON(): String = {
    Json.stringify(Json.toJson(memory.toMap))
  }
}

object UniGramModel {

  def create(_sentences: String): UniGramModel = {
    var sentences = Json.parse(_sentences).as[List[List[List[String]]]]
    var tags = getTags(sentences)
    var memory = getMemory(sentences, tags)
    new UniGramModel(memory)
  }

  def createFromJSON(_json: String): UniGramModel = {
    var memory = Json.parse(_json).as[Map[String, Int]]
    new UniGramModel(memory)
  }

  def getTags(sentences: List[List[List[String]]]): List[String] = {
    sentences
      .flatten
      .map((token) => { token(1) })
      .distinct
  }

  def getMemory(sentences: List[List[List[String]]], tags: List[String]): HashMap[String, Int] = {
    var memory = new HashMap[String, Int]
    var new_tags = tags :+ "_START_"
    new_tags.foreach((tag) => { memory += (tag -> countTag(sentences, tag)) })
    memory
  }

  def countTag(sentences: List[List[List[String]]], tag: String): Int = {
    if (tag == "_START_" || tag == "_STOP_")
      return sentences.length
    else
      return sentences
        .flatten
        .filter((token) => { (token(1) == tag) })
        .length
  }
}