package com.anpandu.hmm.word

import play.api.libs.json._
import scala.collection.immutable.{ Map, HashMap }

class Memory(val memory: Map[String, Int]) {

  def get(index: String): Int = {
    memory getOrElse (index, 0)
  }

  def toJSON(): String = {
    Json.stringify(Json.toJson(memory.toMap))
  }
}