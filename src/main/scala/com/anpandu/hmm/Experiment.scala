package com.anpandu.hmm

object Experiment extends App {

  override def main(args: Array[String]) = {
    val hmm: HMM = HMM.createFromModel(args(0))
    val total_words = hmm.sentences.flatten.length

    val count = hmm.sentences
      .par
      .map((tuples) => {
        val words = tuples.map(t => t(0))
        val labels = tuples.map(t => t(1))
        val preds = hmm.getTagSequence(words)
        println(words.zip(preds))
        val eq = labels.zip(preds)
          .map { case (l, p) => { if (l == p) 1 else 0 } }
          .foldLeft(0)((a, b) => { a + b })
        eq
      })
      .toList
      .foldLeft(0)((a, b) => { a + b })
    println("Matched     = " + count)
    println("Total Words = " + total_words)
    println("Precision   = " + count.toDouble / total_words.toDouble)
  }
}