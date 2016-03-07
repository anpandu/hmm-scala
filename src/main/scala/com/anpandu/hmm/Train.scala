package com.anpandu.hmm

object Train extends App {
  override def main(args: Array[String]) = {
    val hmm: HMM = HMM.createFromCorpus(args(0))
    println(hmm.export)
  }
}