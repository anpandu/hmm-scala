HMM-Scala
=========

Implementation of Hidden Markov Model in Scala

## Train

```scala
import com.anpandu.hmm._

// sentences to be trained
val sentences = 
"""
[
  [
    ["Saya", "PRP"],
    ["terkena", "VBT"],
    ["bisa", "NN"],
    ["ular", "NN"],
    [".", "."]
  ],
  [
    ["Bisa", "NN"],
    ["ular", "NN"],
    ["bisa", "MD"],
    ["membunuh", "VBT"],
    ["orang", "NN"],
    [".", "."]
  ],
  ...
]
"""

// train model
val hmm: HMM = HMM.create(sentences)

// save to JSON
println(hmm.export)

// load
val hmm_load: HMM = HMM.createFromModel("/model.json")
```

## Usage

```scala
// load
val hmm_load: HMM = HMM.createFromModel("/model.json")

// get tag sequence
val words = List("Kamu", "bisa", "tidur", ".")
val tags = hmm.getTagSequence(words2)
println(tags) // List("PRP", "MD", "VBI", ".")
```

## License

MIT © [Pandu](pandu.ml)