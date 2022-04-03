package u05lab.ex2

import scala.collection.mutable

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List()

  def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    if scores.size < Question.values.length then throw new IllegalArgumentException
    reviews = reviews :+ (article, scores)

  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val map: Map[Question, Int] = Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin
    )
    loadReview(article, map)


  def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter((id, _) => id == article)
      .map((_, m) => m(question))
      .sorted

  def averageFinalScore(article: Int): Double =
    val l: List[Double] = reviews.filter((id, _) => id == article).map((_, m) => m(Question.FINAL))
    l.sum / l.length

  private def accepted(article: Int): Boolean =
    averageFinalScore(article) > 5.0 && !reviews.filter((id, _) => id == article)
              .flatMap((_, m) => m.toSet)
              .filter((k,v) => k == Question.RELEVANCE && v >= 8)
              .isEmpty


  def acceptedArticles: Set[Int] =
    reviews.map((id, _) => id)
      .distinct
      .filter(accepted)
      .toSet

  def sortedAcceptedArticles: List[(Int, Double)] =
    acceptedArticles.map(a => (a, averageFinalScore(a)))
      .toList
      .sortBy((a,avr) => avr)

  private def averageWeightedFinalScore(article: Int): Double =
    val tmp = reviews.filter((id, _) => id == article).map((id, m) => m(Question.FINAL) * m(Question.CONFIDENCE)/10.0)
    tmp.sum / tmp.length

  def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map((id,_) => id)
      .distinct
      .map(i => i -> averageWeightedFinalScore(i))
      .toMap

