package com.gvolpe.demo

import shapeless._

object Merge extends App {

  implicit class MergeSyntax[T](t: T) {
    def merge[U](u: U)(implicit merge: CaseClassMerge[T, U]): T = merge(t, u)
  }

  case class LegacyUser(id: Long, email: String)
  case class User(id: Long, username: String, email: String, active: Boolean)

  val legacyUser  = LegacyUser(32L, "gvolpe@github.com")
  val user        = User(32L, "gvolpe", "", true)

  val merged      = user merge legacyUser

  assert(merged == User(32L, "gvolpe", "gvolpe@github.com", true))
}

trait CaseClassMerge[T, U] {
  def apply(t: T, u: U): T
}

object CaseClassMerge {
  import ops.record.Merger

  def apply[T, U](implicit merge: CaseClassMerge[T, U]): CaseClassMerge[T, U] = merge

  implicit def mkCCMerge[T, U, RT <: HList, RU <: HList]
    (implicit
     tgen: LabelledGeneric.Aux[T, RT],
     ugen: LabelledGeneric.Aux[U, RU],
     merger: Merger.Aux[RT, RU, RT]
    ): CaseClassMerge[T, U] =
      new CaseClassMerge[T, U] {
        def apply(t: T, u: U): T =
          tgen.from(merger(tgen.to(t), ugen.to(u)))
      }
}