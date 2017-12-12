package nl.gn0s1s.julius

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

object ExtendedList {
  implicit def listToListWithReplaceSlice[A](l: List[A]): ListWithReplaceSlice[A] = new ListWithReplaceSlice(l)

  class ListWithReplaceSlice[A](val source: List[A]) {
    def replaceSlice(target: List[A], replacement: List[A]): List[A] = {
      if ((source containsSlice target) && target != replacement && target.nonEmpty) {
        val (left, right) = source.splitAt(source.indexOfSlice(target))
        left ::: replacement ::: right.drop(target.length).replaceSlice(target, replacement)
      }
      else source
    }

    @tailrec final def substitute(substitutes: ListMap[List[A], List[A]]): List[A] = {
      if (substitutes.isEmpty) source
      else source.replaceSlice(substitutes.head._1, substitutes.head._2).substitute(substitutes.tail)
    }
  }
}
