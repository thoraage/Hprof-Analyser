package com.arktekk.hprofreader

import com.sun.tools.hat.internal.model.JavaLazyReadObject

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

object Analyser {

  class RichEnumeration[T](enumeration: java.util.Enumeration[T]) extends Iterator[T] {
    def hasNext: Boolean = enumeration.hasMoreElements()
    def next: T = enumeration.nextElement()
  }

  object BreakOffReason extends Enumeration {
    val InfiniteLoop = Value("InfiniteLoop")
    val NoReferers = Value("NoReferers")
    val MaxDepthExceeded = Value("MaxDepthExceeded")
  }

  case class InstanceStack(stack: List[JavaLazyReadObject], reason: BreakOffReason.Value) {
    override def toString = reason + ": " + stack.foldRight("") {(obj, string) => obj.getClazz.getName + "(" + obj.getId + ")" + (if (string.length == 0) "" else " -> " + string)}
  }

  def checkReferences(obj: JavaLazyReadObject, depth: Int, visited: List[JavaLazyReadObject]): List[InstanceStack] = {
    val newVisited = obj :: visited
    if (!visited.forall {_.getId != obj.getId}) {
      new InstanceStack(newVisited, BreakOffReason.InfiniteLoop) :: Nil
    } else if (depth > 50) {
      new InstanceStack(newVisited, BreakOffReason.MaxDepthExceeded) :: Nil
    } else {
      val referers = new RichEnumeration(obj.getReferers)
      if (referers.hasNext) {
        println("class: " + obj.getClazz.getName + ", id: " + obj.getId + ", size: " + newVisited.size)
        referers.foldRight(List[InstanceStack]()) {
          (ref, instanceStacks) =>
            checkReferences(ref.asInstanceOf[JavaLazyReadObject], depth + 1, newVisited) ::: instanceStacks
        }
      } else {
        new InstanceStack(newVisited, BreakOffReason.NoReferers) :: Nil
      }
    }
  }

  def checkReferencesAndPrintResult(obj: JavaLazyReadObject): Unit = {
    checkReferences(obj, 0, Nil).foreach { instanceStack => println(obj.getClazz.getName + ": " + instanceStack) }
  }
  
}