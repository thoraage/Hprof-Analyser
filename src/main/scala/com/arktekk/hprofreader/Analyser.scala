package com.arktekk.hprofreader

import com.sun.tools.hat.internal.model.{JavaClass, JavaLazyReadObject}

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

object Analyser {
  object BreakOffReason extends Enumeration {
    val InfiniteLoop = Value("InfiniteLoop")
    val NoReferers = Value("NoReferers")
    val MaxDepthExceeded = Value("MaxDepthExceeded")
    val InInterestPath = Value("InInterestPath")
  }

  def toStringJavaObject(obj: JavaLazyReadObject): String = {
    obj.getClazz.getName + "(" + obj.getId + ")"
  }
  case class InstanceStack(stack: List[JavaLazyReadObject], reason: BreakOffReason.Value) {
    override def toString = reason + ": " + stack.foldRight("") {(obj, string) => toStringJavaObject(obj) + (if (string.length == 0) "" else " -> " + string)}
  }

  def evaluate(instanceStack: InstanceStack): List[InstanceStack] = instanceStack.reason match {
    case BreakOffReason.InInterestPath => instanceStack :: Nil
    case _ => Nil
  }

  def checkReferences(obj: JavaLazyReadObject, depth: Int, visited: List[JavaLazyReadObject], interestPath: String): List[InstanceStack] = {
    val newVisited = obj :: visited
    if (!visited.forall {_.getId != obj.getId}) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InfiniteLoop))
    } else if (obj.getClazz.getName.startsWith(interestPath)) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InInterestPath))
    } else if (depth > 50) {
      error(new InstanceStack(newVisited, BreakOffReason.MaxDepthExceeded).toString)
    } else {
      val referers = new RichEnumeration(obj.getReferers)
      if (referers.hasNext) {
        println("class: " + obj.getClazz.getName + ", id: " + obj.getId + ", size: " + newVisited.size)
        referers.foldRight(List[InstanceStack]()) {
          (ref, instanceStacks) =>
            checkReferences(ref.asInstanceOf[JavaLazyReadObject], depth + 1, newVisited, interestPath) ::: instanceStacks
        }
      } else {
        evaluate(new InstanceStack(newVisited, BreakOffReason.NoReferers))
      }
    }
  }

  def checkReferencesAndPrintResult(clazz: JavaClass, interestPath: String): Unit = {
    println("Analyzing class: " + clazz.getName)
    new RichEnumeration(clazz.getInstances(false)).foreach {
      obj => {
        val javaObject = obj.asInstanceOf[JavaLazyReadObject]
        println("Analyzing instance: " + toStringJavaObject(javaObject))
        checkReferences(javaObject, 0, Nil, interestPath).foreach {instanceStack => println(instanceStack)}
      }
    }
  }

}