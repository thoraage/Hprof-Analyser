package com.arktekk.hprofreader

import com.sun.tools.hat.internal.model.{JavaClass, JavaLazyReadObject}
import java.util.zip.{ZipEntry, ZipFile}

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

object Analyser {
  object BreakOffReason extends Enumeration {
    val InfiniteLoop = Value("InfiniteLoop")
    val NoReferers = Value("NoReferers")
    val MaxDepthExceeded = Value("MaxDepthExceeded")
    val InInterestPath = Value("InInterestPath")
    val InAppServerLibrary = Value("InAppServerLibrary")
    val InDeepStructure = Value("InDeepStructure")
    val StaticField = Value("StaticField")
  }

  def toStringJavaObject(obj: JavaLazyReadObject): String = {
    obj.getClazz.getName + "(" + obj.getId + ")"
  }

  case class InstanceStack(stack: List[JavaLazyReadObject], reason: BreakOffReason.Value) {
    override def toString = reason + ": " + stack.foldRight("") {(obj, string) => string + (if (string.length == 0) "" else " -> ") + toStringJavaObject(obj)}
  }

  def evaluate(instanceStack: InstanceStack): List[InstanceStack] = instanceStack.reason match {
    case BreakOffReason.InAppServerLibrary => instanceStack :: Nil
    /*case BreakOffReason.InDeepStructure => instanceStack :: Nil
    case BreakOffReason.NoReferers => instanceStack :: Nil
    case BreakOffReason.InfiniteLoop => instanceStack :: Nil
    case BreakOffReason.InInterestPath => instanceStack :: Nil*/
    case _ => Nil
  }

  def checkForReferenceLists(visited: List[JavaLazyReadObject]): Boolean = {
    val classNames = Array("java.util.LinkedHashMap$Entry", "java.util.HashMap$Entry", "java.lang.ref.Finalizer")
    classNames.exists {className => visited.take(2).forall(_.getClazz.getName == className)}
  }

  def checkReferences(obj: JavaLazyReadObject, depth: Int, visited: List[JavaLazyReadObject], interestPath: String, appServerClassMap: Map[String, (ZipFile, ZipEntry)]): List[InstanceStack] = {
    val newVisited = obj :: visited
    if (!visited.forall {_.getId != obj.getId}) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InfiniteLoop))
    } else if (newVisited.size >= 2 && checkForReferenceLists(newVisited)) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InDeepStructure))
    } else if (obj.getClazz.getName.startsWith(interestPath) && visited.size != 0) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InInterestPath))
    } else if (appServerClassMap.contains(obj.getClazz.getName)) {
      evaluate(new InstanceStack(newVisited, BreakOffReason.InAppServerLibrary))
    } else if (depth > 250) {
      error(new InstanceStack(newVisited, BreakOffReason.MaxDepthExceeded).toString)
    } else {
      val filteredReferers = new RichEnumeration(obj.getReferers).filter({
        ref => ref match {
          case ref: JavaLazyReadObject => true
          case ref: JavaClass => {
            // TODO: This is probably an enum instance, but what is a enum instance to class reference?
            false
          }
          case ref => error("Got unknown class: " + ref)
        }
      }).map(_.asInstanceOf[JavaLazyReadObject])
      if (filteredReferers.hasNext) {
        filteredReferers.foldRight(List[InstanceStack]()) {
          (ref, instanceStacks) => checkReferences(ref, depth + 1, newVisited, interestPath, appServerClassMap) ::: instanceStacks
        }
      } else {
        evaluate(new InstanceStack(newVisited, BreakOffReason.NoReferers))
      }
    }
  }

  def checkReferencesAndPrintResult(clazz: JavaClass, interestPath: String, appServerClassMap: Map[String, (ZipFile, ZipEntry)]): Unit = {
    val instances = new RichEnumeration(clazz.getInstances(false))
    if (instances.hasNext) {
      println("Analyzing class: " + clazz.getName)
      instances.foreach {
        obj => {
          val javaObject = obj.asInstanceOf[JavaLazyReadObject]
          println("Analyzing instance: " + toStringJavaObject(javaObject))
          val stackInstances = checkReferences(javaObject, 0, Nil, interestPath, appServerClassMap)
          stackInstances.foreach {
            instanceStack => println(instanceStack)
          }
        }
      }
    }
  }

}