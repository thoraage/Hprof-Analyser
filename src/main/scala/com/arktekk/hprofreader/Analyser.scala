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
  }

  def toStringJavaObject(obj: JavaLazyReadObject): String = {
    obj.getClazz.getName + "(" + obj.getId + ")"
  }
  
  case class InstanceStack(stack: List[JavaLazyReadObject], reason: BreakOffReason.Value) {
    override def toString = reason + ": " + stack.foldRight("") {(obj, string) =>  string + (if (string.length == 0) "" else " -> ") + toStringJavaObject(obj)}
  }

  def evaluate(instanceStack: InstanceStack): List[InstanceStack] = instanceStack.reason match {
    case BreakOffReason.InAppServerLibrary => instanceStack :: Nil
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
      val referers = new RichEnumeration(obj.getReferers)
      if (referers.hasNext) {
        referers.foldRight(List[InstanceStack]()) {
          (ref, instanceStacks) => ref match {
            case ref: JavaLazyReadObject => checkReferences(ref, depth + 1, newVisited, interestPath, appServerClassMap) ::: instanceStacks
            case ref: JavaClass => {
              // TODO: This is probably an enum instance, but what is a enum instance to class reference?
              instanceStacks
            }
            case ref => error("Got unknown class: " + ref)
          }
        }
      } else {
        evaluate(new InstanceStack(newVisited, BreakOffReason.NoReferers))
      }
    }
  }

  def checkReferencesAndPrintResult(clazz: JavaClass, interestPath: String, appServerClassMap: Map[String, (ZipFile, ZipEntry)]): Unit = {
    println("Analyzing class: " + clazz.getName)
    new RichEnumeration(clazz.getInstances(false)).foreach {
      obj => {
        val javaObject = obj.asInstanceOf[JavaLazyReadObject]
        println("Analyzing instance: " + toStringJavaObject(javaObject))
        checkReferences(javaObject, 0, Nil, interestPath, appServerClassMap).foreach {instanceStack => println(instanceStack)}
      }
    }
  }

}