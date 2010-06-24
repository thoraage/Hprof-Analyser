package com.arktekk.hprofreader

import java.io.File
import com.sun.tools.hat.internal.parser.Reader
import com.sun.tools.hat.internal.model._
import collection.jcl.MutableIterator.Wrapper

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

object HprofAnalyse {
  def usage(message: String) = println(message);

  def main(args: Array[String]): Unit = {
    if (args.size != 1) {
      usage("No arguments");
    } else {
      val fileName: String = args(0)
      val file: File = new File(fileName)
      if (!file.exists) {
        usage("File " + fileName + " not found");
      } else {
        val snapshot = Reader.readFile("/Users/thoraageeldby/Documents/Arktekk/Sources/callway/dump.hprof", true, 0)
        snapshot.resolve(true)
        val interestPath = "com.tandberg"
        var mine = new Wrapper(snapshot.getClasses.asInstanceOf[java.util.Iterator[JavaClass]]).filter(_.asInstanceOf[JavaClass].getName.startsWith(interestPath))
        var clazz = mine.next

        checkReferencesAndPrintResult(clazz.getInstances(false).nextElement.asInstanceOf[JavaObject])
        println("Analysed");
      }
    }
  }

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
    override def toString = reason + ": " + stack.foldRight("") {(obj, string) => obj.getClazz.getName + "(" + obj.getId + ") -> "}
  }

  def checkReferences(obj: JavaLazyReadObject, depth: Int, visited: List[JavaLazyReadObject]): List[InstanceStack] = {
    //val prepend = if (depth == 0) "" else String.format("%" + (depth * 2) + "s", "")
    //println(prepend + "Class: " + obj.getClazz.getName + ", ref: " + obj.getId)
    if (!visited.forall {_.getId != obj.getId}) {
      //error("Object already visited")
      new InstanceStack(visited, BreakOffReason.InfiniteLoop) :: Nil
    } else if (depth > 50) {
      new InstanceStack(visited, BreakOffReason.MaxDepthExceeded) :: Nil
    } else {
      val referers = new RichEnumeration(obj.getReferers)
      if (referers.hasNext) {
        referers.foldRight(List[InstanceStack]()) {
          (ref, instanceStacks) =>
            checkReferences(ref.asInstanceOf[JavaLazyReadObject], depth + 1, obj :: visited) ::: instanceStacks
        }
        /*while (referers.hasNext) {
          checkReferences(referers.next.asInstanceOf[JavaLazyReadObject], depth + 1, obj :: visited)
        }*/
      } else {
        new InstanceStack(visited, BreakOffReason.NoReferers) :: Nil
      }
    }
  }

  def checkReferencesAndPrintResult(obj: JavaLazyReadObject): Unit = {
    checkReferences(obj, 0, Nil).foreach {println _}
  }

}