package com.arktekk.hprofreader

import java.lang.String
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
        var mine = new Wrapper(snapshot.getClasses.asInstanceOf[java.util.Iterator[JavaClass]]).filter(_.asInstanceOf[JavaClass].getName.startsWith("com.tandberg"))
        mine.next.visitReferencedObjects(new JavaHeapObjectVisitor() {
          override def visit(javaHeapObject: JavaHeapObject) {
            println(javaHeapObject.getClazz.getName + "[" + javaHeapObject.getId + "]: ")
            new RichEnumeration(javaHeapObject.getReferers.asInstanceOf[java.util.Enumeration[JavaHeapObject]])
                    .filter(!_.getClazz.getName.startsWith("java"))
                    .map("RefferedBy: " + _.getClazz.getName)
                    .foreach(println _)
          }
          override def exclude(javaClass: JavaClass, javaField: JavaField): Boolean = {
            println("exclude");
            false
          }
          override def mightExclude: Boolean = {
            println("mightexclude");
            false
          }
        })
        println("Analysed");
      }
    }
  }

  class RichEnumeration[T](enumeration: java.util.Enumeration[T]) extends Iterator[T] {
    def hasNext: Boolean = enumeration.hasMoreElements()
    def next: T = enumeration.nextElement()
  }

}