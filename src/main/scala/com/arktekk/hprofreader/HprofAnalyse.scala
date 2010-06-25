//package com.arktekk.hprofreader

import com.sun.tools.hat.internal.parser.Reader
import com.sun.tools.hat.internal.model._
import collection.jcl.MutableIterator.Wrapper

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

object HprofAnalyse {
  def usage(message: String) = println(message);

  //def main(args: Array[String]): Unit = {
  /*    if (args.size != 1) {
usage("No arguments");
} else {
val fileName: String = args(0)
val file: File = new File(fileName)
if (!file.exists) {
  usage("File " + fileName + " not found");
} else {*/
  val snapshot = Reader.readFile("/Users/thoraageeldby/Documents/Arktekk/Sources/callway/dump.hprof", true, 0)
  snapshot.resolve(true)
  val interestPath = "com.tandberg"
  val mine = new Wrapper(snapshot.getClasses.asInstanceOf[java.util.Iterator[JavaClass]]).filter(_.asInstanceOf[JavaClass].getName.startsWith(interestPath))
  /*
  var clazz = HprofAnalyse.mine.next
  Analyser.checkReferencesAndPrintResult(clazz, HprofAnalyse.interestPath)
  */
  println("Analysed");
  //}
  //}

}
