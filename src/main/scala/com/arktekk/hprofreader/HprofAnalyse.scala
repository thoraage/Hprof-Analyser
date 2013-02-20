package com.arktekk.hprofreader

import java.io.File

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
        new ClassListLoader("../callway/callway-portal/target/callway-portal-1.0.4-SNAPSHOT.war").classes.foreach { println }
        /*val snapshot = Reader.readFile(fileName, true, 0)
        snapshot.resolve(true)*/
        //val interestPath = "com.tandberg"
        /*val classes = new ClassListLoader("/opt/local/packages/glassfish/lib").classes ++ new ClassListLoader("/opt/local/packages/glassfish/domains/domain1/lib").classes
        val mine = snapshot.getClasses.asInstanceOf[java.util.Iterator[JavaClass]].filter(_.asInstanceOf[JavaClass].getName.startsWith(interestPath)).filter(!_.asInstanceOf[JavaClass].getName.contains("EntityFilter"))

        mine.foreach {
          clazz =>
            Analyser.checkReferencesAndPrintResult(clazz, interestPath, classes)
        }*/
      }
    }
  }

}