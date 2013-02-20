package com.arktekk.hprofreader

import java.io.File
import java.util.zip.{ZipInputStream, ZipEntry, ZipFile}
import collection.immutable.HashMap

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

class ClassListLoader(val path: String) {

  type ClassMap = HashMap[String, List[String]]

  /*  case class ClassMap(map: Map[String, List[String]]) {
    def add(name: String, fileName: String): ClassMap = ClassMap(map + (name -> (fileName :: Nil))) 
  }

  object ClassMap {
    def apply(name: String, fileName: String): ClassMap =
      ClassMap(Map[String, List[String]]() + (name -> (fileName :: Nil)))
  }*/

  def createClassNameFromFileName(name: String): String = {
    name.replaceAll("\\.class", "").replaceAll("/", ".")
  }

  def collectZipFile(zipFile: ZipFile): ClassMap = {
    new RichEnumeration(zipFile.entries).foldLeft(new ClassMap()) {
      (map, entry) => {
        val name = entry.getName
        if (name.endsWith(".class"))
          map + (createClassNameFromFileName(name) -> (entry.getName :: Nil))
        else
          map
      }
    }
  }

  case class ZipInputStreamIterator(zipInputStream: ZipInputStream) extends Iterator[ZipEntry] {
    var nextElement: Option[ZipEntry] = None
    override def hasNext = nextElement match {
      case None =>
        zipInputStream.getNextEntry match {
          case null => false
          case zipEntry =>
            nextElement = Some(zipEntry)
            true
        }
      case Some(_) => true
    }
    override def next = {
      val element = nextElement.get
      nextElement = None
      element
    }
  }

  def collectZipFile(zipInputStream: ZipInputStream): ClassMap = {
    (for (entry <- ZipInputStreamIterator(zipInputStream)) yield entry).foldLeft(new ClassMap()) {
      (map, entry) =>
        val name = entry.getName
        if (name.toLowerCase.endsWith(".class")) map + (name -> ("file?" :: Nil)) else map
    }
  }

  def collectWarFile(zipFile: ZipFile): ClassMap = {
    new RichEnumeration(zipFile.entries).foldLeft(new ClassMap()) {
      (map, entry) => {
        val name = entry.getName.toLowerCase
        if (name.endsWith(".jar")) {
          collectZipFile(new ZipInputStream(zipFile.getInputStream(entry)))
          map
        } else if (name.endsWith(".class"))
          map + (createClassNameFromFileName(name) -> (entry.getName :: Nil))
        else
          map
      }
    }
  }

  def collect(file: File): ClassMap = {
    if (file.isDirectory) {
      collectDirectory(file)
    } else if (file.isFile) {
      if (file.getName.toLowerCase.endsWith(".jar"))
        collectZipFile(new ZipFile(file))
      else if (file.getName.toLowerCase.endsWith(".war"))
        collectWarFile(new ZipFile(file))
      else
        new ClassMap()
    } else {
      error("File " + file + " exists, but is neither file nor directory; meesa confused")
    }
  }

  def collectDirectory(directory: File): ClassMap = {
    directory.listFiles.foldLeft(new ClassMap()) {
      (map, file) => {
        if (file.isFile && file.getName.toLowerCase.endsWith(".class"))
          map + (createClassNameFromFileName(file.getName) -> (file.toString :: Nil))
        else
          map ++ collect(file)
      }
    }
  }

  val file = new File(path)
  val classes: ClassMap = if (file.exists) {
    collect(file)
  } else {
    error("File " + path + " does not exist")
  }

}
