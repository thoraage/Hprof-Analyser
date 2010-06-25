package com.arktekk.hprofreader

import java.io.File
import java.util.zip.{ZipEntry, ZipFile}

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

class ClassListLoader(val path: String) {

  def collect(zipFile: ZipFile): Map[String, (ZipFile, ZipEntry)] = {
    new RichEnumeration(zipFile.entries).foldRight(Map[String, (ZipFile, ZipEntry)]()) {
      (entry, map) => {
        val name = entry.getName
        if (name.endsWith(".class"))
          map + (name.replaceAll("\\.class", "").replaceAll("/", ".") -> (zipFile, entry))
        else
          map
      }
    }
  }

  def collectDirectory(directory: File): Map[String, (ZipFile, ZipEntry)] = {
    directory.listFiles.foldRight(Map[String, (ZipFile, ZipEntry)]()) {
      (file, map) => {
        if (file.isFile) {
          if (file.getName.endsWith(".jar"))
            map ++ collect(new ZipFile(file))
          else
            map
        } else if (file.isDirectory) {
          map ++ collectDirectory(file)
        } else {
          error("File " + file + " exists, but is neither file nor directory; meesa confused")
        }
      }
    }
  }

  val file = new File(path)
  val classes: Map[String, (ZipFile, ZipEntry)] = if (file.exists) {
    if (file.isDirectory) {
      collectDirectory(file)
    } else if (file.isFile) {
      collect(new ZipFile(file))
    } else {
      error("File " + path + " exists, but is neither file nor directory; meesa confused")
    }
  } else {
    error("File " + path + " does not exist")
  }
}