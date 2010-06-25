package com.arktekk.hprofreader

import java.io.File
import java.util.zip.{ZipEntry, ZipFile}

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

class ClassListLoader(val path: String) {
  def collectZipFile(zipFile: ZipFile): Map[String, (ZipFile, ZipEntry)] = {
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

  def collect(file: File): Map[String, (ZipFile, ZipEntry)] = {
    if (file.isDirectory) {
      collectDirectory(file)
    } else if (file.isFile) {
      if (file.getName.endsWith(".jar"))
        collectZipFile(new ZipFile(file))
      else
        Map[String, (ZipFile, ZipEntry)]()
    } else {
      error("File " + file + " exists, but is neither file nor directory; meesa confused")
    }
  }

  def collectDirectory(directory: File): Map[String, (ZipFile, ZipEntry)] = {
    directory.listFiles.foldRight(Map[String, (ZipFile, ZipEntry)]()) {
      (file, map) => {
        map ++ collect(file)
      }
    }
  }

  val file = new File(path)
  val classes: Map[String, (ZipFile, ZipEntry)] = if (file.exists) {
    collect(file)
  } else {
    error("File " + path + " does not exist")
  }
}