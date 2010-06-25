package com.arktekk.hprofreader

/**
 * @author Thor Åge Eldby (thoraageeldby@gmail.com)
 */

class RichEnumeration[T](enumeration: java.util.Enumeration[T]) extends Iterator[T] {
  def hasNext: Boolean = enumeration.hasMoreElements()
  def next: T = enumeration.nextElement()
}
