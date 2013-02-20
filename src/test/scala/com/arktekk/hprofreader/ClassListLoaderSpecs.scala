package com.arktekk.hprofreader

import org.specs.Specification
import java.io.File
import collection.immutable.HashMap

class ClassListLoaderSpecs extends Specification {

  "collect file" in {
    val path = "src/test/resources/class-loader-classes"
    new ClassListLoader(path).classes must be equalTo (
      HashMap("com.arktekk.test.T" -> (path + "/T.class" :: Nil))
      )
  }

  "collect jar file" in {
    val path = "src/test/resources/example-jar-1.0.jar"
    new ClassListLoader(path).classes must be equalTo(
      HashMap("com.arktekk.examplejar.Example" -> (path :: Nil)) +
      ("com.arktekk.examplejar.Example$1B" -> (path :: Nil)) +
      ("com.arktekk.examplejar.Example$A" -> (path :: Nil))
    )
  }

}
