/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
 */

package intaglio

import guillotine._
import mitigation._
import annexation._
import java.nio.file.{Files, Paths}
import language.implicitConversions

trait LatexBackend {
  def process(latex: Latex, data: Seq[(String, Array[Byte])]): Result[PdfFile, ~ | ShellFailure | LatexException]
}

object TextProcess {
  implicit val defaultTextProcess: TextProcess = unindent()
}
trait TextProcess {
  def apply(s: String): String
}

object unindent {
  def apply(): TextProcess = implicitTextProcess
  implicit val implicitTextProcess: TextProcess = new TextProcess {
    def apply(s: String): String = {
      val lines = s.split("\n").dropWhile(_.isEmpty)
      val indent = lines.headOption.getOrElse("").indexWhere(_ != ' ')
      lines.map { ln =>
        if (ln.take(indent).forall(_ == ' ')) ln.drop(indent) else ln.dropWhile(_ == ' ')
      }.mkString("\n")
    }
  }
}

object latexBackends {
  implicit val xelatex: LatexBackend = new LatexBackend {
    def process(latex: Latex, data: Seq[(String, Array[Byte])]): Result[PdfFile, ~ | ShellFailure | LatexException] = {
      val dir = new java.io.File("/tmp", java.util.UUID.randomUUID().toString)
      dir.mkdir()
      data.foreach {
        case (name, bytes) =>
          val f = new java.io.File(dir, name)
          val bos = new java.io.FileOutputStream(f)
          bos.write(bytes)
      }
      val file = new java.io.File(dir, "document.tex")
      val fw = new java.io.FileWriter(file)
      fw.write(latex.content)
      fw.flush()
      fw.close()

      val cmd = sh"/usr/bin/xelatex -interaction nonstopmode ${file.getAbsolutePath}"
      implicit val env = environments.enclosing.copy(workDir = Some(dir.getAbsolutePath))
      val output = cmd.exec[String].split("\n").to[Iterator]

      while (output.hasNext) {
        var next = output.next()
        if (next startsWith "! ") {
          val msg = next.drop(if (next.startsWith("! LaTeX Error: ")) 15 else 2)
          val content = new StringBuilder()
          while (output.hasNext && !next.startsWith("l.")) {
            next = output.next()
            if (!next.startsWith("l.")) content.append(s"$next\n")
            else {
              val line = next.drop(2).takeWhile(_ != ' ').toInt
              LatexException(msg, line, content.toString.trim)
            }
          }
        }
      }

      val bytes = Files.readAllBytes(Paths.get(file.getAbsolutePath.replaceAll("tex$", "pdf")))
      Answer(PdfFile(bytes))
    }
  }
}

object Latex {
  def escape(string: String): String = string.flatMap {
    case c @ ('#' | '$' | '%' | '&' | '_' | '{' | '}') => s"\\$c"
    case '\\' => "\\textbackslash{}"
    case '\n' => " \\\\\n"
    case '^' => "\\textasciicircum{}"
    case '~' => "\\textasciitilde{}"
    case c => c.toString
  }
}

case class LatexException(msg: String, line: Int, content: String) extends Exception {
  override def getMessage = s"latex error at line $line: $msg"
}

case class Latex(content: String) {

  def generate(data: (String, Array[Byte])*)(implicit backend: LatexBackend): Result[PdfFile, ~ | ShellFailure | LatexException] =
    backend.process(this, data)
}

case class PdfFile(data: Array[Byte])

object Latexable {
  implicit val stringLatexable: Latexable[String] = new Latexable[String] {
    def toLatex(s: String) = Latex.escape(s)
  }

  implicit val latexLatexable: Latexable[Latex] = new Latexable[Latex] {
    def toLatex(latex: Latex) = latex.content
  }

  implicit def seqLatexable[T: Latexable]: Latexable[Seq[T]] = new Latexable[Seq[T]] {
    def toLatex(seq: Seq[T]) = seq.map(implicitly[Latexable[T]].toLatex(_)).mkString
  }
}

trait Latexable[-T] { def toLatex(t: T): String }

case class LatexStringContext(sc: StringContext) {
  def latex(variables: Annex[Latexable]*)(implicit process: TextProcess) =
    Latex(process(sc.parts.zip(variables.map(_(_.toLatex))).map { case (a, b) => a + b }.mkString + sc.parts.last))
}

object `package` {
  implicit def latexStringContext(sc: StringContext): LatexStringContext = LatexStringContext(sc)
}
