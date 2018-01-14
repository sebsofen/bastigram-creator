package de.bastigram.post.postentities

import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.VariableMemory

import scala.concurrent.Future

trait PostEntityTrait {
  def typeDesc(): String

  def memOverride(old: PostEntityTrait): PostEntityTrait = this

  /**
    * merge two post entity traits
    * @param pet
    * @return
    */
  def +(pet: PostEntityTrait): PostEntityTrait
}

trait PostEntityTraitMatcher {

  def matchPost(matchInstruction: PostCompiler.Instruction): Boolean
  def postEntityFromInstruction(
      matchInstruction: PostCompiler.Instruction,
      postCache: String => Option[CompiledPost],
      postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)]
}

/**
  * listing of possible post entity variable declarations
  */
case object PostEntity {
  val entityMatcherList: Seq[PostEntityTraitMatcher] =
    Seq(DummyPostEntity,
        ImportStatementPostEntity,
        PostBodyEntity,
        ListPostEntity,
        ImagePostEntity,
        DatePostEntity,
        MapPostEntity,
        YoutubePostEntity,
        LabelPostEntity)

  /**
    * TODO: might be better placed in own trait
    * @param string
    * @return
    */
  def strToArgMap(string: String): Map[String, String] =
    string
      .stripPrefix("[")
      .foldLeft[(Map[String, String], String, String, MapFoldState)](
        (Map(), "", "", NameBuilder(""))) {
        case ((map, oldName, oldValue, mapFoldState), char) =>
          mapFoldState match {

            case x @ NameBuilder(name) if char != '=' =>
              (map, oldName, oldValue, x.copy(name = name + char))

            case NameBuilder(name) if char == '=' =>
              (map, name, "", ValueBuilder("", None))

            case x @ ValueBuilder(value, endChar) if endChar.isEmpty =>
              char match {
                case '"' =>
                  (map, oldName, oldValue, x.copy(endChar = Some("\"")))
                case _ =>
                  (map,
                   oldName,
                   oldValue,
                   x.copy(value = x.value + char, endChar = Some(" ]")))
              }

            case x @ ValueBuilder(value, endChar) if endChar.isDefined =>
              char match {

                case chr if endChar.get contains chr  =>
                  (map + (oldName.replaceAll(" ", "") -> x.value),
                   "",
                   "",
                   NameBuilder(""))

                case _ =>
                  (map, oldName, oldValue, x.copy(value = x.value + char))

              }

          }

      }
      ._1

  abstract class MapFoldState
  case class NameBuilder(name: String) extends MapFoldState
  case class ValueBuilder(value: String, endChar: Option[String])
      extends MapFoldState

  def strToArgList(str: String): Array[String] =
    str
      .stripPrefix("[")
      .stripSuffix("]")
      .split(" ")
      .filter(_ != "")

}

abstract class PostException extends Exception
class InvalidOperandExeption extends PostException
class PostNotFoundException extends PostException
class MissingStatementException extends PostException
class StatementNotSupportedException extends PostException
