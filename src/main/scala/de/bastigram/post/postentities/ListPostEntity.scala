package de.bastigram.post.postentities

import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.{VariableDeclaration, VariableMemory}

import scala.concurrent.Future

case object ListPostEntity extends PostEntityTraitMatcher {
  override def matchPost(matchInstruction: PostCompiler.Instruction): Boolean = matchInstruction match {
    case VariableDeclaration(variable, statement) => statement.startsWith("[list")
    case _                                        => false
  }

  override def postEntityFromInstruction(
      matchInstruction: PostCompiler.Instruction,
      postCache: (String) => Option[CompiledPost],postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)] = matchInstruction match {
    case VariableDeclaration(variable, statement) =>
      val args = PostEntity.strToArgList(statement.stripPrefix("[list")).toList
      Future.successful((variable, ListPostEntity(args)))
    case _ =>
      Future.failed(new StatementNotSupportedException)
  }
}

case class ListPostEntity(list: List[String]) extends PostEntityTrait {
  override def typeDesc(): String = "LIST"

  /**
    * merge two post entity traits
    *
    * @param pet
    * @return
    */
  override def +(pet: PostEntityTrait): PostEntityTrait = pet match {
    case ListPostEntity(otherlist) => ListPostEntity(otherlist ++ list)
  }

}
