package de.bastigram.post.postentities
import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.{VariableDeclaration, VariableMemory}
import de.bastigram.post.postentities.MapPostEntity.logger

import scala.concurrent.Future

case object YoutubePostEntity extends PostEntityTraitMatcher {
  override def matchPost(matchInstruction: PostCompiler.Instruction): Boolean = matchInstruction match {
    case VariableDeclaration(_, statement) => statement.startsWith("[youtube")
    case _                                 => false
  }

  override def postEntityFromInstruction(matchInstruction: PostCompiler.Instruction, postCache: String => Option[CompiledPost], postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)] =     matchInstruction match {
    case VariableDeclaration(variable, statement) =>
      logger.debug(statement)
      val entity = PostEntity.strToArgMap(statement.stripPrefix("[youtube")).foldLeft(YoutubePostEntity("")) {
        case (entity,(declaration, value)) =>
          declaration match {
            case "key" =>
              YoutubePostEntity(value.replace("\"", ""))
            case f =>
              logger.error("was trying to compile youtube post entity, but cannot understand " + f )
              entity
          }
      }

      Future.successful((variable, entity))
    case _ =>
      Future.failed(new StatementNotSupportedException)

  }
}

case class YoutubePostEntity(youtubeKey: String) extends PostEntityTrait {
  override def typeDesc(): String = "YOUTUBE"

  /**
    * merge two post entity traits
    *
    * @param pet
    * @return
    */
  override def +(pet: PostEntityTrait): PostEntityTrait = ???
}