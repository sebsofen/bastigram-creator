package de.bastigram.post.postentities

import com.typesafe.scalalogging.Logger
import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.{VariableDeclaration, VariableMemory}

import scala.concurrent.Future

case object ImagePostEntity extends PostEntityTraitMatcher {
  val logger = Logger(classOf[ImagePostEntity])
  override def matchPost(matchInstruction: PostCompiler.Instruction): Boolean = matchInstruction match {
    case VariableDeclaration(_, statement) => statement.startsWith("[imgs")
    case _                                 => false
  }

  override def postEntityFromInstruction(matchInstruction: PostCompiler.Instruction,
                                         postCache: (String) => Option[CompiledPost],
                                         postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)] = {
    matchInstruction match {
      case VariableDeclaration(variable, statement) =>
        logger.debug(statement)

        val imgList =
          PostEntity.strToArgList(statement.stripPrefix("[imgs")).map(image => image.replace("~", postSlug))
        logger.debug(imgList.toString)
        Future.successful((variable, ImagePostEntity(imgList.toList)))

      case _ =>
        Future.failed(new StatementNotSupportedException)

    }
  }
}

case class ImagePostEntity(imgs: List[String]) extends PostEntityTrait {
  override def typeDesc(): String = "IMGS"

  /**
    * merge two post entity traits
    *
    * @param pet
    * @return
    */
  override def +(pet: PostEntityTrait): PostEntityTrait = pet match {
    case ImagePostEntity(petimgs) => ImagePostEntity(imgs ++ petimgs)
  }
}
