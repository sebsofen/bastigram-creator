package de.bastigram.model

import de.bastigram.api.PlainPostSource.PlainPost
import de.bastigram.post.PostCompiler.VariableMemory
import de.bastigram.post.postentities.{DatePostEntity, MapPostEntity, PostBodyEntity}

import scala.util.Try

case class CompiledPost(slug: String, checkSum: String, memory: VariableMemory, originPost: PlainPost) {
  def getBody(): Option[PostBodyEntity] = {
    Try {
      memory("postBody").asInstanceOf[PostBodyEntity]
    }.toOption
  }

  def getCreated(): Long =
    Try {
      memory("created").asInstanceOf[DatePostEntity].timestamp
    }.toOption.getOrElse(0L)

  def getLocation(): Option[MapPostEntity] = Try { memory("location").asInstanceOf[MapPostEntity] }.toOption

}
