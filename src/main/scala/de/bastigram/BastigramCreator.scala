package de.bastigram

import akka.Done

import scala.concurrent.Future

object BastigramCreator {

  def apply(): BastigramCreator = ???

}

case class BastigramFile(name: String)

class BastigramCreator {

  def createRaw(slug: String,
                readme: String,
                releaseDate: Long,
                hidden: Boolean,
                files: Seq[BastigramFile]): Future[Done] = ???
}
