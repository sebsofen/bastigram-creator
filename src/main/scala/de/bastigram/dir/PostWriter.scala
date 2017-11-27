package de.bastigram.dir

import java.io.{File, FileOutputStream}
import java.nio.file.Paths
import java.text.SimpleDateFormat

import akka.Done
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import de.bastigram.BastigramFile
import de.bastigram.api.BastigramExceptions.PostAlreadyExistsException

import scala.concurrent.{ExecutionContextExecutor, Future}

class PostWriter(settings: PlainPostSourceFromDirSettings)(
    implicit system: ActorSystem,
    _ec: ExecutionContextExecutor,
    materializer: ActorMaterializer) {

  val simpleDate = new SimpleDateFormat("mm.dd.yyyy")

  def createRaw(slug: String,
                readme: String,
                releaseDate: Option[Long],
                hidden: Boolean,
                files: Seq[BastigramFile]): Future[Done] = {
    val postPath = Paths.get(settings.POSTS_DIR, slug)
    if (postPath.toFile.exists()) {
      Future.failed(new PostAlreadyExistsException())
    } else {
      //create post directory

      //write readme file
      postPath.toFile.mkdir()

      val readmeWriteFuture = Future {
        val readmeFile = Paths.get(settings.POSTS_DIR, slug, "readme").toFile
        writeFileContent(readmeFile, readme)
      }

      val releaseWriteFuture = Future {
        val releaseFile = Paths.get(settings.POSTS_DIR, slug, "release").toFile
        releaseDate
          .map(l => simpleDate.format(l))
          .map(d => writeFileContent(releaseFile, d))
      }

      val hiddenWriteFuture = Future {
        if (hidden) {} else {
          Done
        }
      }

      val mediaFileWriter =
        Source.fromIterator(() => files.toIterator).mapAsync(1) {
          mediaFileObj =>
            val mediaFile =
              Paths.get(settings.POSTS_DIR, slug, mediaFileObj.name).toFile
            writeFileContent(mediaFile, mediaFileObj.content)
        }

      for {
        _ <- readmeWriteFuture
        _ <- releaseWriteFuture
        _ <- hiddenWriteFuture
      } yield Done

    }
  }

  def deleteBySlug(slug: String) : Future[Done] = ???

  private def writeFileContent(file: File, content: Array[Byte]): Future[Done] =
    Future {
      val os = new FileOutputStream(file)
      os.write(content)
      os.close()
    }.map(f => Done)

  private def writeFileContent(file: File, content: String): Future[Done] =
    writeFileContent(file, content.getBytes)



}
