package de.bastigram.post.postentities

import com.typesafe.scalalogging.Logger
import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.{VariableDeclaration, VariableMemory}

import scala.concurrent.Future

case object MapPostEntity extends PostEntityTraitMatcher {
  val logger = Logger(classOf[MapPostEntity])
  override def matchPost(matchInstruction: PostCompiler.Instruction): Boolean = matchInstruction match {
    case VariableDeclaration(_, statement) => statement.startsWith("[map")
    case _                                 => false
  }

  override def postEntityFromInstruction(matchInstruction: PostCompiler.Instruction,
                                         postCache: (String) => Option[CompiledPost],
                                         postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)] = {
    matchInstruction match {
      case VariableDeclaration(variable, statement) =>
        logger.debug(statement)


          val entity = PostEntity.strToArgMap(statement.stripPrefix("[map")).foldLeft(MapPostEntity()) {
            case (entity,(declaration, value)) =>
              declaration match {

                case "geofile" =>
                  entity.copy(geofile = Some(value.replace("~", postSlug)))

                case "name" =>
                  entity.copy(name = Some(value.replace("\"", "")))

                case "latlon" =>
                  val latlon = value.split(",").map(_.toDouble)
                  entity.copy(latLon = Some(LatLon(latlon.head,latlon(1))))

                case "fromto" =>
                  val latlons = value.split('|')
                  println(latlons.mkString(" "))
                  val from = latlons(0).split(",").map(_.toDouble)
                  val to = latlons(1).split(",").map(_.toDouble)
                  println(from.mkString(" "))
                  println(to.mkString(" "))
                  entity.copy(fromTo = Some(LatLon(from.head,from(1)),LatLon(to.head,to(1))))

                case "label" =>
                  //TODO: MATCH FOR STRING IN VARIABLE instead of reference
                  val label = memory(value).asInstanceOf[LabelPostEntity].label
                  entity.copy(label = Some(label))

                case "type" =>
                  entity.copy(markerType = Some(value))

                case f =>
                  logger.error("was trying to compile map post entity, but cannot understand " + f )
                  entity

              }
          }

        Future.successful((variable, entity))

      case _ =>
        Future.failed(new StatementNotSupportedException)

    }
  }
}
case class LatLon(lat: Double, lon: Double)

case class MapPostEntity(geofile: Option[String] = None, latLon: Option[LatLon] = None, fromTo: Option[(LatLon,LatLon)] = None, name: Option[String] = None, markerType: Option[String] = None, label: Option[String] = None) extends PostEntityTrait {
  override def typeDesc(): String = "MAP"

  /**
    * merge two post entity traits
    *
    * @param pet
    * @return
    */
  override def +(pet: PostEntityTrait): PostEntityTrait = ???
}
