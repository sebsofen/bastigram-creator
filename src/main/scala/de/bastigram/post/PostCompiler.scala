package de.bastigram.post

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import com.typesafe.scalalogging.Logger
import de.bastigram.api.PlainPostSource.PlainPost
import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler.{Instruction, UnfinishedInstruction, VariableMemory}
import de.bastigram.post.postentities.{PostEntity, PostEntityTrait}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.Try

object PostCompiler {
  type VariableMemory = Map[String, PostEntityTrait]

  abstract class Instruction
  case class FromImportAsInst(from: String, imp: String, as: String) extends Instruction
  case class NopInst(content: String) extends Instruction
  case class UnfinishedInstruction(inst: Instruction) extends Instruction

  case class VariableDeclaration(variable: String, statement: String) extends Instruction

  abstract class NextInstruction
  case object NextUnset extends NextInstruction
  case object NextFrom extends NextInstruction
  case object NextImport extends NextInstruction
  case object NextAs extends NextInstruction

  case class FromImportAsInstBuilder(nextInst: NextInstruction = NextUnset,
                                     from: String = "",
                                     imp: String = "",
                                     as: String = "") {
    def build(): FromImportAsInst = FromImportAsInst(from, imp, as)
  }

  case class InstructionsAndBody(instructions: Seq[Instruction], body: String)

  /**
    * parse line to instruction
    * @param f
    * @return
    */
  def lineToInstruction(f: String, mayBeUnfinishedInst: Option[UnfinishedInstruction]): Instruction = f match {
    case f if f.startsWith("#from") =>
      val seperated = f.tail.split(" ")
      seperated
        .foldLeft(FromImportAsInstBuilder()) {
          case (builder, fragment) =>
            fragment match {
              case "from"   => builder.copy(nextInst = NextFrom)
              case "import" => builder.copy(nextInst = NextImport)
              case "as"     => builder.copy(nextInst = NextAs)
              case _ =>
                builder.nextInst match {
                  case NextFrom   => builder.copy(from = fragment)
                  case NextImport => builder.copy(imp = fragment)
                  case NextAs     => builder.copy(as = fragment)
                }
            }
        }
        .build()


    case f if f.startsWith("#val") =>
      val variableAndDeclaration = f.stripPrefix("#val").split("=")

      val variable = variableAndDeclaration.head.replaceAll(" ", "")
      val declaration = variableAndDeclaration.tail.mkString("=").split(" ").filter(_ != "").mkString(" ")

      val newStatement = VariableDeclaration(variable, declaration)

      if(f.stripLineEnd.endsWith("]")) {
        newStatement
      }else{
        UnfinishedInstruction(newStatement)
      }

    case f if mayBeUnfinishedInst.isDefined && mayBeUnfinishedInst.get.inst.isInstanceOf[VariableDeclaration] =>
      println("line " + f)
      val unfinshed = mayBeUnfinishedInst.get.inst.asInstanceOf[VariableDeclaration]
      val newStatement = unfinshed.copy(statement = unfinshed.statement + "\n" + f)
      if(f.stripLineEnd.endsWith("]")) {
        newStatement
      }else{
        UnfinishedInstruction(newStatement)
      }
    case f =>
      NopInst(f)
  }

  /**
    * convert an instruction to a variable
    * @param instruction
    * @param memory
    * @param ec
    * @return
    */
  def instructionToPostEntity(instruction: Instruction,
                              memory: VariableMemory,
                              postCache: String => Option[CompiledPost],
                              postSlug: String)(implicit ec: ExecutionContext): Future[(String, PostEntityTrait)] = {
    val entityMatcher = Try(PostEntity.entityMatcherList.filter(_.matchPost(instruction)).head).toOption match {
      case Some(f) => f
      case None => throw new IllegalArgumentException("instruction not known " + instruction)
    }
    entityMatcher.postEntityFromInstruction(instruction, postCache, postSlug,memory)

  }

}

class PostCompiler()(implicit system: ActorSystem, ec: ExecutionContextExecutor, materializer: ActorMaterializer) {
  val logger = Logger(classOf[PostCompiler])

  def compile(post: PlainPost, postCache: String => Option[CompiledPost]): Future[CompiledPost] = {
    logger.debug("compiling post " + post.slug)

    val zero: (VariableMemory, String, Option[UnfinishedInstruction]) = (Map(), "", None)

    val postFut = post.postBody
      .foldAsync(zero) {
        case ((varMem, postBody, mayBeUnfinishedInstruction), line) =>
          val newPostBody = postBody + "\n" + line
          val instr = PostCompiler.lineToInstruction(line,mayBeUnfinishedInstruction)

          instr match {
            case instruction : UnfinishedInstruction =>
              println("UnfinishedInsturciont " + instruction)
              Future.successful(varMem, postBody, Some(instruction))
            case instruction : Instruction =>
              PostCompiler.instructionToPostEntity(instruction, varMem, postCache, post.slug).map {
                case (varName, postEntity) =>
                  if (varMem.contains(varName)) {
                    (varMem + (varName -> postEntity.memOverride(varMem(varName))), newPostBody, None)
                  } else {
                    (varMem + (varName -> postEntity), newPostBody, None)
                  }

              }
          }


      }
      .runWith(Sink.head)
      .map { case (mem, postBody,_) => CompiledPost(post.slug, postBody, mem, post) }

    postFut.onFailure{case f => f.printStackTrace()}

    postFut
  }

}
