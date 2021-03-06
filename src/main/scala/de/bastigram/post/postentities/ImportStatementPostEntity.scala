package de.bastigram.post.postentities

import de.bastigram.model.CompiledPost
import de.bastigram.post.PostCompiler
import de.bastigram.post.PostCompiler.{FromImportAsInst, VariableMemory}

import scala.concurrent.Future



case object ImportStatementPostEntity extends PostEntityTraitMatcher {

  override def matchPost(matchInstruction: PostCompiler.Instruction): Boolean =
    matchInstruction match {
      case FromImportAsInst(from, imp, as) => true
      case _                               => false
    }

  /**
    *
    * @param matchInstruction this will be cased to FromImportAsInst immediately
    * @param postCache
    * @return
    */
  override def postEntityFromInstruction(
      matchInstruction: PostCompiler.Instruction,
      postCache: (String) => Option[CompiledPost], postSlug: String,memory: VariableMemory): Future[(String, PostEntityTrait)] = {
    val inst = matchInstruction.asInstanceOf[FromImportAsInst]

    postCache(inst.from) match {
      case Some(post) =>
        post.memory.get(inst.imp) match {
          case Some(variable) =>
            Future.successful((inst.as, variable))
          case None => Future.failed(new PostNotFoundException)
        }
      case None => Future.failed(new PostNotFoundException)
    }
  }
}
