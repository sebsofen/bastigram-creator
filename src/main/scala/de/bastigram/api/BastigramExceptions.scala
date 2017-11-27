package de.bastigram.api

object BastigramExceptions {
  abstract class PostWriteException extends Exception
  class PostAlreadyExistsException extends PostWriteException
}


