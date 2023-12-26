object Exceptions {
  case object StartNotFoundException extends Exception("No start found")
  case object ForkFoundException extends Exception("Fork found, must only have one available direction change")
  case object NoExitFoundException extends Exception("No exit found")
  case object TooManyStartsException extends Exception("Too many starts found, must only have one available start")
  case object IllegalPathChangeException extends Exception("Illegal path or change of direction")
  case class IllegalCharException(c: Char) extends Exception(s"Illegal maze character found: $c")

}
