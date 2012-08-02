package net.mtgto.regexsample

import collection.mutable.ArrayBuffer

trait State {
  def matches(chars: Seq[Char]): Boolean
}

class EmptyState extends State {
  val emptyStates = ArrayBuffer.empty[State]

  override def matches(chars: Seq[Char]): Boolean = {
    emptyStates.exists(
      _.matches(chars)
    )
  }

  // add state to emptyStates
  def addState(state: State) = {
    emptyStates += state
  }

  override def toString: String = {
    "Empty"
  }
}

// finite state
object FiniteState extends State {
  override def matches(chars: Seq[Char]): Boolean = {
    chars.isEmpty
  }

  override def toString: String = {
    "Finite"
  }
}

// state eats one character
class CharState(char: Char, nextState: State) extends State {
  override def matches(chars: Seq[Char]): Boolean = {
    chars match {
      case hd :: tl => {
        if (hd == char) {
          nextState.matches(tl)
        } else {
          false
        }
      }
      case Nil => false
    }
  }

  override def toString: String = {
    "Char(" + char + ") -> " + nextState
  }
}
