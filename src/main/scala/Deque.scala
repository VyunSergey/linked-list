import Deque._

case class Deque[T](
                     // link to head of Double Linked List
                     // [head] <-> [...] <-> ... <-> [...]
                     private var head: Option[Node[T]] = None,
                     // link to tail of Double Linked List
                     // [...] <-> [...] <-> ... <-> [tail]
                     private var tail: Option[Node[T]] = None
                   ) {
  // insert value at back
  def push(value: T): Unit = {
    // if tail exists add Node(value) to tail of it
    // else create Node(value)
    tail = tail.map(node => Node(value, prev = Some(node))).orElse(Some(Node(value)))
    // update link to next in tail
    tail.map(_.prev.map(_.next = tail))
    // update head
    head = head.orElse(tail)
  }

  // remove value at back
  def pop(): Option[T] = {
    // if tail exists update link in prev to next in tail
    // else None
    val res = tail.map { node =>
      node.prev.foreach(_.next = None)
      node.value
    }
    // update tail
    tail = tail.flatMap(_.prev)
    // update head
    if (tail.isEmpty) head = None
    res
  }

  // insert value at front
  def unshift(value: T): Unit = {
    // if head exists add Node(value) to head of it
    // else create Node(value)
    head = head.map(node => Node(value, next = Some(node))).orElse(Some(Node(value)))
    // update link to prev in head
    head.map(_.next.map(_.prev = head))
    // update tail
    tail = tail.orElse(head)
  }

  // remove value at front
  def shift(): Option[T] = {
    // if head exists update link in next to prev in tail
    // else None
    val res = head.map { node =>
      node.next.foreach(_.prev = None)
      node.value
    }
    // update head
    head = head.flatMap(_.next)
    // update tail
    if (head.isEmpty) tail = None
    res
  }
}

object Deque {
  case class Node[T](
                      value: T,
                      var prev: Option[Node[T]] = None,
                      var next: Option[Node[T]] = None
                    )
}
