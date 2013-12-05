package spray.caching

import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import scala.concurrent.duration.Duration
import scala.annotation.tailrec
import scala.concurrent.Future

trait ExpiringCache[V] extends Cache[V] {
  val maxCapacity: Long
  val initialCapacity: Int
  val timeToLive: Duration
  val timeToIdle: Duration

  require(!timeToLive.isFinite || !timeToIdle.isFinite || timeToLive > timeToIdle,
    s"timeToLive($timeToLive) must be greater than timeToIdle($timeToIdle)")

  private[caching] val store = new ConcurrentLinkedHashMap.Builder[Any, Entry[V]]
    .initialCapacity(initialCapacity)
    .maximumWeightedCapacity(maxCapacity)
    .build()

  @tailrec
  final def get(key: Any): Option[Future[V]] = store.get(key) match {
    case null ⇒ None
    case entry if (isAlive(entry)) ⇒
      entry.refresh()
      Some(entry.future)
    case entry ⇒
      // remove entry, but only if it hasn't been removed and reinserted in the meantime
      if (store.remove(key, entry)) None // successfully removed
      else get(key) // nope, try again
  }

  def remove(key: Any) = store.remove(key) match {
    case null                      ⇒ None
    case entry if (isAlive(entry)) ⇒ Some(entry.future)
    case entry                     ⇒ None
  }

  def clear(): Unit = { store.clear() }

  def size = store.size

  private[caching] def isAlive(entry: Entry[V]) =
    (entry.created + timeToLive).isFuture &&
      (entry.lastAccessed + timeToIdle).isFuture
}