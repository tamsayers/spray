package spray.caching

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.Promise
import scala.annotation.tailrec
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap

final class StaleOnErrorLruCache[V](val maxCapacity: Long,
                                    val initialCapacity: Int,
                                    val timeToLive: Duration,
                                    val timeToIdle: Duration) extends ExpiringCache[V] {

  def apply(key: Any, genValue: () ⇒ Future[V])(implicit ec: ExecutionContext): Future[V] = {
    def insert() = {
      val newEntry = new Entry(Promise[V]())
      val valueFuture =
        store.put(key, newEntry) match {
          case null ⇒ genValue()
          case entry ⇒
            if (isAlive(entry)) {
              // we date back the new entry we just inserted
              // in the meantime someone might have already seen the too fresh timestamp we just put in,
              // but since the original entry is also still alive this doesn't matter
              newEntry.created = entry.created
              entry.future
            } else genValue().recoverWith {
              case _ ⇒ {
                newEntry.created = entry.created
                entry.future
              }
            }
        }
      valueFuture.onComplete { value ⇒
        newEntry.promise.tryComplete(value)
        // in case of exceptions we remove the cache entry (i.e. try again later)
        if (value.isFailure) store.remove(key, newEntry)
      }
      newEntry.promise.future
    }
    store.get(key) match {
      case null ⇒ insert()
      case entry if (isAlive(entry)) ⇒
        entry.refresh()
        entry.future
      case entry ⇒ insert()
    }
  }
}