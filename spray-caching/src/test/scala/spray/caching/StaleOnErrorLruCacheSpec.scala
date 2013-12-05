package spray.caching

import scala.concurrent.duration._
import org.specs2.runner.JUnitRunner
import spray.util._

class StaleOnErrorLruCacheSpec extends ExpiringLruCacheSpec {
  import system.dispatcher

  "An expiring LruCache" should {

    "return stale values on error" in {
      val cache = lruCache[String](timeToLive = 75 millis span)
      cache(1)("A").await === "A"
      cache(2)("B").await === "B"
      Thread.sleep(50)
      cache(3)("C").await === "C"
      cache.size === 3
      Thread.sleep(50)
      cache(2)((throw new RuntimeException("Naa")): String).await === "B" // stale on error
    }

    "return stale values on error the replace when ok" in {
      val cache = lruCache[String](timeToLive = 75 millis span)
      cache(1)("A").await === "A"
      cache(2)("B").await === "B"
      Thread.sleep(50)
      cache(3)("C").await === "C"
      cache.size === 3
      Thread.sleep(50)
      cache(2)((throw new RuntimeException("Naa")): String).await === "B" // stale on error
      cache(2)("OK").await === "OK"
    }
  }

  override def lruCache[T](maxCapacity: Int = 500,
                           initialCapacity: Int = 16,
                           timeToLive: Duration = Duration.Inf,
                           timeToIdle: Duration = Duration.Inf): ExpiringCache[T] =
    new StaleOnErrorLruCache[T](maxCapacity, initialCapacity, timeToLive, timeToIdle)
}