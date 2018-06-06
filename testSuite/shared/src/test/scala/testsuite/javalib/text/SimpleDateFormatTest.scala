package testsuite.javalib.text

import java.util.Date
import java.text.SimpleDateFormat

import utest._

import io.github.cquiroz.utils.JVMDate
import testsuite.utils.LocaleTestSetup
import scala.language.reflectiveCalls

object SimpleDateFormatTest extends TestSuite with LocaleTestSetup {
  final val time = 1491381282242L
  val date = new Date(time)
  val jvmDate = JVMDate.get(time)
  val year = totalSize((1900 + jvmDate.year), 4)
  val month = totalSize(1 + jvmDate.month, 2)
  val day = totalSize(jvmDate.date, 2)
  val hours = totalSize(jvmDate.hours, 2)
  val minutes = totalSize(jvmDate.minutes, 2)
  val seconds = totalSize(jvmDate.seconds, 2)
  val millis = totalSize(date.getTime - Date.UTC(date.getYear,
     date.getMonth,
     date.getDay,
     date.getHours,
     date.getMinutes,
     date.getSeconds), 3)

  def totalSize(num: Long, size: Int): String = {
    val s: String = num.toString

    if (s.size > size) s.substring(s.size - size)
    else s.reverse.padTo(size, '0').reverse
  }

  val tests = Tests {
    'test_year_format - {
      val f = new SimpleDateFormat("yyyy")
      assertEquals(year, f.format(date))
    }

    'test_month_format - {
      val f = new SimpleDateFormat("MM")
      assertEquals(month, f.format(date))
    }

    'test_day_format - {
      val f = new SimpleDateFormat("dd")
      assertEquals(day, f.format(date))
    }

    'test_hour_format - {
      val f = new SimpleDateFormat("HH")
      assertEquals(hours, f.format(date))
    }

    'test_minutes_format - {
      val f = new SimpleDateFormat("mm")
      assertEquals(minutes, f.format(date))
    }

    'test_seconds_format - {
      val f = new SimpleDateFormat("ss")
      assertEquals(seconds, f.format(date))
    }

    'test_millis_format - {
      val f = new SimpleDateFormat("SSS")
      assertEquals(millis, f.format(date))
    }

    'test_full_date_format - {
      val f = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss:SSS")
      assertEquals(s"$day/$month/$year $hours:$minutes:$seconds:$millis",
        f.format(date))
    }
  }
}
