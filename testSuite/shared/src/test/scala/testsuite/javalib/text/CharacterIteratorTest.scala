package testsuite.javalib.text

import java.text.CharacterIterator
import testsuite.utils.LocaleTestSetup
import utest._

object CharacterIteratorTest extends TestSuite with LocaleTestSetup {
  val tests = Tests {
    'test_done - {
      assertEquals('\uFFFF', CharacterIterator.DONE)
    }
  }
}
