package testsuite.javalib.text

import java.text.AttributedCharacterIterator.Attribute
import testsuite.utils.LocaleTestSetup
import utest._

object AttributedCharacterIteratorTest extends TestSuite with LocaleTestSetup {
  val tests = Tests {
    'test_static_value_to_string - {
      assertEquals("java.text.AttributedCharacterIterator$Attribute(language)",
                   Attribute.LANGUAGE.toString)
      assertEquals("java.text.AttributedCharacterIterator$Attribute(reading)",
                   Attribute.READING.toString)
      assertEquals(
        "java.text.AttributedCharacterIterator$Attribute(input_method_segment)",
        Attribute.INPUT_METHOD_SEGMENT.toString)
    }

    'test_equals - {
      assertEquals(Attribute.LANGUAGE, Attribute.LANGUAGE)
      assertFalse(Attribute.READING == Attribute.LANGUAGE)
    }
  }
}
