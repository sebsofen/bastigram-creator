import de.bastigram.post.postentities.PostEntity
import specs.BaseSpec

class PostEntityTest extends BaseSpec {

  "PostEntity object function strToArgMap" should
    "parse a=\"b\" " in {
    PostEntity.strToArgMap("a=\"b\"") should equal(Map("a" -> "b"))
  }
  it should "parse a=b with tailing whitespace" in {
    PostEntity.strToArgMap("a=b ") should equal(Map("a" -> "b"))
  }
  it should "parse a=b without tailing whitespace" in {

    PostEntity.strToArgMap("a=b]") should equal(Map("a" -> "b"))
  }
}
