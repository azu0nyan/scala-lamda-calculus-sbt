package object testsGenerator {

  trait QuestionType{
    def generate(seed:Int):Question
  }

  trait Question{
    def html:String
    def answHtml:String
  }

}
