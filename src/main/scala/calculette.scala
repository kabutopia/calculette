package calculette

/**
 * sealed garanti que Tree ne pourra pas être étendu
 * c'est ainsi un Algebraic Data Type
 */
sealed trait Tree
final case class Node(op: Operator, left: Tree, right: Tree) extends Tree
final case class Leaf(value: Int) extends Tree

trait Operator
case object + extends Operator
case object - extends Operator
//case object * extends Operator
//case object / extends Operator

sealed trait Token
final case class OpToken(op: String) extends Token
final case class DigitToken(digits: String) extends Token

object Calculette {
    type Error = String
    type ParseResult = Either[Error, Tree]
    type TokenResult = Either[Error, List[Token]]

    def tokenization(str: String): TokenResult =  {
        // val strclean = sr.replace(" ", "")
        def doTheJob(str2: String, acc: List[Token]): TokenResult = {
            if (str2.isEmpty) Right(acc.reverse) // Either-isation
            else str2.head match {
                case '+' => doTheJob(str2.tail, OpToken("+") :: acc )
                case '-' => doTheJob(str2.tail, OpToken("-") :: acc )
                case x if x.isDigit => 
                    doTheJob(str2.dropWhile(_.isDigit), DigitToken(str2.takeWhile(_.isDigit)) :: acc )
                case ' ' => doTheJob(str2.tail, acc )
                case y => Left(s"""Failed, unexpected "$y" symbol""")
            }
        }
        doTheJob(str, Nil)
    }

    def makeOp(s: String): Operator = {
        s match {
            case "+" => +
            case "-" => -
        }
    }

    def makeTree(tks: List[Token]): ParseResult = {
        tks match {
            case DigitToken(d) :: OpToken(o) :: tail => 
                makeTree(tail).map( subtree => Node(makeOp(o), Leaf(d.toInt), subtree))
            case DigitToken(d) :: Nil => Right(Leaf(d.toInt))
            case _ => Left(s"""Error""")
        }
    }   

    def parser(str: String) : ParseResult = 
        for {
            // tokenization returns an Either, not a List[Token]
            // la function map et flatMap de Either fait que ca marche
            tokens <- tokenization(str)
            tree   <- makeTree(tokens)
        } yield tree
        

    def evaluate(tree: Tree) : Int = ???

    def apply(str: String) : Int =
        for {
            tree <- parser(str)
        } yield evaluate(tree)
}

