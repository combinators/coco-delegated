trait BaseExpressions {
  object BaseDefs {
    trait FinalTypes {
      type Expression
    }

    trait Expression {
      def getSelfExpression: finalTypes.Expression
    }

    trait Factory {
      def convert(exp: Expression): Expression
    }
  }

  val finalTypes: BaseDefs.FinalTypes
  val factory: BaseDefs.Factory
}

trait M0Expressions extends BaseExpressions {
  object M0Defs {
    trait FinalTypes extends BaseDefs.FinalTypes {
      type Lit
      type Add
    }

    trait Expression extends BaseDefs.Expression {
      def add(other: BaseDefs.Expression): BaseDefs.Expression
      def print: String
    }

    trait Lit extends Expression {
      def getSelfLit: finalTypes.Lit

      def value: Int

      def add(other: BaseDefs.Expression): BaseDefs.Expression = {
        factory.add(other, this)
      }
      def print: String = value.toString
    }

    trait Add extends Expression {
      def getSelfAdd: finalTypes.Add

      def left: BaseDefs.Expression
      def right: BaseDefs.Expression

      def add(other: BaseDefs.Expression): BaseDefs.Expression = {
        factory.add(other, this)
      }
      def print: String = s"(${factory.convert(left).print} + ${factory.convert(right).print})"
    }

    trait Factory extends BaseDefs.Factory {
      def convert(exp: BaseDefs.Expression): Expression
      def convert(lit: Lit): Lit
      def convert(add: Add): Add

      def lit(value: Int): Lit
      def add(left: BaseDefs.Expression, right: BaseDefs.Expression): Add
    }
  }

  val finalTypes: M0Defs.FinalTypes
  val factory: M0Defs.Factory
}

object FinalM0Expressions extends M0Expressions {
  object FinalDefs {
    trait FinalTypes extends M0Defs.FinalTypes {
      type Expression = FinalDefs.Expression
      type Lit = FinalDefs.Lit
      type Add = FinalDefs.Add
    }

    trait Expression extends M0Defs.Expression {
      override def getSelfExpression: Expression = this
    }

    case class Lit(value: Int) extends M0Defs.Lit with Expression {
      override def getSelfLit: Lit = this
    }
    case class Add(left: BaseDefs.Expression, right: BaseDefs.Expression) extends M0Defs.Add with Expression {
      override def getSelfAdd: Add = this
    }

    trait Factory extends M0Defs.Factory {
      def convert(exp: BaseDefs.Expression): FinalDefs.Expression = exp.getSelfExpression
      def convert(lit: M0Defs.Lit): FinalDefs.Lit = lit.getSelfLit
      def convert(add: M0Defs.Add): FinalDefs.Add = add.getSelfAdd
      def lit(value: Int): FinalDefs.Lit = Lit(value)
      def add(left: BaseDefs.Expression, right: BaseDefs.Expression): FinalDefs.Add = Add(left, right)
    }
  }

  val finalTypes: FinalDefs.FinalTypes = new FinalDefs.FinalTypes {}
  val factory: FinalDefs.Factory = new FinalDefs.Factory {}
}

trait BaseStatements {
  object BaseDefs {
    trait FinalTypes {
      type Statement
    }

    trait Statement {
      def getSelfStatement: finalTypes.Statement
    }

    trait Factory {
      def convert(stmt: Statement): Statement
    }
  }

  val finalTypes: BaseDefs.FinalTypes
  val factory: BaseDefs.Factory
}

trait M0Statements extends BaseStatements {
  object M0Defs {
    trait FinalTypes extends BaseDefs.FinalTypes {
      type If
      type Block
      type PrintLn
    }

    trait Statement extends BaseDefs.Statement {
      def print: String
    }

    trait If extends BaseDefs.Statement {
      def getSelfIf: finalTypes.If

      def condition: expressions.BaseDefs.Expression
      def thenStmt: BaseDefs.Statement

      def print: String = s"if (${expressions.factory.convert(this.condition).print}) ${factory.convert(this.thenStmt).print}"
    }

    trait Block extends BaseDefs.Statement {
      def getSelfBlock: finalTypes.Block

      def stmts: Seq[BaseDefs.Statement]

      def print: String = stmts.map(stmt => factory.convert(stmt).print).mkString("{\n\t", "\n\t", "\n}\n")
    }

    trait Println extends BaseDefs.Statement {
      def getSelfPrintln: finalTypes.PrintLn

      def printExpression: expressions.BaseDefs.Expression

      def print: String = s"println(${expressions.factory.convert(printExpression).print})"
    }

    trait Factory extends BaseDefs.Factory {
      def convert(stmt: BaseDefs.Statement): Statement
      def convert(ifstm: If): If
      def convert(block: Block): Block
      def convert(println: Println): Println

      def ifStmt(condition: expressions.BaseDefs.Expression, thenBlock: BaseDefs.Statement): If
      def block(stmts: Seq[BaseDefs.Statement]): Block
      def println(printExpression: expressions.BaseDefs.Expression): Println
    }
  }

  val finalTypes: M0Defs.FinalTypes
  val factory: M0Defs.Factory
  val expressions: M0Expressions
}

class FinalM0Statements[E <: M0Expressions](val expressions: E) extends M0Statements {
  object FinalDefs {
    trait FinalTypes extends M0Defs.FinalTypes {
      type Statement = FinalDefs.Statement
      type If = FinalDefs.If
      type Block = FinalDefs.Block
      type PrintLn = FinalDefs.PrintLn
    }

    trait Statement extends M0Defs.Statement {
      override def getSelfStatement: finalTypes.Statement = this
    }
    case class If(condition: expressions.BaseDefs.Expression, thenStmt: BaseDefs.Statement) extends M0Defs.If with Statement {
      override def getSelfIf: finalTypes.If = this
    }
    case class Block(stmts: Seq[BaseDefs.Statement]) extends M0Defs.Block with Statement {
      override def getSelfBlock: finalTypes.Block = this
    }
    case class PrintLn(printExpression: expressions.BaseDefs.Expression) extends M0Defs.Println with Statement {
      override def getSelfPrintln: finalTypes.PrintLn = this
    }

    trait Factory extends M0Defs.Factory {
      def convert(stmt: BaseDefs.Statement): Statement = stmt.getSelfStatement
      def convert(ifstm: M0Defs.If): If = ifstm.getSelfIf
      def convert(block: M0Defs.Block): Block = block.getSelfBlock
      def convert(println: M0Defs.Println): PrintLn = println.getSelfPrintln

      def ifStmt(condition: expressions.BaseDefs.Expression, thenBlock: BaseDefs.Statement): If = If(condition, thenBlock)
      def block(stmts: Seq[BaseDefs.Statement]): Block = Block(stmts)
      def println(printExpression: expressions.BaseDefs.Expression): PrintLn = PrintLn(printExpression)
    }
  }

  val finalTypes: FinalDefs.FinalTypes = new FinalDefs.FinalTypes {}
  val factory: FinalDefs.Factory = new FinalDefs.Factory {}
}


object Main extends App {
  val expressions: FinalM0Expressions.type = FinalM0Expressions
  val statements = new FinalM0Statements(expressions)

  import expressions.factory.*
  import statements.factory.*
  val one = lit(1)
  val complex = add(lit(1), add(lit(2), lit(3)))
  val ifPrint = ifStmt(one, block(Seq(println(complex), println(one))))
  print(ifPrint.print)
}
