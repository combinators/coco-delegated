import FinalM0Expressions.finalTypes

import scala.language.implicitConversions

trait BaseExpressions {
  object BaseDefs {
    trait FinalTypes {
      type Expression <: BaseDefs.Expression
    }

    trait Expression {
      def getSelfExpression: finalTypes.Expression
    }

    trait Factory {
      implicit def convert(exp: Expression): finalTypes.Expression = exp.getSelfExpression
    }
  }

  val finalTypes: BaseDefs.FinalTypes
  val factory: BaseDefs.Factory
}

trait M0Expressions extends BaseExpressions {
  object M0Defs {
    trait FinalTypes extends BaseDefs.FinalTypes {
      type Expression <: M0Defs.Expression
      type Lit <: M0Defs.Lit
      type Add <: M0Defs.Add
    }

    trait Expression extends BaseDefs.Expression {
      def add(other: BaseDefs.Expression): BaseDefs.Expression
      def print: String
    }

    trait Lit(val value: Int) extends Expression {
      def getSelfLit: finalTypes.Lit

      def add(other: BaseDefs.Expression): BaseDefs.Expression = factory.add(other, this)
      def print: String = value.toString
    }

    trait Add(val left: BaseDefs.Expression, val right: BaseDefs.Expression) extends Expression {
      def getSelfAdd: finalTypes.Add

      def add(other: BaseDefs.Expression): BaseDefs.Expression = factory.add(other, this)
      def print: String = {
        import factory._
        s"(${left.print} + ${right.print})"
      }
    }

    trait Factory extends BaseDefs.Factory {
      implicit def convert(lit: Lit): finalTypes.Lit = lit.getSelfLit
      implicit def convert(add: Add): finalTypes.Add = add.getSelfAdd

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
      type Expression = M0Defs.Expression
      type Lit = M0Defs.Lit
      type Add = M0Defs.Add
    }

    trait Expression extends M0Defs.Expression {
      override def getSelfExpression: Expression = this
    }

    trait Factory extends M0Defs.Factory {
      def lit(value: Int): finalTypes.Lit = new M0Defs.Lit(value) with Expression {
        def getSelfLit: finalTypes.Lit = this
      }
      def add(left: BaseDefs.Expression, right: BaseDefs.Expression): finalTypes.Add = new M0Defs.Add(left, right) with Expression {
        def getSelfAdd: finalTypes.Add = this
      }
    }
  }

  val finalTypes: FinalDefs.FinalTypes = new FinalDefs.FinalTypes {}
  val factory: FinalDefs.Factory = new FinalDefs.Factory {}
}

trait BaseStatements {
  object BaseDefs {
    trait FinalTypes {
      type Statement <: BaseDefs.Statement
    }

    trait Statement {
      def getSelfStatement: finalTypes.Statement
    }

    trait Factory {
      implicit def convert(stmt: Statement): finalTypes.Statement = stmt.getSelfStatement
    }
  }

  val finalTypes: BaseDefs.FinalTypes
  val factory: BaseDefs.Factory
}

trait M0Statements extends BaseStatements {
  object M0Defs {
    trait FinalTypes extends BaseDefs.FinalTypes {
      type Statement <: M0Defs.Statement
      type If <: M0Defs.If
      type Block <: M0Defs.Block
      type PrintLn <: M0Defs.PrintLn
    }

    trait Statement extends BaseDefs.Statement {
      def print: String
    }

    trait If(val condition: expressions.BaseDefs.Expression, val thenStmt: BaseDefs.Statement) extends BaseDefs.Statement {
      def getSelfIf: finalTypes.If

      def print: String = {
        import expressions.factory._
        import factory._
        s"if (${condition.print}) ${thenStmt.print}"
      }
    }

    trait Block(val stmts: Seq[BaseDefs.Statement]) extends BaseDefs.Statement {
      def getSelfBlock: finalTypes.Block

      def print: String = {
        import factory._
        stmts.map(stmt => stmt.print).mkString("{\n\t", "\n\t", "\n}\n")
      }
    }

    trait PrintLn(val printExpression: expressions.BaseDefs.Expression) extends BaseDefs.Statement {
      def getSelfPrintln: finalTypes.PrintLn

      def print: String = {
        import expressions.factory._
        s"println(${printExpression.print})"
      }
    }

    trait Factory extends BaseDefs.Factory {
      implicit def convert(ifstm: If): finalTypes.If = ifstm.getSelfIf
      implicit def convert(block: Block): finalTypes.Block = block.getSelfBlock
      implicit def convert(println: PrintLn): finalTypes.PrintLn = println.getSelfPrintln

      def ifStmt(condition: expressions.BaseDefs.Expression, thenBlock: BaseDefs.Statement): If
      def block(stmts: Seq[BaseDefs.Statement]): Block
      def println(printExpression: expressions.BaseDefs.Expression): PrintLn
    }
  }

  val finalTypes: M0Defs.FinalTypes
  val factory: M0Defs.Factory
  val expressions: M0Expressions
}

class FinalM0Statements[E <: M0Expressions](val expressions: E) extends M0Statements {
  object FinalDefs {
    trait FinalTypes extends M0Defs.FinalTypes {
      type Statement = M0Defs.Statement
      type If = M0Defs.If
      type Block = M0Defs.Block
      type PrintLn = M0Defs.PrintLn
    }

    trait Statement extends M0Defs.Statement {
      override def getSelfStatement: finalTypes.Statement = this
    }


    trait Factory extends M0Defs.Factory {
      def ifStmt(condition: expressions.BaseDefs.Expression, thenBlock: BaseDefs.Statement) = new M0Defs.If(condition, thenBlock) with Statement {
        def getSelfIf: M0Defs.If = this
      }
      def block(stmts: Seq[BaseDefs.Statement]) = new M0Defs.Block(stmts) with Statement {
        def getSelfBlock: M0Defs.Block = this
      }
      def println(printExpression: expressions.BaseDefs.Expression) = new M0Defs.PrintLn(printExpression) with Statement {
        def getSelfPrintln = this
      }
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
