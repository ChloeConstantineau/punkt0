package io.punkt0.analyzer

import io.punkt0.{Context, Phase}
import io.punkt0.analyzer.Symbols._
import io.punkt0.analyzer.Types._
import io.punkt0.ast.Trees._

import scala.collection.mutable.ListBuffer

object NameAnalysis extends Phase[Program, Program] {

  val acceptableAssignType = List(
    New.toString(),
    Null.toString(),
    IntLit.toString(),
    StringLit.toString(),
    True.toString(),
    False.toString
  )

  def run(prog: Program)(ctx: Context): Program = {
    var globalScope = new GlobalScope()

    def getType(tpe: TypeTree): Type = tpe match {
      case BooleanType() => TBoolean
      case StringType()  => TString
      case IntType()     => TInt
      case UnitType()    => TUnit
      case Identifier(value) => {
        import io.punkt0.Reporter //Check Class exists
        if (!globalScope.classes.contains(value))
          Reporter.fatal(
            "This type (" + value + ") is not declared, it cannot be used",
            tpe
          )

        val classSymbol = globalScope.classes.get(value).get
        TClass(classSymbol)
      }
      case _ => TUntyped
    }

    def makeVariableSymbol(aTree: Tree): VariableSymbol = aTree match {
      case VarDecl(tpe, id, expr) =>
        new VariableSymbol(id.value, getType(tpe)).setPos(aTree)
      case Formal(tpe, id) =>
        new VariableSymbol(id.value, getType(tpe)).setPos(aTree)
      case _ =>
        import io.punkt0.Reporter
        Reporter.fatal(
          "Expected a varriable declaration or a formal declaration",
          aTree
        )
    }

    def makeMethodSymbol(
        method: MethodDecl,
        classSymbol: ClassSymbol
    ): MethodSymbol = {

      verifyOverrideConstraints(method, classSymbol)

      val methodType = getType(method.retType)
      val newMethodSymbol =
        new MethodSymbol(method.id.value, classSymbol, methodType)
          .setPos(method)

      newMethodSymbol.params = attachMethodParams(method.args)
      newMethodSymbol.members = attachMembers(method.vars)

      if (method.overrides) {
        var overridenMethodSymbol =
          classSymbol.lookupMethod(method.id.toString())
        newMethodSymbol.overridden = overridenMethodSymbol
      }

      newMethodSymbol
    }

    def verifyOverrideConstraints(
        method: MethodDecl,
        classSymbol: ClassSymbol
    ): Unit = {
      var parent: Option[ClassSymbol] = classSymbol.parent
      var matchingMethod: Option[MethodSymbol] = None

      if (method.overrides) {
        import io.punkt0.Reporter

        //Verify Class has a parent to override
        if (!parent.isDefined)
          Reporter.error(
            "This class has no parent, there can be no overriding of methods",
            method
          )

        //Get method in parent tree if exists
        matchingMethod = parent.get.lookupMethod(method.id.value)

        //Verify the parent tree contains the method being overriden
        if (!matchingMethod.isDefined)
          Reporter.error(
            "The parent tree does not contains such a method, overriding does not apply",
            method
          )

        //Both methods have the same number of parameters
        if (matchingMethod.get.params.size != method.args.size)
          Reporter.error(
            "This method does not have the same number of parameters as the method it overrides",
            method
          )

        //Return type must match
        if (!matchingMethod.get.tpe.equals(getType(method.retType)))
          Reporter.error(
            "The return type of the overridden fonction does not match the original, it needs to be of type: " + matchingMethod.get.tpe,
            method
          )

        //Match types of  params of both methods
        for ((param, i) <- matchingMethod.get.params.zipWithIndex) {
          if (!param._2.tpe.equals(getType(method.args(i).tpe)))
            Reporter.error(
              "The param in the overriding method does not match the original signature of the method, the parameter " + method
                .args(i)
                .id + " has the wrong type",
              method.args(i)
            )
        }
      }

      //No method has same name in class and his parents unless the class method has 'override'
      if (
        !method.overrides && classSymbol
          .lookupMethodInParentTree(method.id.value)
          .isDefined
      ) {
        import io.punkt0.Reporter
        Reporter.error(
          "This method has already been declared in the parent tree. Solution is to override the method",
          method
        )
      }
    }

    //First phase: get all classes to have defined token
    def attachClasses(): Unit = {

      //Iterates every class in program
      for (i <- 0 until prog.classes.length) {

        //Verify General Constraints
        checkConstraintClassDecl(prog.classes(i))

        //Create new Class Symbol
        var newClassSymbol =
          new ClassSymbol(prog.classes(i).id.value).setPos(prog.classes(i))
        prog.classes(i).setSymbol(newClassSymbol)

        //Add symbol class to global classes map
        globalScope.classes += (prog.classes(i).id.value -> newClassSymbol)
      }
    }

    def checkConstraintClassDecl(classDecl: ClassDecl): Unit = {
      import io.punkt0.Reporter

      //Cannot define Main more than once
      if (classDecl.id.value.equals("Main"))
        Reporter.error(
          "Main was already declared at line" + globalScope.mainClass.posString,
          classDecl
        )

      //Class has already been declared
      if (globalScope.lookupClass(classDecl.id.value) != None)
        Reporter.error(
          "A class with this name has already been declared at line " + globalScope
            .lookupClass(classDecl.id.value)
            .get
            .posString,
          classDecl
        )
    }

    //Second phase: Attach parent classes to token classes and verify parent constraints
    def attachParentClasses(): Unit = {

      for (i <- 0 until prog.classes.length) {
        var classSymbol = globalScope.classes.get(prog.classes(i).id.value).get

        if (prog.classes(i).parent.isDefined) {
          //Verify Parent Constraints
          checkParentConstraintClassDecl(prog.classes(i), classSymbol)

          //Attach Parent Symbol if Parent is Defined
          classSymbol.parent =
            globalScope.classes.get(prog.classes(i).parent.get.value)
        }
      }

      //Once all parents are assign, verify there is no cyclic dependence among classes
      searchCyclicClassDependence()
    }

    def checkParentConstraintClassDecl(
        classDecl: ClassDecl,
        newClassSymbol: ClassSymbol
    ): Unit = {
      import io.punkt0.Reporter
      //Check Parent Class Exists
      if (!globalScope.classes.contains(classDecl.parent.get.value))
        Reporter.error(
          "The parent class is not declared: " + classDecl.parent.get.value,
          classDecl.parent.get
        )
    }

    def searchCyclicClassDependence(): Unit = {
      for (i <- 0 until prog.classes.length) {
        var classSymbol = globalScope.classes.get(prog.classes(i).id.value).get

        if (prog.classes(i).parent.isDefined) {
          preventCyclicClassDependency(classSymbol)
        }
      }
    }

    def preventCyclicClassDependency(classSymbol: ClassSymbol): Unit = {

      var visitedClasses = new ListBuffer[ClassSymbol]

      //Add current class to list
      visitedClasses.+=(classSymbol)

      //Traverse parent tree starting with grandparent
      if (classSymbol.parent.get.parent.isDefined) {
        var nextGeneration = classSymbol.parent.get.parent;

        while (nextGeneration.isDefined) {
          import io.punkt0.Reporter
          if (visitedClasses.contains(nextGeneration.get))
            Reporter.fatal(
              "The class declared has a cycle of dependendies with its parents",
              classSymbol
            )
          else {
            visitedClasses.+=(nextGeneration.get)
            nextGeneration = nextGeneration.get.parent
          }
        }
      }
    }

    def attachMain(): Unit = {
      //Create Main symbol
      globalScope.mainClass = new ClassSymbol(prog.main.obj.value)

      //Create Main Parent
      if (!prog.main.parent.value.equals("App")) {
        import io.punkt0.Reporter
        Reporter.error("Main.parent must be App", prog.main)
      }
      globalScope.mainClass.parent = Some(
        new ClassSymbol(prog.main.parent.value)
      )

      //Set both symbols
      prog.main.setSymbol(globalScope.mainClass)
      prog.main.parent.setSymbol(globalScope.mainClass.parent.get)
    }

    def analyseClasses(): Unit = {
      for (i <- 0 until prog.classes.length if prog.classes.length.>(0)) {

        var aClass = prog.classes(i)
        var aClassSymbol = globalScope.lookupClass(aClass.id.value).get

        //Attach Members
        aClassSymbol.members = attachMembers(aClass.vars)

        //Analyse member assign
        analyseVarAssign(prog.classes(i).vars, aClassSymbol, None)

        //Attach Methods
        aClassSymbol.methods = attachMethods(aClassSymbol, aClass.methods)
      }
    }

    def analyseMainClass(): Unit = {
      //Attach Members
      globalScope.mainClass.members = attachMembers(prog.main.vars)

      //Analyse member assign
      analyseVarAssign(prog.main.vars, globalScope.mainClass, None)

      //Attach Identifiers from exprs if present
      for (i <- 0 until prog.main.exprs.length if prog.main.exprs.length.>(0)) {
        getExpr(prog.main.exprs(i), globalScope.mainClass, None)
      }
    }

    def analyseVarAssign(
        vars: List[VarDecl],
        classSymbol: ClassSymbol,
        methodSymbol: Option[MethodSymbol]
    ): Unit = {

      for (i <- 0 until vars.length if vars.length.>(0)) {
        import io.punkt0.Reporter

        //Check type variable is of Id type and if this class exists
        if (
          vars(i).tpe.getClass.getSimpleName
            .equals(Identifier.toString()) && !globalScope.classes.contains(
            vars(i).tpe.toString.slice(11, vars(i).tpe.toString.length() - 1)
          )
        )
          Reporter.fatal(
            "This type (" + vars(i).tpe
              .toString() + ") is not declared, it cannot be used",
            vars(i)
          )

        //Verify initializer is either constant or New
        checkAssignIsValid(vars(i).expr)

        var assignExprClass = vars(i).expr.getClass.getSimpleName

        getExpr(vars(i).expr, classSymbol, methodSymbol)

      }
    }

    def checkAssignIsValid(variable: ExprTree): Unit = {
      if (!acceptableAssignType.contains(variable.getClass.getSimpleName)) {
        import io.punkt0.Reporter
        Reporter.error(
          "This variable assign is not permitted : " + variable.getClass.getSimpleName + ". Must be of type : New, Null, Boolean, IntLit or StringLit.",
          variable
        )
      }
    }

    def attachMembers(vars: List[VarDecl]): Map[String, VariableSymbol] = {
      var mapMembers = scala.collection.mutable.Map[String, VariableSymbol]()

      for (i <- 0 until vars.length if vars.length.>(0)) {
        import io.punkt0.Reporter
        if (!mapMembers.contains(vars(i).id.value)) {

          var variableSymbol = makeVariableSymbol(vars(i))
          vars(i).setSymbol(variableSymbol)
          mapMembers += (vars(i).id.value -> variableSymbol)

        } else
          Reporter.error(
            "This variable has already been declared at line " + mapMembers
              .get(vars(i).id.value)
              .get
              .posString,
            vars(i)
          )
      }

      mapMembers.toMap
    }

    def attachMethods(
        classSymbol: ClassSymbol,
        methods: List[MethodDecl]
    ): Map[String, MethodSymbol] = {
      var mapMethods = scala.collection.mutable.Map[String, MethodSymbol]()

      for (i <- 0 until methods.length if (methods.length.>(0))) {
        import io.punkt0.Reporter
        if (!mapMethods.contains(methods(i).id.value)) {

          var methodSymbol = makeMethodSymbol(methods(i), classSymbol)
          methods(i).setSymbol(methodSymbol)

          //Analyse variable assigns
          analyseVarAssign(methods(i).vars, classSymbol, Some(methodSymbol))

          //Attach identifers in method
          var allExprs = new ListBuffer[ExprTree]
          allExprs.++=(methods(i).exprs)
          allExprs.+=(methods(i).retExpr)

          for (j <- 0 until allExprs.length) {
            getExpr(allExprs(j), classSymbol, Some(methodSymbol))
          }

          //Add method symbol to map
          mapMethods += (methods(i).id.value -> methodSymbol)
        } else
          Reporter.error(
            "This method has already been declared here " + mapMethods
              .get(methods(i).id.value)
              .get
              .posString,
            methods(i)
          )
      }

      mapMethods.toMap
    }

    def attachMethodParams(
        params: List[Formal]
    ): Map[String, VariableSymbol] = {
      var mapParams = scala.collection.immutable.Map[String, VariableSymbol]()

      if (params.length.>(0)) {
        for (i <- 0 until params.length) {
          import io.punkt0.Reporter
          if (mapParams.contains(params(i).id.value))
            Reporter.error("This params has already been declared")

          var variableSymbol = makeVariableSymbol(params(i))

          params(i).setSymbol(variableSymbol)
          params(i).setType(getType(params(i).tpe))
          mapParams += (params(i).id.value -> variableSymbol)

        }
      }
      mapParams.toMap
    }

    def getExpr(
        expr: ExprTree,
        classSymbol: ClassSymbol,
        methodSymbol: Option[MethodSymbol]
    ): Unit = {
      expr match {

        case Not(expr) => {
          getExpr(expr, classSymbol, methodSymbol)
        }
        case Println(expr) => {
          getExpr(expr, classSymbol, methodSymbol)
        }
        case While(expr, block) => {
          getExpr(expr, classSymbol, methodSymbol)
          getExpr(block, classSymbol, methodSymbol)
        }
        case If(expr, thn, elseExpr) => {
          getExpr(expr, classSymbol, methodSymbol)
          getExpr(thn, classSymbol, methodSymbol)
          if (elseExpr.isDefined) {
            getExpr(elseExpr.get, classSymbol, methodSymbol)
          }
        }
        case Block(listExpr) => {
          for (i <- 0 until listExpr.length) {
            getExpr(listExpr(i), classSymbol, methodSymbol)
          }
        }
        case Assign(id, expr) => {
          getExpr(id, classSymbol, methodSymbol)
          getExpr(expr, classSymbol, methodSymbol)
        }
        case And(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Or(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Plus(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Minus(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Times(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Div(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case LessThan(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case Equals(lhs, rhs) => {
          getExpr(lhs, classSymbol, methodSymbol)
          getExpr(rhs, classSymbol, methodSymbol)
        }
        case MethodCall(obj, method, args) => {
          getExpr(obj, classSymbol, methodSymbol)
          for (i <- 0 until args.length) {
            getExpr(args(i), classSymbol, methodSymbol)
          }
        }
        case _ => getExprInternal(expr, classSymbol, methodSymbol)
      }
    }

    def getExprInternal(
        expr: ExprTree,
        classSymbol: ClassSymbol,
        methodSymbol: Option[MethodSymbol]
    ): Unit = {
      expr match {
        case Identifier(id) => {

          //Is a method member or param
          if (
            methodSymbol.isDefined && methodSymbol.get.lookupVar(id).isDefined
          ) {
            expr
              .asInstanceOf[Identifier]
              .setSymbol(methodSymbol.get.lookupVar(id).get)

            //Is a class member
          } else if (classSymbol.lookupVar(id).isDefined) {
            expr
              .asInstanceOf[Identifier]
              .setSymbol(classSymbol.lookupVar(id).get)

            //Case where variable of superclass is used, create current class symbol instance, set symbol and add to class members
          } else if (classSymbol.lookupVar(id).isDefined) {

            val inheritedMember = classSymbol.lookupVar(id).get
            val newMemberSymbol =
              new VariableSymbol(inheritedMember.name, inheritedMember.tpe)

            expr.asInstanceOf[Identifier].setSymbol(newMemberSymbol)
            globalScope.classes
              .get(classSymbol.name)
              .get
              .members += newMemberSymbol.name -> newMemberSymbol

          } else {
            import io.punkt0.Reporter
            Reporter.error(
              "This variable has never been declared or is of type not definied"
            )
          }
        }

        case New(id) => getExprIdentifier(id, classSymbol)
        case This()  => expr.setType(TClass(classSymbol))
        case _       => None
      }

      def getExprIdentifier(expr: ExprTree, classSymbol: ClassSymbol): Unit = {
        expr match {
          case Identifier(id) => {
            import io.punkt0.Reporter
            if (globalScope.lookupClass(id).isDefined) {
              expr
                .asInstanceOf[Identifier]
                .setSymbol(globalScope.lookupClass(id).get)
            } else
              Reporter.error(
                "Cannot create an object of class " + id + ". This class has not been defined",
                expr
              )
          }
          case _ =>
            import io.punkt0.Reporter
            Reporter.fatal("Expected an Identifier..")
        }
      }
    }

    //Execute all
    attachMain()
    attachClasses()
    attachParentClasses()
    analyseMainClass()
    analyseClasses()

    prog
  }

}
