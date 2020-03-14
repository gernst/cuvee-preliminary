package cuvee

import cuvee.Type.array

object Check {
  def infer(expr: Expr, ty: Map[Id, Type], st: State): Type = expr match {
    case _: Num =>
      Sort.int

    case id: Id if (ty contains id) =>
      ty(id)

    case id: Id if (st.funs contains id) =>
      val (ts, tr) = st funs id
      ensure(ts.isEmpty, s"not a constant: $id", expr)
      tr

    case _: Id =>
      error(s"unknown identifier $expr")

    case Note(expr, _) =>
      infer(expr, ty, st)

    case UMinus(arg) =>
      val typ = infer(arg, ty, st)
      ensure(typ == Sort.int, s"argument for unary minus must be integer but was $typ", expr)
      Sort.int

    case App(id, args) if (st.funs contains id) =>
      val ts1 = args map (infer(_, ty, st))
      val (ts2, tr) = st funs id
      ensure(ts1 == ts2, s"arguments for $id do not match function signature. Expected $ts2 but got $ts1", expr)
      tr

    case App(id, _) =>
      error(s"unknown function $id", expr)

    case Bind(_, formals, body) =>
      val ts: List[(Id, Type)] = formals
      val tr = infer(body, ty ++ ts, st)
      ensure(tr == Sort.bool, s"body of bind must be boolean but was $tr", expr)
      tr

    case Old(expr) =>
      infer(expr, ty, st)

    case Eq(left, right) =>
      val lt = infer(left, ty, st)
      val rt = infer(right, ty, st)
      ensure(lt == rt, s"types of sides of equals must match but were $lt and $rt", expr)
      Sort.bool

    case Distinct(exprs) =>
      val types = exprs.map(infer(_, ty, st)).distinct
      ensure(types.length == 1, s"types of arguments of distinct must match but were ${types mkString " != "}", expr)
      Sort.bool

    case Ite(test, left, right) =>
      val tt = infer(test, ty, st)
      ensure(tt == Sort.bool, s"test of if-then-else expression must be boolean but was $tt", expr)
      val lt = infer(left, ty, st)
      val rt = infer(right, ty, st)
      ensure(lt == rt, s"types of then- and else-clause must match but were $lt != $rt", expr)
      lt

    case WP(prog, post) => inferWpLike(prog, post, ty, st)
    case Box(prog, post) => inferWpLike(prog, post, ty, st)
    case Dia(prog, post) => inferWpLike(prog, post, ty, st)

    case Let(pairs, body) =>
      val vars = pairs.map(pair => pair.x -> infer(pair.e, ty, st)).toMap
      infer(body, ty ++ vars, st)

    case Match(expr, cases) =>
      infer(expr, ty, st) match {
        case sort: Sort =>
          ensure(st.datatypes contains sort, s"trying to match non-datatype $sort", expr)
          val types = for (Case(pat, expr) <- cases) yield {
            inferCase(sort, pat, expr, ty, st)
          }
          ensure(types.distinct.size == 1, s"match cases must return equal type but found ${types.distinct.mkString(", ")}", expr)
          types.head
        case typ => error(s"can only match sorts, found $typ", expr)
      }

    case Select(arr, index) => infer(arr, ty, st) match {
      case array(dom, ran) =>
        val idxType = infer(index, ty, st)
        ensure(idxType == dom, s"expected index to be of type $dom byt was $idxType", expr)
        ran
      case other =>
        error(s"expected array type but got $other", expr)
    }

    case Store(arr, index, value) => infer(arr, ty, st) match {
      case array(dom, ran) =>
        val idxType = infer(index, ty, st)
        ensure(idxType == dom, s"expected index to be of type $dom byt was $idxType", expr)
        val valType = infer(value, ty, st)
        ensure(valType == ran, s"expected value to be stored in array of type $ran but was $valType", expr)
        array(dom, ran)
      case other =>
        error(s"expected array type but got $other", expr)
    }
  }

  private def inferCase(typ: Type, pat: Pat, expr: Expr, ty: Map[Id, Type], st: State): Type = pat match {
    case id: Id => infer(expr, ty + (id -> typ), st)
    case UnApp(fun, args) =>
      ensure(st.funs contains fun, s"unknown constructor $fun", pat)
      val (constrTypes, datatype) = st.funs(fun)
      ensure(typ == datatype, s"trying to match $typ with $datatype-constructor $fun", pat)
      ensure(args.length == constrTypes.length, s"wrong number of arguments for $fun", pat)
      infer(expr, ty ++ args.zip(constrTypes), st)
  }

  private def inferWpLike(prog: Prog, post: Expr, ty: Map[Id, Type], st: State): Type = {
    ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", post)
    checkProg(prog, ty, st, false)
    Sort.bool
  }

  /**
   * @param loop true if this program is in a loop. This allows the break statement.
   */
  def checkProg(prog: Prog, ty: Map[Id, Type], st: State, loop: Boolean): Unit = prog match {
    case Block(progs, withOld) =>
      for (prog <- progs)
        checkProg(prog, ty, st, loop)

    case Break =>
      ensure(loop, "break most only occur in while")

    case Assign(pairs) =>
      for (Pair(id, value) <- pairs) {
        val expected: Type = if (ty contains id) {
          ty(id)
        } else if (st.funs.contains(id) && st.funs(id)._1.isEmpty) {
          // is this okay tho? it's called a _constant_
          // this is used in compare.smt2
          st.funs(id)._2
        } else {
          error(s"unknown variable $id")
        }
        val actual = infer(value, ty, st)
        ensure(expected == actual, s"expected $value to have type $expected but was $actual", id, value)
      }

    case Spec(xs, pre, post) =>
      xs.foreach(infer(_, ty, st)) // just check if defined
      ensure(infer(pre, ty, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", pre)

    case If(test, left, right) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of if-then-else statement must be boolean", test)
      checkProg(left, ty, st, loop)
      checkProg(right, ty, st, loop)

    case While(test, body, after, term, pre, post) =>
      ensure(infer(test, ty, st) == Sort.bool, "test of while statement must be boolean", test)
      checkProg(body, ty, st, true)
      checkProg(after, ty, st, loop)
      ensure(infer(term, ty, st) == Sort.int, "termination expression must be integral", term)
      ensure(infer(pre, ty, st) == Sort.bool, "pre-condition must be boolean", pre)
      ensure(infer(post, ty, st) == Sort.bool, "post-condition must be boolean", pre)

    case Call(name, in, out) if st.procs contains name =>
      val (xs, ys) = st.procs(name)
      val args = in map (infer(_, ty, st))
      val ass = out map (infer(_, ty, st))
      ensure(xs == args, "procedure call arguments do not math function signature", args, xs)
      ensure(ys == ass, "procedure return values do not math function signature", args, ys)

    case Call(name, _, _) =>
      error("call to undefined procedure", name)

    case Choose(xs, phi) =>
      for (x <- xs) {
        ensure(ty contains x, "unknown variable $x")
      }
      val typ = infer(phi, ty, st)
      ensure(typ == Sort.bool, "condition of choose must be boolean but was $typ")
  }

  def checkObj(sort: Sort, obj: Obj, st: State): Unit = {
    val Obj(xs, init, ops) = obj
    Check.checkProc(Id.init, init, st, xs)
    ops.foreach(proc => Check.checkProc(proc._1, proc._2, st, xs))
  }

  /**
   * Does some basic checks on a procedure w.r.t. well-definedness. This exludes anything that requires knowledge about
   * the state.
   *
   * @param id name of the procedure for error messages
   * @param xs (optional) state of the surrounding object if this procedure is defined on an object
   */
  def checkProc(id: Id, proc: Proc, st: State, xs: List[Formal] = Nil): Unit = {
    val Proc(in, out, pre, post, body) = proc
    val inVars: List[Id] = in
    val duplicateInputDeclarations = inVars.groupBy(identity).filter(_._2.size > 1)
    ensure(duplicateInputDeclarations.isEmpty, s"The method $id declares duplicate input parameters ${duplicateInputDeclarations.keys.mkString(", ")}")

    // the outputs may have the same variable name in multiple places if the type is equal.
    // outputs may overlap with inputs but, again, the type must be equal
    val nonUniqueAgruments = (in ++ out).groupBy(_.id).filter(_._2.map(_.typ).distinct.size > 1)
    ensure(nonUniqueAgruments.isEmpty, "The method $id declares non-unique type for argument ${nonUniqueAgruments.keys.mkString(", ")}")

    ensure(infer(pre, xs ++ in, st) == Sort.bool, "precondition must be boolean")
    ensure(infer(post, xs ++ in ++ out, st) == Sort.bool, "postcondition must be boolean")

    for (body <- body) {
      checkBody(id, body, in, out, st, xs)
    }
  }

  def checkBody(id: Id, body: Body, in: List[Formal], out: List[Formal], st: State, xs: List[Formal] = Nil): Unit = {
    val inVars: List[Id] = in

    // procedure must at most modify its own variables
    val modifiableVariables: List[Id] = (in ++ out ++ xs ++ body.locals).distinct
    val modifiedVariables = body.mod
    val illegallyModifiedVariables = modifiedVariables.filter(!modifiableVariables.contains(_))
    ensure(illegallyModifiedVariables.isEmpty, s"The method $id modifies undeclared variables ${illegallyModifiedVariables.mkString(", ")}")

    // procedure must at least modify output variables that are not input variables
    val outputsThatMustBeSet = out.map(_.id).filter(!inVars.contains(_))
    val unsetOutputs = outputsThatMustBeSet.filter(!modifiedVariables.contains(_))
    ensure(unsetOutputs.isEmpty, s"The method $id does not modify its output parameters ${unsetOutputs.mkString(", ")}")

    checkProg(body.prog, in ++ out ++ body.locals ++ xs, st, loop = false)
  }
}
