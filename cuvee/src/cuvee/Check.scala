package cuvee

import cuvee.Type.array

object Check {
  def infer(expr: Expr, ty: Map[Id, Type], st: State): Type = try {
    doInfer(expr, ty, st)
  } catch {
    case e: Error =>
      throw Error(e.info.toList ++ List(expr.toString))
    case other: Throwable =>
      throw other
  }

  private def doInfer(expr: Expr, ty: Map[Id, Type], st: State): Type = expr match {
    case _: Num =>
      Sort.int

    case id: Id if ty contains id =>
      ty(id)

    case id: Id if st.funs contains id =>
      val (ts, tr) = st funs id
      ensure(ts.isEmpty, s"not a constant: $id")
      tr

    case _: Id =>
      error(s"unknown identifier $expr")

    case Note(expr, _) =>
      infer(expr, ty, st)

    case UMinus(arg) =>
      val typ = infer(arg, ty, st)
      ensure(typ == Sort.int, s"argument for unary minus must be integer but was $typ")
      Sort.int

    case App(id, args) if st.funs contains id =>
      val ts1 = args map (infer(_, ty, st))
      val (ts2, tr) = st funs id
      ensure(ts1 == ts2, s"arguments for $id do not match function signature." +
        s" Expected ${sig(ts2)} but was ${sig(ts1)}")
      tr

    case App(id, _) =>
      error(s"unknown function $id")

    case Bind(_, formals, body) =>
      val ts: List[(Id, Type)] = formals
      val tr = infer(body, ty ++ ts, st)
      ensure(tr == Sort.bool, s"body of bind must be boolean but was $tr")
      tr

    case Old(expr) =>
      infer(expr, ty, st)

    case Eq(left, right) =>
      val lt = infer(left, ty, st)
      val rt = infer(right, ty, st)
      ensure(lt == rt, s"types of sides of equals must match but were $lt and $rt")
      Sort.bool

    case Distinct(exprs) =>
      val types = exprs.map(infer(_, ty, st)).distinct
      ensure(types.length == 1, s"types of arguments of distinct must match but were ${types mkString " != "}")
      Sort.bool

    case Ite(test, left, right) =>
      val tt = infer(test, ty, st)
      ensure(tt == Sort.bool, s"test of if-then-else expression must be boolean but was $tt")
      val lt = infer(left, ty, st)
      val rt = infer(right, ty, st)
      ensure(lt == rt, s"types of then- and else-clause must match but were $lt != $rt")
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
          ensure(st.datatypes contains sort, s"trying to match non-datatype $sort")

          val types = for (Case(pat, expr) <- cases) yield {
            inferCase(sort, pat, expr, ty, st)
          }
          ensure(types.distinct.size == 1, s"match cases must return equal type but found ${types.distinct.mkString(", ")}")

          types.head
        case typ => error(s"can only match sorts, found $typ")
      }

    case Select(arr, index) => infer(arr, ty, st) match {
      case array(dom, ran) =>
        val it = infer(index, ty, st)
        ensure(it == dom, s"expected index to be of type $dom byt was $it")

        ran
      case other =>
        error(s"expected array type but got $other")
    }

    case Store(arr, index, value) => infer(arr, ty, st) match {
      case typ @ array(dom, ran) =>
        val it = infer(index, ty, st)
        ensure(it == dom, s"expected index to be of type $dom byt was $it")

        val vt = infer(value, ty, st)
        ensure(vt == ran, s"expected value to be stored in array of type $ran but was $vt")

        typ
      case other =>
        error(s"expected array type but got $other")
    }
  }

  private def inferCase(typ: Type, pat: Pat, expr: Expr, ty: Map[Id, Type], st: State): Type = pat match {
    case id: Id => infer(expr, ty + (id -> typ), st)
    case UnApp(fun, args) =>
      ensure(st.funs contains fun, s"unknown constructor $fun", pat)
      val (ct, dt) = st.funs(fun)
      ensure(typ == dt, s"trying to match $typ with $dt-constructor $fun", pat)
      ensure(args.length == ct.length, s"wrong number of arguments for $fun", pat)
      infer(expr, ty ++ args.zip(ct), st)
  }

  private def inferWpLike(prog: Prog, post: Expr, ty: Map[Id, Type], st: State): Type = {
    val postt = infer(post, ty, st)
    ensure(postt == Sort.bool, s"post-condition must be boolean but was $postt", post)
    checkProg(prog, ty, st, loop = false)
    Sort.bool
  }

  /**
   * @param loop true if this program is in a loop. This allows the break statement.
   */
  def checkProg(prog: Prog, ty: Map[Id, Type], st: State, loop: Boolean): Unit = try {
    doCheckProg(prog, ty, st, loop)
  } catch {
    case e: Error =>
      throw Error(e.info.toList ++ List(prog.toString))
    case other: Throwable =>
      throw other
  }

  /**
   * @param loop true if this program is in a loop. This allows the break statement.
   */
  private def doCheckProg(prog: Prog, ty: Map[Id, Type], st: State, loop: Boolean): Unit = prog match {
    case Block(progs, _) =>
      for (prog <- progs)
        checkProg(prog, ty, st, loop)

    case Break =>
      ensure(loop, "break must only occur in while")

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
        ensure(expected == actual, s"value of type $actual cannot be assigned to variable $id of type $expected")
      }

    case Spec(xs, pre, post) =>
      xs.foreach(infer(_, ty, st)) // just check if defined

      val pret = infer(pre, ty, st)
      ensure(pret == Sort.bool, s"pre-condition must be boolean but was $pret", pre)

      val postt = infer(post, ty, st)
      ensure(postt == Sort.bool, s"post-condition must be boolean but was $postt", post)

    case If(test, left, right) =>
      val tt = infer(test, ty, st)
      ensure(tt == Sort.bool, s"test of if-then-else statement must be boolean but was $tt", test)
      checkProg(left, ty, st, loop)
      checkProg(right, ty, st, loop)

    case While(test, body, after, term, pre, post) =>
      val tt = infer(test, ty, st)
      ensure(tt == Sort.bool, s"test of while statement must be boolean but was $tt", test)

      checkProg(body, ty, st, loop = true)
      checkProg(after, ty, st, loop)

      val termt = infer(term, ty, st)
      ensure(termt == Sort.int, s"termination expression must be integral but was $termt", term)

      val pret = infer(pre, ty, st)
      ensure(pret == Sort.bool, s"pre-condition must be boolean but was $pret", pre)

      val postt = infer(post, ty, st)
      ensure(postt == Sort.bool, s"post-condition must be boolean but was $postt", post)

    case Call(name, in, out) if st.procs contains name =>
      val (xs, ys) = st.procs(name)
      val args = in map (infer(_, ty, st))
      val ass = out map (infer(_, ty, st))
      ensure(xs == args, s"procedure call arguments do not math function signature. Expected ${sig(xs)} but was ${sig(args)}")
      ensure(ys == ass, s"procedure return values do not math function signature. Expected ${sig(ys)} but was ${sig(ass)}")

    case Call(name, _, _) =>
      error(s"call to undefined procedure $name")

    case Choose(xs, phi) =>
      for (x <- xs) {
        ensure(ty contains x, s"unknown variable $x")
      }
      val typ = infer(phi, ty, st)
      ensure(typ == Sort.bool, s"condition of choose must be boolean but was $typ", phi)
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

  private def sig(ts2: List[Type]) = {
    ts2.mkString("(", ", ", ")")
  }
}
