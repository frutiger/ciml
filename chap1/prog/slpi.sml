structure Prog1 = struct
  fun maxargs(Sup.PrintStm(exps)) = foldl (op +) 0 (List.map maxargsE exps)
    | maxargs(Sup.CompoundStm(s1, s2)) = Int.max(maxargs(s1), maxargs(s2))
    | maxargs(Sup.AssignStm(_, e)) = maxargsE(e)
  and maxargsE(Sup.OpExp(e1, _, e2)) = Int.max(maxargsE(e1), maxargsE(e2))
    | maxargsE(Sup.EseqExp(s, e)) = Int.max(maxargs(s), maxargsE(e))
    | maxargsE(_) = 1
end

structure Prog2: sig
  val interp: Sup.stm -> unit
end = struct
  exception UnboundVar of Sup.id

  fun lookup(id, nil) = raise UnboundVar(id)
    | lookup(id, (i, v)::ts) = if i = id then v else lookup(id, ts)

  fun interpExp(Sup.IdExp(id), t) = (lookup(id, t), t)
    | interpExp(Sup.NumExp(x), t) = (x, t)
    | interpExp(Sup.OpExp(e1, oper, e2), t) =
        let
          val lhs = #1(interpExp(e1, t))
          val rhs = #1(interpExp(e2, t))
          val result = case oper
            of Sup.Plus  => lhs + rhs
             | Sup.Minus => lhs - rhs
             | Sup.Times => lhs * rhs
             | Sup.Div   => lhs div rhs
        in
          (result, t)
        end
    | interpExp(Sup.EseqExp(s, e), t) = interpExp(e, interpStm(s, t))
  and interpStm(Sup.CompoundStm(s1, s2), t) = interpStm(s2, (interpStm(s1, t)))
    | interpStm(Sup.AssignStm(id, e), t) = (id, #1(interpExp(e, t)))::t
    | interpStm(Sup.PrintStm(nil), t) = (print("\n"); t)
    | interpStm(Sup.PrintStm(e::es), t) =
        let
          val v = #1(interpExp(e, t))
        in
          (print(Int.toString(v)); print(" "); interpStm(Sup.PrintStm(es), t))
        end

  fun interp(s) = (interpStm(s, []); ())
end

