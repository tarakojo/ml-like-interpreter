
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TRUE
    | THEN
    | SUB
    | MUL
    | LT
    | INT of (
# 5 "lib/parser.mly"
       (int)
# 20 "lib/parser.ml"
  )
    | IF
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DIV
    | ADD
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
open Syntax

# 38 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parseExpr) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parseExpr. *)

  | MenhirState03 : (('s, _menhir_box_parseExpr) _menhir_cell1_IF, _menhir_box_parseExpr) _menhir_state
    (** State 03.
        Stack shape : IF.
        Start symbol: parseExpr. *)

  | MenhirState06 : ((('s, _menhir_box_parseExpr) _menhir_cell1_IF, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 06.
        Stack shape : IF expression.
        Start symbol: parseExpr. *)

  | MenhirState08 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 08.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState10 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 10.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState12 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 12.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState14 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 14.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState16 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 16.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState18 : (('s, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 18.
        Stack shape : expression.
        Start symbol: parseExpr. *)

  | MenhirState20 : (((('s, _menhir_box_parseExpr) _menhir_cell1_IF, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_state
    (** State 20.
        Stack shape : IF expression expression.
        Start symbol: parseExpr. *)


and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and _menhir_box_parseExpr = 
  | MenhirBox_parseExpr of (Syntax.expr) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 26 "lib/parser.mly"
        ( EValue(VInt(_1)) )
# 106 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_02 =
  fun () ->
    (
# 27 "lib/parser.mly"
         ( EValue(VBool(true)) )
# 114 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_03 =
  fun () ->
    (
# 28 "lib/parser.mly"
          ( EValue(VBool(false)) )
# 122 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_04 =
  fun _1 _3 ->
    (
# 29 "lib/parser.mly"
                              ( EBin (OpAdd, _1, _3) )
# 130 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_05 =
  fun _1 _3 ->
    (
# 30 "lib/parser.mly"
                              ( EBin (OpSub, _1, _3) )
# 138 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_06 =
  fun _1 _3 ->
    (
# 31 "lib/parser.mly"
                              ( EBin (OpMul, _1, _3) )
# 146 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_07 =
  fun _1 _3 ->
    (
# 32 "lib/parser.mly"
                              ( EBin (OpDiv, _1, _3) )
# 154 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_08 =
  fun _1 _3 ->
    (
# 33 "lib/parser.mly"
                             ( EEq (_1, _3) )
# 162 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 34 "lib/parser.mly"
                             ( ELt (_1, _3) )
# 170 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_10 =
  fun _2 _4 _6 ->
    (
# 35 "lib/parser.mly"
                                                  ( EIf (_2, _4, _6) )
# 178 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_action_11 =
  fun _1 ->
    (
# 23 "lib/parser.mly"
                   (_1)
# 186 "lib/parser.ml"
     : (Syntax.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | DIV ->
        "DIV"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | IF ->
        "IF"
    | INT _ ->
        "INT"
    | LT ->
        "LT"
    | MUL ->
        "MUL"
    | SUB ->
        "SUB"
    | THEN ->
        "THEN"
    | TRUE ->
        "TRUE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_23 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_11 _1 in
          MenhirBox_parseExpr _v
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ELSE | EOF | EQ | LT | SUB | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_05 _1 _3 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_06 _1 _3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState20 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_21 : type  ttv_stack. ((((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_IF, _menhir_box_parseExpr) _menhir_cell1_expression, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF | THEN ->
          let MenhirCell1_expression (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell1_expression (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _6 = _v in
          let _v = _menhir_action_10 _2 _4 _6 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState14
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. ((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_09 _1 _3 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_07 _1 _3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parseExpr) _menhir_state -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_02 () in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
          | INT _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_1 in
              let _v = _menhir_action_01 _1 in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
          | IF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_03 () in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
          | _ ->
              _eRR ())
      | SUB ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. (((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_IF, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQ ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_02 () in
              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState20 _tok
          | INT _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_1 in
              let _v = _menhir_action_01 _1 in
              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState20 _tok
          | IF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_03 () in
              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState20 _tok
          | _ ->
              _eRR ())
      | DIV ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_08 _1 _3 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. ((ttv_stack, _menhir_box_parseExpr) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_parseExpr) _menhir_state -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | MUL ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD | ELSE | EOF | EQ | LT | SUB | THEN ->
          let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_04 _1 _3 in
          _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parseExpr =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_01 _1 in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
end

let parseExpr =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parseExpr v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
