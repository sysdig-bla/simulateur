exception Error

type token = 
  | XOR
  | WHERE
  | THEN
  | STRING of (string)
  | STAR
  | SLASH
  | SEMICOL
  | RPAREN
  | ROM
  | REG
  | RBRACKET
  | RAM
  | PROBING
  | POWER
  | PLUS
  | OR
  | NOT
  | NAND
  | NAME of (string)
  | MINUS
  | LPAREN
  | LESS
  | LEQ
  | LBRACKET
  | INT of (int)
  | INLINED
  | IF
  | GREATER
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOTDOT
  | DOT
  | CONST
  | COMMA
  | COLON
  | BOOL_INT of (string)
  | BOOL of (bool)
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState173
  | MenhirState168
  | MenhirState164
  | MenhirState159
  | MenhirState155
  | MenhirState154
  | MenhirState153
  | MenhirState145
  | MenhirState137
  | MenhirState135
  | MenhirState133
  | MenhirState131
  | MenhirState129
  | MenhirState124
  | MenhirState122
  | MenhirState118
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState72
  | MenhirState68
  | MenhirState67
  | MenhirState64
  | MenhirState60
  | MenhirState59
  | MenhirState54
  | MenhirState53
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState31
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState4
  | MenhirState1
  | MenhirState0

  

open Ident
open Static
open Ast
open Location
open Misc

let fresh_param () =
  mk_static_exp (SVar ("_n"^(Misc.gen_symbol ())))

let _eRR =
  Error

let rec _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159
            | LPAREN ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, se, _startpos_se_, _endpos_se_), _), _, thenb), _, elseb) = _menhir_stack in
                let _v : (Ast.block) =                                                               ( BIf(se, thenb, elseb) ) in
                _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PROBING ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Ast.ident list) =               ( [] ) in
            _menhir_goto_probe_decls _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_equ_tail : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.equation list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let tl = _v in
        let ((_menhir_stack, _menhir_s, _endpos__1_), _, eq) = _menhir_stack in
        let _v : (Ast.equation list) =                                ( eq::tl ) in
        _menhir_goto_equ_tail _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let tl = _v in
        let (_menhir_stack, _menhir_s, eq) = _menhir_stack in
        let _v : (Ast.equation list) =                          ( eq::tl ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let eqs = _v in
        let _v : (Ast.block) =              ( BEqs (eqs, []) ) in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, xs00) = _menhir_stack in
        let _endpos = _endpos__3_ in
        let _v : (Ast.exp list) = let e =
          let xs0 = xs00 in
          let l =
            let xs = xs0 in
                ( xs )
          in
                                                                                 (l)
        in
                                                (e) in
        (match _menhir_s with
        | MenhirState114 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let a = _v in
            let _endpos_a_ = _endpos in
            let (((((_menhir_stack, _menhir_s, ro, _startpos_ro_), _, addr_size, _startpos_addr_size_, _endpos_addr_size_), _), _, word_size, _startpos_word_size_, _endpos_word_size_), _) = _menhir_stack in
            let _startpos = _startpos_ro_ in
            let _endpos = _endpos_a_ in
            let _v : (Ast.edesc) = let input_file =
                                ( None )
            in
                ( Emem(ro, addr_size, word_size, input_file, a) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | MenhirState118 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let a = _v in
            let _endpos_a_ = _endpos in
            let ((((((_menhir_stack, _menhir_s, ro, _startpos_ro_), _, addr_size, _startpos_addr_size_, _endpos_addr_size_), _), _, word_size, _startpos_word_size_, _endpos_word_size_), _), v0) = _menhir_stack in
            let _startpos = _startpos_ro_ in
            let _endpos = _endpos_a_ in
            let _v : (Ast.edesc) = let input_file =
              let v = v0 in
                                ( Some(v) )
            in
                ( Emem(ro, addr_size, word_size, input_file, a) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | MenhirState90 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let a = _v in
            let _endpos_a_ = _endpos in
            let ((_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_), p) = _menhir_stack in
            let _startpos = _startpos_n_ in
            let _endpos = _endpos_a_ in
            let _v : (Ast.edesc) =                                 ( Ecall (n, p, a) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.equation list) =               ( [] ) in
    _menhir_goto_equ_tail _menhir_env _menhir_stack _menhir_s _v

and _menhir_run154 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.equation) -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | ELSE | END | PROBING ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _endpos__1_) = _menhir_stack in
        let _v : (Ast.equation list) =             ( [] ) in
        _menhir_goto_equ_tail _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.exp list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (Ast.exp list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run122 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run129 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_run135 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_run131 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run137 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_run133 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_goto_option_SEMICOL_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__5_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, n, _startpos_n_, _endpos_n_), _, se, _startpos_se_, _endpos_se_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (Ast.const_dec) =       ( mk_const_dec ~loc:(Loc(_startpos,_endpos)) n se ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | EOF | INLINED | NAME _ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__14_ = _endpos in
        let (((((((((_menhir_stack, _menhir_s, inlined, _startpos_inlined_), _, n), p), _, _startpos__4_), _, args), _endpos__6_), _, out), _, b), probes) = _menhir_stack in
        let _startpos = _startpos_inlined_ in
        let _endpos = _endpos__14_ in
        let _v : (Ast.node_dec) =       ( mk_node n (Loc (_startpos,_endpos)) inlined args out p b probes ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
        | EOF ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NAME _ ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | _ ->
        _menhir_fail ()

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState91 in
        let _v : (Ast.exp list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_goto_separated_nonempty_list_COMMA_static_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Static.static_exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _), _, xs) = _menhir_stack in
        let _v : (Static.static_exp list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_static_exp_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GREATER ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, l0) = _menhir_stack in
            let _v : (Static.static_exp list) = let pl =
              let l = l0 in
                                                                                     (l)
            in
                                                         ( pl ) in
            _menhir_goto_call_params _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_goto_rom_or_ram : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.mem_kind) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LESS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
        | NAME _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_call_params : _menhir_env -> 'ttv_tail -> (Static.static_exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.value) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let c = _v in
    let _startpos_c_ = _startpos in
    let _endpos_c_ = _endpos in
    let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : (Ast.edesc) =             ( Econst c ) in
    _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.edesc) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_e_ in
    let _v : (Ast.exp) =             ( mk_exp ~loc:(Loc (_startpos,_endpos)) e ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                       ( Ecall ("xor", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                         ( Ecall("xor", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DOT | ELSE | END | OR | PLUS | PROBING | RPAREN | SEMICOL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                        ( Ecall ("or", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                        ( Ecall ("nand", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                       ( Ecall ("and", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DOT | ELSE | END | OR | PLUS | PROBING | RPAREN | SEMICOL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =                      ( Ecall ("or", [], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DOT | ELSE | END | PROBING | RPAREN | SEMICOL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_, _endpos_e1_), _, e2, _startpos_e2_, _endpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (Ast.edesc) =     ( Ecall("concat", [fresh_param(); fresh_param(); fresh_param ()], [e1; e2]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BOOL _v ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | BOOL_INT _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LBRACKET ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | LPAREN ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | NOT ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | RAM ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | REG ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | ROM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
        | DOT ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Ast.exp list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_a_ in
            let _v : (Ast.edesc) =                   ( Ecall ("not", [], [a])) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | PROBING | RPAREN | SEMICOL | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (Ast.edesc) =               ( Ereg e ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack)
        | NAND ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack)
        | POWER ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack)
        | ELSE | END | PROBING | SEMICOL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, p), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _v : (Ast.equation) =                        ( mk_equation p e ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState67 | MenhirState159 | MenhirState76 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOL ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_endp
                | ELSE | END | PROBING ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
            | MenhirState154 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOL ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_endp
                | ELSE | END | PROBING ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _endpos = _menhir_env._menhir_startp in
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_SEMICOL_ _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_x_ = _endpos in
    let x = () in
    let _endpos = _endpos_x_ in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_SEMICOL_ _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var_dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let xs00 = _v in
    let _v : (Ast.var_dec list) = let vl =
      let xs0 = xs00 in
      let l =
        let xs = xs0 in
            ( xs )
      in
                                                                             (l)
    in
                               ( vl ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState59 in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | NAME _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | RPAREN ->
                        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
                | NAME _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, out) = _menhir_stack in
            let _v : (Ast.var_dec list) =                            ( out ) in
            _menhir_goto_node_out _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce91 : _menhir_env -> 'ttv_tail * _menhir_state * (Static.static_exp_desc) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, se, _startpos_se_, _endpos_se_) = _menhir_stack in
    let _startpos = _startpos_se_ in
    let _endpos = _endpos_se_ in
    let _v : (Static.static_exp) =                            ( mk_static_exp ~loc:(Loc (_startpos,_endpos)) se ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | MINUS | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                        ( SBinOp(SMult, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | MINUS | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                         ( SBinOp(SPower, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | MINUS | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | SLASH | STAR | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                         ( SBinOp(SDiv, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | COMMA | CONST | DOTDOT | EOF | GREATER | INLINED | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                        ( SBinOp(SAdd, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                         ( SBinOp(SMinus, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                       ( SBinOp(SLeq, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | COMMA | CONST | DOTDOT | EOF | EQUAL | GREATER | INLINED | LEQ | NAME _ | PLUS | RBRACKET | RPAREN | SEMICOL | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, se1, _startpos_se1_, _endpos_se1_), _), _, se2, _startpos_se2_, _endpos_se2_) = _menhir_stack in
            let _startpos = _startpos_se1_ in
            let _endpos = _endpos_se2_ in
            let _v : (Static.static_exp_desc) =                                         ( SBinOp(SEqual, se1, se2) ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | SEMICOL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_endp
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | CONST | EOF | INLINED | NAME _ ->
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _startpos__1_), _, se, _startpos_se_, _endpos_se_) = _menhir_stack in
            let _v : (Ast.ty) =                                             ( TBitArray se ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let t = _v in
            let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
            let _v : (Ast.var_dec) =                                ( mk_var_dec n t ) in
            _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IF ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAREN ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState86 | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | GREATER ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Static.static_exp list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_static_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState101 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__2_), _), _, high, _startpos_high_, _endpos_high_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.edesc) =     (
      let params = [mk_static_exp (SInt 0); high; fresh_param ()] in
      Ecall("slice", params, [e1])
    ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOTDOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState103 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState105 in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__5_ = _endpos in
                let ((((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__2_), _, low, _startpos_low_, _endpos_low_), _) = _menhir_stack in
                let _startpos = _startpos_e1_ in
                let _endpos = _endpos__5_ in
                let _v : (Ast.edesc) =     ( let n = fresh_param () in
      let high = mk_static_exp (SBinOp(SMinus, n, mk_static_exp (SInt 1))) in
      Ecall("slice", [low; high; n], [e1]) ) in
                _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState103 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let (((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__2_), _, idx, _startpos_idx_, _endpos_idx_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos__4_ in
            let _v : (Ast.edesc) =     ( Ecall ("select", [idx; fresh_param()], [e1]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__6_ = _endpos in
            let (((((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__2_), _, low, _startpos_low_, _endpos_low_), _), _, high, _startpos_high_, _endpos_high_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos__6_ in
            let _v : (Ast.edesc) =     ( Ecall("slice", [low; high; fresh_param()], [e1]) ) in
            _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | GREATER ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | LPAREN ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQUAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | GREATER ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LPAREN ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | LEQ ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | POWER ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SLASH ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | STAR ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.var_dec list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GREATER ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, l0) = _menhir_stack in
            let _v : (Ast.param list) = let pl =
              let l = l0 in
                                                                                     (l)
            in
                                                    ( pl ) in
            _menhir_goto_params _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.param list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.mem_kind) =         ( MRom ) in
    _menhir_goto_rom_or_ram _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.mem_kind) =         ( MRam ) in
    _menhir_goto_rom_or_ram _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LESS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
        | NAME _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | AND | COMMA | DOT | ELSE | END | LBRACKET | NAND | OR | PLUS | POWER | PROBING | RPAREN | SEMICOL | XOR ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (Static.static_exp list) =               ( [] ) in
        _menhir_goto_call_params _menhir_env _menhir_stack _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOL _v ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | BOOL_INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | NOT ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | RAM ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | REG ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | ROM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.value) =                       ( VBitArray (Array.make 0 false) ) in
        _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _startpos_i_ = _startpos in
    let _endpos_i_ = _endpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Ast.value) =     ( match i with
      | 0 -> VBit false
      | 1 -> VBit true
      | _ -> raise Parsing.Parse_error
    ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _startpos_b_ = _startpos in
    let _endpos_b_ = _endpos in
    let _startpos = _startpos_b_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.value) =                ( VBitArray (bool_array_of_string b) ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let b = _v in
    let _startpos_b_ = _startpos in
    let _endpos_b_ = _endpos in
    let _startpos = _startpos_b_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.value) =            ( VBit b ) in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto__exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.edesc) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState78 | MenhirState80 | MenhirState82 | MenhirState91 | MenhirState145 | MenhirState137 | MenhirState135 | MenhirState129 | MenhirState133 | MenhirState131 | MenhirState122 | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.edesc) =                               ( e ) in
            _menhir_goto__simple_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | AND | DOT | NAND | OR | PLUS | POWER | XOR ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_probe_decls : _menhir_env -> 'ttv_tail -> (Ast.ident list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | WHERE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMICOL ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_endp
            | EOF | INLINED | NAME _ ->
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_node_out : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var_dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHERE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
        | NAME _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_arg_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var_dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState60 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.var_dec list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_arg__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.var_dec list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto__static_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Static.static_exp_desc) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState74 | MenhirState110 | MenhirState112 | MenhirState99 | MenhirState105 | MenhirState100 | MenhirState84 | MenhirState86 | MenhirState53 | MenhirState4 | MenhirState16 | MenhirState22 | MenhirState20 | MenhirState18 | MenhirState14 | MenhirState9 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, se, _startpos_se_, _endpos_se_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Static.static_exp_desc) =                                  ( se ) in
            _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | EQUAL | LEQ | MINUS | PLUS | POWER | SLASH | STAR ->
            _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_node_dec_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.node_dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.node_dec list) =     ( x :: xs ) in
        _menhir_goto_list_node_dec_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ns = _v in
        let _v : (Ast.node_dec list) =                              ( ns ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, c), _, n) = _menhir_stack in
            let _v : (Ast.program) =       ( mk_program c n ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_inlined_status : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.inlined_status) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_params : _menhir_env -> 'ttv_tail -> (Ast.param list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | NAME _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | RPAREN ->
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let n = _v in
    let _startpos_n_ = _startpos in
    let _endpos_n_ = _endpos in
    let _v : (Ast.param) =          ( mk_param n ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | NAME _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | GREATER ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.param list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pat) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BOOL _v ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | BOOL_INT _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | NAME _v ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | NOT ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | RAM ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | REG ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | ROM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto__simple_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.edesc) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : (Ast.exp) =                           ( mk_exp ~loc:(Loc (_startpos,_endpos)) e ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DOTDOT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState99 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | INT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LPAREN ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
                | NAME _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | COMMA | DOT | ELSE | END | NAND | OR | PLUS | POWER | PROBING | RPAREN | SEMICOL | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : (Ast.edesc) =                    ( e ) in
        _menhir_goto__exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ident list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, l0) = _menhir_stack in
            let _v : (Ast.pat) = let p =
              let l = l0 in
                                                                                     (l)
            in
                                                     ( Etuplepat p ) in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.ident list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, l) = _menhir_stack in
        let _v : (Ast.ident list) =                                                     ( l ) in
        _menhir_goto_probe_decls _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var_dec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 | MenhirState64 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | NAME _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.var_dec list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_arg_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, a) = _menhir_stack in
        let _v : (Ast.var_dec list) =           ( [a] ) in
        _menhir_goto_node_out _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let n = _v in
    let _startpos_n_ = _startpos in
    let _endpos_n_ = _endpos in
    let _startpos = _startpos_n_ in
    let _endpos = _endpos_n_ in
    let _v : (Static.static_exp_desc) =            ( SVar n ) in
    _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | NAME _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _startpos_i_ = _startpos in
    let _endpos_i_ = _endpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (Static.static_exp_desc) =           ( SInt i ) in
    _menhir_goto__static_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _startpos = _menhir_env._menhir_startp in
    let _v : (Ast.inlined_status) =               ( NotInlined ) in
    _menhir_goto_inlined_status _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.node_dec list) =     ( [] ) in
    _menhir_goto_list_node_dec_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.inlined_status) =             ( Inlined ) in
    _menhir_goto_inlined_status _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce62 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
    let _startpos = _startpos_n_ in
    let _endpos = _endpos_n_ in
    let _v : (Ast.name) =              ( n ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | NAME _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 | MenhirState164 | MenhirState67 | MenhirState159 | MenhirState154 | MenhirState76 | MenhirState78 | MenhirState80 | MenhirState82 | MenhirState145 | MenhirState91 | MenhirState137 | MenhirState135 | MenhirState133 | MenhirState131 | MenhirState129 | MenhirState124 | MenhirState122 | MenhirState92 | MenhirState72 | MenhirState68 | MenhirState64 | MenhirState60 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : (Ast.ident) =            ( ident_of_string n ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
        (match _menhir_s with
        | MenhirState59 | MenhirState64 | MenhirState60 | MenhirState47 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LPAREN ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
                    | NAME _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | COMMA | RPAREN | WHERE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
                let _v : (Ast.var_dec) =             ( mk_var_dec n TBit ) in
                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState164 | MenhirState72 | MenhirState68 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | NAME _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
            | END | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
                let _v : (Ast.ident list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState78 | MenhirState80 | MenhirState82 | MenhirState145 | MenhirState91 | MenhirState137 | MenhirState135 | MenhirState133 | MenhirState131 | MenhirState129 | MenhirState124 | MenhirState122 | MenhirState92 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
            let _startpos = _startpos_n_ in
            let _endpos = _endpos_n_ in
            let _v : (Ast.edesc) =                               ( Evar n ) in
            _menhir_goto__simple_exp _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | MenhirState67 | MenhirState159 | MenhirState154 | MenhirState76 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
            let _v : (Ast.pat) =                                          ( Evarpat n ) in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, n, _startpos_n_, _endpos_n_) = _menhir_stack in
        let _v : (Ast.name) =            ( reset_symbol_table (); n ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LESS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | NAME _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (Ast.param list) =               ( [] ) in
            _menhir_goto_params _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_const_dec_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.const_dec list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let c = _v in
        let _v : (Ast.const_dec list) =                               (c) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INLINED ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
        | EOF ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NAME _ ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.const_dec list) =     ( x :: xs ) in
        _menhir_goto_list_const_dec_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.const_dec list) =     ( [] ) in
    _menhir_goto_list_const_dec_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | NAME _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | EOF | INLINED | NAME _ ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)




