
module T_SQL_parser =
  struct
    type token = Sql_parser.token
    type result = Sql.stmt
    let rule = Sql_lexer.parse_rule
    let input = Sql_parser.input
  end

module T = Parser_utils.Make (T_SQL_parser)
  
let parse_stmt feeder_queue = T.parse_buf_exn (Lexing.from_function (Stmt_elements.queue_byte_feeder ~feeder_queue))
