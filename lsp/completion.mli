type item = private {
  label : string;
  detail : string;
  kind : Linol_lsp.Types.CompletionItemKind.t;
  rank : int;
}

val make : Document.t -> int -> Sql.Pos.t * item list
