val parsed : Sqlgg.Analysis.parsed -> V1.statement
val checked : Sqlgg.Syntax.result -> V1.statement
val invalid :
  sql:string -> Sqlgg.Analysis.diagnostic list -> V1.statement
