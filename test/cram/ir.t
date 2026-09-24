Schema-less statements are emitted as parsed IR:

  $ sqlgg -gen ir - <<'SQL'
  > SELECT 1;
  > SQL
  Unknown output language: ir
  [1]
