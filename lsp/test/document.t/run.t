Completion uses the schema before its statement, not the final document schema:

  $ ../ask.exe state.sql diags complete-fields:'na FROM t WHERE' complete-fields:'na FROM t;'
  ### diags
  3:7-3:9 missing attribute : na
  9:7-9:9 missing attribute : na
  ### complete-fields:na FROM t WHERE
  replace 3:7-3:9
  id  Int? — t
  name  Text? — t
  ### complete-fields:na FROM t;
  replace 9:7-9:9
  id  Int? — t

The parser recovers from different syntax errors and still completes from the
statement's sources:

  $ ../ask.exe recovery.sql complete-fields:na1 complete-fields:na2 complete-fields:na3 complete-fields:'ti;'
  ### complete-fields:na1
  replace 14:28-14:31
  email  Text? — users
  id  Int — users
  name  Text — users
  ### complete-fields:na2
  replace 17:36-17:39
  email  Text? — users
  id  Int — users
  name  Text — users
  ### complete-fields:na3
  replace 20:25-20:28
  email  Text? — users
  id  Int — users
  name  Text — users
  ### complete-fields:ti;
  replace 23:63-23:65
  author_id  Int — p
  id  Int — p
  title  Text? — p
