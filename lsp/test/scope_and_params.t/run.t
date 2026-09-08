Parameters of a reused query belong to the query that declares them: a statement
referencing it twice takes the parameter once and gets no highlight for it, because
the parameter has no place in this statement's text.

A parameter without a name occupies text just like a named one, so both are
highlighted, and both can be hovered.

  $ ../ask.exe q.sql tokens hover:'c2 AS &b' hover:'b = ^' hover:'@name^'
  ### tokens
  4:26-4:28 parameter
  15:26-15:27 parameter
  15:36-15:42 parameter
  ### hover:c2 AS &b
  7:0-8:16
  `twice` — SELECT — any number of rows
  
  **Parameters**
  
  ```sql
  @x  Int
  ```
  
  **Result**
  
  ```sql
  a  Int
  ```
  ### hover:b = ^
  15:26-15:27
  ```sql
  ?  Int
  ```
  ### hover:@name^
  15:36-15:42
  ```sql
  @named  Int
  ```

A bare column resolves against the sources in FROM, not against every declared CTE:
`unused` is never joined, so it offers nothing to `a`. Its name is still known, so
hovering the declaration works.

  $ ../ask.exe q.sql hover:'a AS x' def:'a AS x' hover:'unused AS'
  ### hover:a AS x
  12:7-12:8
  ```sql
  used.a  Int?
  ```
  
  Available in this statement
  ### def:a AS x
  q.sql 11:21-11:22
  ### hover:unused AS
  11:32-11:38
  **CTE** `unused`
  
  ```sql
  a  Int?
  ```
  
  Available in this statement
