Reusable queries chained as CTEs: each `&name` resolves to its own statement,
and a reusable query over a missing table stays a diagnostic instead of killing the request:

  $ ../ask.exe q.sql diags hover:'&active' hover:'&written' def:'&written' hover:'u JOIN' complete:'u.'
  ### diags
  11:0-11:39 no such table companies
  ### hover:&active
  14:10-14:17
  `active` — SELECT — any number of rows
  
  **Parameters**
  
  ```sql
  @id  Int
  ```
  
  **Result**
  
  ```sql
  id    Int
  name  Text?
  ```
  ### hover:&written
  14:24-14:32
  `written` — SELECT — any number of rows
  
  **Parameters**
  
  ```sql
  @title  Text
  ```
  
  **Result**
  
  ```sql
  user_id  Int?
  title    Text
  ```
  ### def:&written
  q.sql 8:0-8:53
  ### hover:u JOIN
  15:28-15:29
  **CTE** `u`
  
  ```sql
  id    Int
  name  Text?
  ```
  
  Available in this statement
  ### complete:u.
  replace 15:7-15:8
  id  Int — u
  name  Text? — u
  title  Text — p
  user_id  Int? — p
  p  source — 2 columns
  u  source — 2 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function

Parameters of a reused query belong to its declaration: using it twice adds the
parameter once and creates no token at either reference. Unnamed and named local
parameters both remain hoverable.

  $ ../ask.exe params.sql tokens hover:'WITH c1' hover:'c2 AS &b' hover:'b = ^' hover:'@name^'
  ### tokens
  4:26-4:28 parameter
  15:26-15:27 parameter
  15:36-15:42 parameter
  ### hover:WITH c1
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
  ### hover:c2 AS &b
  7:17-7:19
  **CTE** `c2`
  
  ```sql
  a  Int
  ```
  
  Available in this statement
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

A bare column resolves against used FROM sources, while an unused CTE name
remains hoverable:

  $ ../ask.exe params.sql hover:'a AS x' def:'a AS x' hover:'unused AS'
  ### hover:a AS x
  12:7-12:8
  ```sql
  used.a  Int?
  ```
  
  Available in this statement
  ### def:a AS x
  params.sql 11:21-11:22
  ### hover:unused AS
  11:32-11:38
  **CTE** `unused`
  
  ```sql
  a  Int?
  ```
  
  Available in this statement
