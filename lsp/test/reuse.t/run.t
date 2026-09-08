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
  id  Int? — users
  name  Text? — users
  users  source — 2 columns
  any_value  function
  avg  function
  coalesce  function
  concat  function
  concat_ws  function
  count  function
  current_date  function
  current_time  function
  current_timestamp  function
