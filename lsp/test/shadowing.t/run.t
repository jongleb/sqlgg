A source of the statement wins over a table of the same name in the schema.
Each of the three names below also exists as a table, and the answer must
come from the statement, not from the schema.

The alias `u` stands for `users`, even though there is a table called `u`:

  $ ../ask.exe q.sql hover:'SELECT u^' def:'SELECT u^' hover:'SELECT u.nam^' def:'SELECT u.nam^'
  ### hover:SELECT u^
  8:7-8:8
  **alias** `u` of `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `q.sql`
  ### def:SELECT u^
  q.sql 8:25-8:26
  ### hover:SELECT u.nam^
  8:9-8:13
  ```sql
  users.name  Text?
  ```
  
  Declared in `q.sql`
  ### def:SELECT u.nam^
  q.sql 1:28-1:32

The alias of a subquery is a subquery, not the table of the same name:

  $ ../ask.exe q.sql hover:'SELECT shadow.i^' def:'SELECT shadow.i^'
  ### hover:SELECT shadow.i^
  11:14-11:16
  ```sql
  shadow.id  Int?
  ```
  
  Available in this statement
  ### def:SELECT shadow.i^
  q.sql 11:45-11:51

A CTE wins over a table of the same name too:

  $ ../ask.exe q.sql hover:'shadow.id FROM shadow^' def:'shadow.id FROM shadow^'
  ### hover:shadow.id FROM shadow^
  15:22-15:28
  **CTE** `shadow`
  
  ```sql
  id  Int?
  ```
  
  Available in this statement
  ### def:shadow.id FROM shadow^
  q.sql 14:5-14:11
