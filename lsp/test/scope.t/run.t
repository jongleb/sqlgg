Aliases, CTEs and subqueries in a statement that type-checks; a CTE column leads to its expression:

  $ ../ask.exe q.sql diags hover:'u.name' hover:'r.title' hover:'sub.n' hover:'recent AS r' def:'u.name' def:'r.title' def:'title, sub' def:'recent AS r' def:'sub.n' complete:'sub.n'
  ### diags
  11:7-11:13 missing attribute : nmae
  13:23-13:27 missing attribute : nmae
  18:4-18:11 missing attribute : nmae
  23:7-23:12 missing attribute : title
  24:7-24:9 duplicate attribute : id
  ### hover:u.name
  5:7-5:8
  **alias** `u` of `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `q.sql`
  ### hover:r.title
  5:15-5:16
  **alias** `r` of `recent`
  
  ```sql
  id     Int
  title  Text?
  ```
  
  Available in this statement
  ### hover:sub.n
  5:24-5:27
  **subquery** `sub`
  
  ```sql
  author  Int?
  n       Int
  ```
  
  Available in this statement
  ### hover:recent AS r
  7:5-7:11
  **CTE** `recent`
  
  ```sql
  id     Int
  title  Text?
  ```
  
  Available in this statement
  ### def:u.name
  q.sql 6:14-6:15
  ### def:r.title
  q.sql 7:15-7:16
  ### def:title, sub
  q.sql 4:27-4:32
  ### def:recent AS r
  q.sql 4:5-4:11
  ### def:sub.n
  q.sql 8:66-8:69
  ### complete:sub.n
  replace 5:24-5:27
  author  Int? — sub
  id  Int? — users
  n  Int — sub
  name  Text? — users
  title  Text? — recent
  r  source — 2 columns
  recent  source — 2 columns
  sub  source — 2 columns
  u  source — 2 columns
  users  source — 2 columns
  any_value  function
  avg  function

The same in statements that do not type-check, thanks to the fallback on the sources alone:

  $ ../ask.exe q.sql hover:'r.id FROM' complete:nmae hover:'uu.id' complete:'nmae ='
  ### hover:r.id FROM
  11:15-11:16
  **alias** `r` of `recent`
  
  ```sql
  id  Int?
  ```
  
  Available in this statement
  ### complete:nmae
  replace 11:9-11:13
  id  Int? — u
  name  Text? — u
  ### hover:uu.id
  13:40-13:42
  **alias** `uu` of `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `q.sql`
  ### complete:nmae =
  replace 13:23-13:27
  id  Int? — users
  name  Text? — users
  users  source — 2 columns
  uu  source — 2 columns

A multi-table UPDATE that does not type-check : every source of the table list stays in
scope, not just the first one.

  $ ../ask.exe q.sql hover:'agg.n WHERE' def:'agg.n WHERE' complete:'n WHERE'
  ### hover:agg.n WHERE
  18:14-18:17
  **subquery** `agg`
  
  ```sql
  author  Int?
  n       Int
  ```
  
  Available in this statement
  ### def:agg.n WHERE
  q.sql 17:81-17:84
  ### complete:n WHERE
  replace 18:18-18:19
  author  Int? — agg
  n  Int — agg

A subquery inside WHERE:

  $ ../ask.exe q.sql hover:'p.title' def:'p.title'
  ### hover:p.title
  15:70-15:71
  **alias** `p` of `posts`
  
  ```sql
  id      Int?
  author  Int?
  title   Text?
  ```
  
  Declared in `q.sql`
  ### def:p.title
  q.sql 15:62-15:63

Unqualified columns are limited to the current statement scope, including
aliases, missing columns, and ambiguity:

  $ ../ask.exe q.sql hover:'id FROM users;' hover:'id FROM users AS only_users' hover:'title FROM users;' hover:'id FROM users JOIN'
  ### hover:id FROM users;
  20:7-20:9
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:id FROM users AS only_users
  22:7-22:9
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:title FROM users;
  nothing
  ### hover:id FROM users JOIN
  24:7-24:9
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  
  ---
  ```sql
  posts.id  Int?
  ```
  
  Declared in `q.sql`

Nested SELECT scopes do not leak into each other:

  $ ../ask.exe q.sql hover:'id FROM users WHERE' hover:'author FROM posts AS p'
  ### hover:id FROM users WHERE
  15:7-15:9
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:author FROM posts AS p
  15:41-15:47
  ```sql
  posts.author  Int?
  ```
  
  Declared in `q.sql`

  $ ../ask.exe q.sql complete-fields:'id FROM users WHERE' complete-fields:'author FROM posts AS p'
  ### complete-fields:id FROM users WHERE
  replace 15:7-15:9
  id  Int? — users
  name  Text? — users
  ### complete-fields:author FROM posts AS p
  replace 15:41-15:47
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts

CTE bodies, FROM subqueries, and ORDER BY use their own SELECT scope:

  $ ../ask.exe q.sql hover:'id, title FROM posts' hover:'author, count' hover:'ORDER BY id^'
  ### hover:id, title FROM posts
  4:23-4:25
  ```sql
  posts.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:author, count
  8:13-8:19
  ```sql
  posts.author  Int?
  ```
  
  Declared in `q.sql`
  ### hover:ORDER BY id^
  15:93-15:95
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`

  $ ../ask.exe q.sql complete-fields:'id, title FROM posts' complete-fields:'author, count' complete-fields:'ORDER BY id^'
  ### complete-fields:id, title FROM posts
  replace 4:23-4:25
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts
  ### complete-fields:author, count
  replace 8:13-8:19
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts
  ### complete-fields:ORDER BY id^
  replace 15:93-15:95
  id  Int? — users
  name  Text? — users

Empty and compound SELECTs keep independent scopes:

  $ ../ask.exe q.sql hover:'id FROM users UNION' hover:'id FROM posts;'
  ### hover:id FROM users UNION
  26:7-26:9
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:id FROM posts;
  26:34-26:36
  ```sql
  posts.id  Int?
  ```
  
  Declared in `q.sql`

  $ ../ask.exe q.sql complete-fields:'1 AS one' complete-fields:'id FROM users UNION' complete-fields:'id FROM posts;'
  ### complete-fields:1 AS one
  replace 25:7-25:7
  ### complete-fields:id FROM users UNION
  replace 26:7-26:9
  id  Int? — users
  name  Text? — users
  ### complete-fields:id FROM posts;
  replace 26:34-26:36
  author  Int? — posts
  id  Int? — posts
  title  Text? — posts

INSERT, UPDATE, and DELETE use their target table as scope:

  $ ../ask.exe q.sql hover:'id, name) VALUES' hover:'name = @next_name' hover:'id = @delete_id'
  ### hover:id, name) VALUES
  27:19-27:21
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`
  ### hover:name = @next_name
  28:17-28:21
  ```sql
  users.name  Text?
  ```
  
  Declared in `q.sql`
  ### hover:id = @delete_id
  29:24-29:26
  ```sql
  users.id  Int?
  ```
  
  Declared in `q.sql`

  $ ../ask.exe q.sql complete-fields:'id, name) VALUES' complete-fields:'name = @next_name' complete-fields:'id = @delete_id'
  ### complete-fields:id, name) VALUES
  replace 27:19-27:21
  id  Int? — users
  name  Text? — users
  ### complete-fields:name = @next_name
  replace 28:17-28:21
  id  Int? — users
  name  Text? — users
  ### complete-fields:id = @delete_id
  replace 29:24-29:26
  id  Int? — users
  name  Text? — users

Annotation comments stay outside statement hover:

  $ ../ask.exe q.sql hover:@shared_users 'hover:name FROM users;'
  ### hover:@shared_users
  nothing
  ### hover:name FROM users;
  32:7-32:11
  ```sql
  users.name  Text?
  ```
  
  Declared in `q.sql`

Statement sources shadow schema tables with the same name.

The alias `u` stands for `users`, even though a table named `u` exists:

  $ ../ask.exe shadowing.sql hover:'SELECT u^' def:'SELECT u^' hover:'SELECT u.nam^' def:'SELECT u.nam^'
  ### hover:SELECT u^
  8:7-8:8
  **alias** `u` of `users`
  
  ```sql
  id    Int?
  name  Text?
  ```
  
  Declared in `shadowing.sql`
  ### def:SELECT u^
  shadowing.sql 8:25-8:26
  ### hover:SELECT u.nam^
  8:9-8:13
  ```sql
  users.name  Text?
  ```
  
  Declared in `shadowing.sql`
  ### def:SELECT u.nam^
  shadowing.sql 1:28-1:32

A subquery alias shadows the table with the same name:

  $ ../ask.exe shadowing.sql hover:'SELECT shadow.i^' def:'SELECT shadow.i^'
  ### hover:SELECT shadow.i^
  11:14-11:16
  ```sql
  shadow.id  Int?
  ```
  
  Available in this statement
  ### def:SELECT shadow.i^
  shadowing.sql 11:45-11:51

A CTE shadows the table with the same name:

  $ ../ask.exe shadowing.sql hover:'shadow.id FROM shadow^' def:'shadow.id FROM shadow^'
  ### hover:shadow.id FROM shadow^
  15:22-15:28
  **CTE** `shadow`
  
  ```sql
  id  Int?
  ```
  
  Available in this statement
  ### def:shadow.id FROM shadow^
  shadowing.sql 14:5-14:11
