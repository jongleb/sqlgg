CREATE TABLE users (id INT PRIMARY KEY, name TEXT, email TEXT, org INT, dept INT);
CREATE TABLE accounts (id INT PRIMARY KEY, email TEXT UNIQUE, label TEXT);
CREATE TABLE memberships (org INT, dept INT, title TEXT, PRIMARY KEY (org, dept));
-- a non-PK UNIQUE column is as good as a PRIMARY KEY -> droppable
-- [sqlgg] dynamic_select=true
-- @unique_key
SELECT u.id, a.label FROM users u LEFT JOIN accounts a ON a.email = u.email WHERE u.id = @uid;
-- composite PRIMARY KEY, only one part equated: can multiply rows -> keep
-- [sqlgg] dynamic_select=true
-- @composite_partial
SELECT u.id, m.title FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = @uid;
-- composite PRIMARY KEY, ALL parts equated -> droppable
-- [sqlgg] dynamic_select=true
-- @composite_full
SELECT u.id, m.title FROM users u LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept WHERE u.id = @uid;
