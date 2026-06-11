CREATE TABLE users (id INT PRIMARY KEY, name TEXT, user_id INT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
CREATE TABLE orders (bio TEXT, amount INT);
-- INNER JOIN removes rows -> never droppable, even with a unique key
-- [sqlgg] dynamic_select=true
-- @inner_join
SELECT u.id, p.bio FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- USING has no ON to analyse -> keep
-- [sqlgg] dynamic_select=true
-- @join_using
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = @uid;
-- NATURAL has no ON to analyse -> keep
-- [sqlgg] dynamic_select=true
-- @join_natural
SELECT u.id, p.bio FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = @uid;
-- a later USING implicitly binds the candidate's column (bio exists only in
-- profiles and orders) -> keep the candidate
-- [sqlgg] dynamic_select=true
-- @using_after_candidate
SELECT u.id, p.bio, o.amount FROM users u LEFT JOIN profiles p ON p.user_id = u.id JOIN orders o USING (bio) WHERE u.id = @uid;
-- same for NATURAL
-- [sqlgg] dynamic_select=true
-- @natural_after_candidate
SELECT u.id, p.bio, o.amount FROM users u LEFT JOIN profiles p ON p.user_id = u.id NATURAL JOIN orders o WHERE u.id = @uid;
