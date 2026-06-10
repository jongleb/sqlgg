CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
-- referenced in GROUP BY -> keep
-- [sqlgg] dynamic_select=true
-- @ref_in_group
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio;
-- referenced in ORDER BY -> keep
-- [sqlgg] dynamic_select=true
-- @ref_in_order
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio;
-- referenced in HAVING -> keep
-- [sqlgg] dynamic_select=true
-- @ref_in_having
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0;
-- complex projection expression cannot be attached to a single column -> keep
-- [sqlgg] dynamic_select=true
-- @complex_proj
SELECT u.id, CONCAT(p.bio, '!') AS shout FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
-- a subquery anywhere in WHERE is unanalysed -> conservative keep
-- [sqlgg] dynamic_select=true
-- @subq_in_where
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles);
-- an unqualified column resolving to the joined table -> keep
-- [sqlgg] dynamic_select=true
-- @unqualified_where
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x';
-- a droppable join whose columns are never selected gets no hole: rendered statically
-- [sqlgg] dynamic_select=true
-- @join_unreferenced
SELECT u.id, u.name FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = @uid;
