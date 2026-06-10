CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT);
-- a parameter inside ON makes positional binding unstable -> keep
-- [sqlgg] dynamic_select=true
-- @param_in_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = @b WHERE u.id = @uid;
-- an extra constant conjunct is harmless: the unique key is still equated -> droppable
-- [sqlgg] dynamic_select=true
-- @extra_const_on
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = @uid;
-- inequality cannot guarantee a single row -> keep
-- [sqlgg] dynamic_select=true
-- @on_inequality
SELECT u.id, p.bio FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = @uid;
-- a table joined without an alias is matched by its own name -> droppable
-- [sqlgg] dynamic_select=true
-- @no_alias
SELECT u.id, profiles.bio FROM users u LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = @uid;
