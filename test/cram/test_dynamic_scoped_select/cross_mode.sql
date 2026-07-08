CREATE TABLE users (
    id INT PRIMARY KEY,
    name TEXT,
    email TEXT
);

-- [sqlgg] scoped=true
-- @fixed_q
SELECT id, name, email FROM users WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- [sqlgg] scoped=true
-- @dyn_q
SELECT id, name FROM users WHERE id = @id;
