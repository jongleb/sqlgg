CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- Two scoped (fixed-SQL) queries that expose id and name at DIFFERENT absolute
-- column positions. A single reusable field-set fragment must decode both
-- correctly: scoped selectors read by absolute index and the fragment is aligned
-- BY NAME via the shared [Scope] module.

-- [sqlgg] scoped=true
-- @scope_q1
SELECT id, name, price, category FROM products WHERE id = @id;

-- [sqlgg] scoped=true
-- @scope_q2
SELECT stock, id, name FROM products WHERE stock > @min_stock;
