CREATE TABLE products (
    id INT PRIMARY KEY,
    name TEXT,
    price DECIMAL(10,2),
    category TEXT,
    stock INT
);

-- Two dynamic_scoped queries (dynamic_select + scoped): columns are chosen at
-- runtime AND [type 'a t] is shared via the generated Dynamic_select module, so a
-- single fragment is reusable across both (this is the third mode, distinct from
-- pure dynamic which keeps a unique per-query [t]).

-- [sqlgg] dynamic_select=true
-- [sqlgg] scoped=true
-- @dscope_q1
SELECT id, name, price, category FROM products WHERE id = @id;

-- [sqlgg] dynamic_select=true
-- [sqlgg] scoped=true
-- @dscope_q2
SELECT id, name, stock FROM products WHERE stock > @min_stock;
