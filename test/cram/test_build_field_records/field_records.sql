CREATE TABLE users (
  id          BIGINT PRIMARY KEY,
  -- [sqlgg] record=name.given
  given_name  TEXT NOT NULL,
  -- [sqlgg] record=name.family
  family_name TEXT NOT NULL,
  -- [sqlgg] record=contact.email
  email       TEXT,
  -- [sqlgg] record=contact.phone
  phone       TEXT,
  created_at  TIMESTAMP NOT NULL
);

-- @list_users
SELECT id, given_name, family_name, email, phone, created_at FROM users;

-- @get_user
SELECT id, given_name, family_name, email, phone, created_at FROM users WHERE id = @id LIMIT 1;

-- @get_name
SELECT given_name, family_name FROM users WHERE id = @id LIMIT 1;

-- @list_names
SELECT given_name, family_name FROM users;

CREATE TABLE accounts (
  -- [sqlgg] module=User_id
  id          BIGINT PRIMARY KEY,
  -- [sqlgg] record=owner.given
  given_name  TEXT NOT NULL,
  -- [sqlgg] record=owner.family
  family_name TEXT NOT NULL,
  -- [sqlgg] record=owner.email
  -- [sqlgg] module=Email_addr
  email       TEXT NOT NULL,
  balance     BIGINT NOT NULL
);

-- @get_account
SELECT id, given_name, family_name, email, balance FROM accounts WHERE id = @id LIMIT 1;

-- @list_accounts
SELECT id, given_name, family_name, email, balance FROM accounts;

-- @insert_account
INSERT INTO accounts (id, given_name, family_name, email, balance) VALUES (@id, @given_name, @family_name, @email, @balance);

CREATE TABLE orders (
  id          BIGINT PRIMARY KEY,
  -- [sqlgg] module=User_id
  buyer_id    BIGINT NOT NULL,
  -- [sqlgg] record=shipping.city
  ship_city   TEXT NOT NULL,
  -- [sqlgg] record=shipping.zip
  ship_zip    TEXT NOT NULL,
  -- [sqlgg] record=shipping.notify_email
  -- [sqlgg] module=Email_addr
  notify_email TEXT,
  -- [sqlgg] module=Money
  amount      BIGINT NOT NULL
);

-- @get_order
SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE id = @id LIMIT 1;

-- @list_orders_by_buyer
SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE buyer_id = @buyer_id;

-- @insert_order
INSERT INTO orders (id, buyer_id, ship_city, ship_zip, notify_email, amount) VALUES (@id, @buyer_id, @ship_city, @ship_zip, @notify_email, @amount);

-- @get_order_with_buyer
SELECT o.id, o.buyer_id, u.given_name, u.family_name, o.ship_city, o.ship_zip, o.notify_email, o.amount
FROM orders o
JOIN users u ON u.id = o.buyer_id
WHERE o.id = @order_id LIMIT 1;

-- @list_orders_with_buyers
SELECT o.id, o.buyer_id, u.given_name, u.family_name, o.ship_city, o.ship_zip, o.notify_email, o.amount
FROM orders o
JOIN users u ON u.id = o.buyer_id;

-- @left_join_orders
SELECT u.id, u.given_name, u.family_name, o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount
FROM users u
LEFT JOIN orders o ON o.buyer_id = u.id;

-- @right_join_users
SELECT o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount, u.id, u.given_name, u.family_name
FROM orders o
RIGHT JOIN users u ON o.buyer_id = u.id;

-- @left_join_order_one
SELECT u.id, u.given_name, u.family_name, o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount
FROM users u
LEFT JOIN orders o ON o.buyer_id = u.id
WHERE u.id = @user_id LIMIT 1;

-- @union_names
SELECT given_name, family_name FROM users WHERE id = @id1
UNION ALL
SELECT given_name, family_name FROM users WHERE id = @id2;

-- @insert_user
INSERT INTO users (id, given_name, family_name, email, phone, created_at) VALUES (@id, @given_name, @family_name, @email, @phone, @created_at);

-- @update_name
UPDATE users SET given_name = @given_name, family_name = @family_name WHERE id = @id;

-- @update_contact
UPDATE users SET email = @email, phone = @phone WHERE id = @id;
