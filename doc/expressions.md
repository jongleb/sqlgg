# Expressions

[← Back to Main](./index.md)

---

sqlgg extends SQL with special expression syntax for dynamic queries.

## `@choice` — Conditional Branches

Pattern matching-like conditional logic:

```sql
@choice { A { expr1 } | B { expr2 } | C { expr3 } }
```

### Basic Example

```sql
-- @search_products
SELECT * FROM products 
WHERE price > @choice { 
    Cheap { 10.0 } | 
    Expensive { @min_price } |
    All { 0.0 }
};
```

- `Cheap` — no parameter, constant `10.0`
- `Expensive` — takes `@min_price` parameter
- `All` — no parameter, constant `0.0`

### With Different Types

```sql
-- @filter
SELECT * FROM test_table 
WHERE @choice {
    ById { id = @id } |
    ByName { name LIKE @pattern } |
    ByRange { created_at BETWEEN @start AND @end } |
    All { 1 = 1 }
};
```

- `ById`: `int`
- `ByName`: `string`
- `ByRange`: `(timestamp * timestamp)` tuple
- `All`: no parameter

### In ORDER BY

```sql
-- @sorted
SELECT * FROM products 
ORDER BY @choice { ByPrice { price } | ByName { name } | ById { id } } DESC;
```

All branches reference columns — no parameters.

**OCaml representation:** [Polymorphic Variants](./ocaml-specifics.md#choice--polymorphic-variants)

## `IN @list` — List Parameters

Dynamic IN clauses:

```sql
-- @find_by_ids
SELECT * FROM users WHERE id IN @user_ids;
```

`@user_ids`: `int list`

```sql
-- @exclude_departments
SELECT * FROM users WHERE department NOT IN @excluded;
```

`@excluded`: `string list`

**OCaml representation:** [List Parameters](./ocaml-specifics.md#in-list--list)

## `IN @tuple_list` — Tuple List Parameters

Multi-column IN:

```sql
-- @find_roles
SELECT * FROM user_roles 
WHERE (user_id, role_id) IN @pairs;
```

`@pairs`: `(int * int) list`

```sql
-- @find_by_criteria
SELECT * FROM user_roles 
WHERE (user_id, department, is_active) IN @criteria;
```

`@criteria`: `(int * string * int) list`

**OCaml representation:** [Tuple List Parameters](./ocaml-specifics.md#in-tuple_list--tuple-list)

## `{...}?` — Optional Clauses

Conditional query parts:

```sql
-- @find_users
SELECT * FROM users
WHERE { age > @min_age }?
  AND { department = @dept }?;
```

- If `@min_age` is `Some` → includes `age > ?`
- If `@min_age` is `None` → becomes `TRUE`

### With DEFAULT

```sql
CREATE TABLE tbl (
    id INTEGER PRIMARY KEY,
    value TEXT DEFAULT 'default'
);

-- @insert
INSERT INTO tbl SET value = { CONCAT(@prefix, 'suffix') }??;
```

- If `@prefix` is `Some` → uses CONCAT expression
- If `@prefix` is `None` → uses DEFAULT value

**OCaml representation:** [Optional Parameters](./ocaml-specifics.md#--option)

[← Back to Main](./index.md)

