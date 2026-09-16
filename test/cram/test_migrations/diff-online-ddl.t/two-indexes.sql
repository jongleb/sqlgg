-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_index_z_email_idx
ALTER TABLE `users` ADD INDEX `z_email_idx` (`email`), ALGORITHM=INPLACE, LOCK=NONE;
ALTER TABLE `users` DROP INDEX `z_email_idx`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_col_middle_col
ALTER TABLE `users` ADD COLUMN `middle_col` INT;
ALTER TABLE `users` DROP COLUMN `middle_col`;

-- [sqlgg] generated
-- [sqlgg] id=20260101000000_alter_users_add_index_a_name_idx
ALTER TABLE `users` ADD INDEX `a_name_idx` (`name`), ALGORITHM=COPY, LOCK=SHARED;
ALTER TABLE `users` DROP INDEX `a_name_idx`;
