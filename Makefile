.PHONY: build doc test clean top

build:
	dune build

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest
	./run_test

top:
	dune utop

VERSION:=$(shell date +%Y%m%d)
NAME=sqlgg-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) $(VERSION)
	git archive --prefix=$(NAME)/ $(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz -o $(NAME).tar.gz.asc

sql_%.ml: gen_%.sql ../monorepo/backend/api/src/core/db_gen/gen_create_tables.sql
	./_build/install/default/bin/sqlgg -static-header -gen caml_io -name Make -params unnamed \
  -category none ../monorepo/backend/api/src/core/db_gen/gen_create_tables.sql \
  -category all $< \
  > $@ || rm $@  

../monorepo/backend/api/src/core/db_gen/sqlddl_tables.ml: ../monorepo/backend/api/src/core/db_gen/gen_create_tables.sql
	./_build/install/default/bin/sqlgg -gen caml_io -name Make -params unnamed -no-header-timestamp \
		-category ddl $^ > $@.out && mv $@.out $@ || rm $@.out  

gen_sql: \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_crawls.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ca_projects.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_academy_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_agency_profiles.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ahrefs_domain_rating0.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ahrefs_domain_rating1.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ai_grader_history.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_alerts.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_async_exports.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_banner_metrics.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ce_presets.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_closed_notifications.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_companies.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_companies_contact_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_competitors.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_csv_download.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_da_app_objects_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_da_cache.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_da_cache_info.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_da_documents_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_dynamic_params.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_gsc_imports.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_gsc_req_credits.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_ke_requests.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_keywords_lists.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_keywords_lists_data.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_notification_events.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_notifications.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_op_documents.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_pages.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_parser_stats.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_performance_indicators.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_portfolios.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_product_updates.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_projects.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_rt_keywords.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_rt_keywords_tags.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_rt_notes.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_rt_projects_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_crawl_cache.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_filter.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_issue.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_issue_user.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_issue_user_project.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_preset.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_projects.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_sa_projects_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_se_overview_settings.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_se_overview_portfolio_settings.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_teammates.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_third_party_applications.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_update_notes.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_usage_metrics.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_users.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_users_history.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_we_history.ml \
  ../monorepo/backend/api/src/core/db_gen/sql_we_queries.ml \
  ../monorepo/backend/api/src/core/migrations/sql_migrations.ml \
  ../monorepo/backend/api/src/core/db_gen/sqlddl_tables.ml \
