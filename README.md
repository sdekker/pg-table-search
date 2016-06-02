# pg-table-search

This is a simple command-line utility for doing string searches in Postgres
database schemas. 

For example, say we have a database which contains the PSMA Administrative
Boundaries dataset (50 tables, 385 columns) and we want to find attributes
similar to "pop", say, for finding the population count in a particular
geographic region:

```
$ pg-table-search --db 'host=localhost port=5432 dbname=spatialdb user=username' --schema 'admin_boundaries' pop

(1.0,"pop","seifa_2011","admin_boundaries")
(0.46153846153846156,"mb11_pop","mb_2011","admin_boundaries")
(0.4,"population","town","admin_boundaries")
(0.3076923076923077,"postcode","locality","admin_boundaries")
```

Using a Jaccard similarity measure, the attribute names in the database are
ranked by their similarity to a given search term. This has proven useful when
mapping out unfamiliar and under-constrained databases.
