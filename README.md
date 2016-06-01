# pg-table-search

This is a simple command-line utility for doing string searches in Postgres database schemas. 

Using a Jaccard index, the attribute names in the database can be ranked by their similarity to a given
search term. This has proven useful when mapping out unfamiliar and under-constrained databases.
