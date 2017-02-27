---
title: "Untitled"
output: html_document
---

### Unused Indexes (since last restart)

MySQL performance depends on a balanced usage of MySQL indexes. While it is easy to add an index and identify queries not using indexes via EXPLAIN during development or slow.log it is a lot harder to get rid of unused indexes. Finding and removing them might be crucial for your performance as indexes can create a remarkable cpu cycle and i/o overhead during updates to tables (INSERT/UPDATE/DELETE).

### Indexes with low cardinality

A lot of advice is available about identifying appropriate indexes to create or identifying bad indexes to drop. It may seem logical that an index on a field like account enabled, which has a very small set of unique values (yes, no), could substantially reduce a result set. In light of the geometry of B-tree indexes, an index with a low number of possible values can actually harm performance rather than help it.

### Nullable Indexes

If an indexed column cannot contain any NULL values, declare it as NOT NULL when you create the table. The optimizer can better determine which index is most effective to use for a query, when it knows whether each column contains NULL values.
