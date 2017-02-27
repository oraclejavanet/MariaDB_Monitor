---
title: "Untitled"
output: html_document
---

### Optimizing SQL Statements

Long runtimes or table scans are a danger sign that a query can be speeded up significantly. For tables with more than a few rows, consider redesigning the query by adding an index for one or more of the columns tested in the WHERE clause. Put extra effort into avoiding table scans for queries that perform joins or reference foreign keys. If the nature of the data means there is no way to avoid reading all the rows, then it might not be practical to make the query faster, or making it faster might involve extensive restructuring of your tables. The following chapters link to the MySQL Reference Manual with information to optimize sql statements:

- [Optimizing SELECT Statements](https://dev.mysql.com/doc/refman/5.5/en/select-optimization.html)
- [Optimizing DML Statements](https://dev.mysql.com/doc/refman/5.5/en/non-select-optimization.html)
- [Optimizing Database Privileges](https://dev.mysql.com/doc/refman/5.5/en/permission-optimization.html)
- [Optimizing INFORMATION_SCHEMA Queries](https://dev.mysql.com/doc/refman/5.5/en/information-schema-optimization.html)
- [Other Optimization Tips](https://dev.mysql.com/doc/refman/5.5/en/miscellaneous-optimization-tips.html)
