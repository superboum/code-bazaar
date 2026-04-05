# pgtoy: understanding PostgreSQL by re-implementing some parts

What works:
 - [ ] [pgwire protocol](https://www.postgresql.org/docs/current/protocol.html)
   - [X] Network Handshake
   - [ ] Simple query
   - [ ] Extended query
   - [ ] Cancelling requests
   - [ ] Pipelining
 - [ ] SQL Parsing, probably through [sqlglot](https://github.com/tobymao/sqlglot)
 - [ ] DCL (Data Control Language)
   - [ ] `GRANT` / `REVOKE ...`
 - [ ] DDL (Data Definition Language)
   - [ ] `CREATE TABLE ...` / `DROP TABLE ...`
   - [ ] `ALTER ...`
   - [ ] Index
   - [ ] Constraints
 - [ ] DML (Data Manipulation Language)
 - [ ] DQL (Data Query Language)
   - [ ] Basic `SELECT`
   - [ ] Basic filtering
   - [ ] Joins
   - [ ] Limit
   - [ ] Offset
 - [ ] Data types
   - [ ] Numbers (int, float)
   - [ ] Text (varchar, text)
   - [ ] Others (eg. JSON, ENUM, Time-related, eg.)   
 - [ ] Persistence
   - [ ] In memory
   - [ ] On-disk
 - [ ] Consistency properties
   - [ ] Multi-users
   - [ ] Transactions

 ## Quickstart

 Launch the server:

 ```
 uv run main.py
 ```

 Start your psql client:

 ```
 psql -h localhost -p 5430
 ```
