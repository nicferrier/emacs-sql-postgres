# Indent Postgres' PlPgSQL

Postgres has a [great little procedural programming
language](https://www.postgresql.org/docs/9.6/static/plpgsql.html),
very like Oracle's PL/SQL and this Emacs code offers an indenter for
it.

## Indenting is hard

A much better way of doing indenting than the way I do it here would
be to [use the Postgres
parser](https://wiki.postgresql.org/wiki/Query_Parsing).

However, what I have here does work.

## Installing

Just eval the [sql-postgres](sql-postgres.el) file from this
repository and it will add itself to the `sql-mode` hook such that it
will be the indenter.

It's a simplistic hook, all sql will be considered postgres. That
might not be what you want.

Send me a better hook?


## Tests

The tests are appalling but were enough to help me get it done.
