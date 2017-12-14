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

## Examples and caveats

This is a list of examples.

### Just a function

This is just a function and everything works with that:

```plpgsql
CREATE OR REPLACE FUNCTION schema_init () RETURNS void AS $$
begin
    -- This is needed to stop crystal's pg lib barfing on the output
    SET client_min_messages = error;
    
    -- Routes
    CREATE SEQUENCE IF NOT EXISTS route_ids;
    CREATE TABLE IF NOT EXISTS route ("id" INTEGER,
                                      "path" TEXT,
                                      "port" INTEGER);

    PERFORM id FROM route WHERE path = '/';
    if not found then
        INSERT INTO route (id, path, port)
        VALUES (nextval('route_ids'), '/', 8001);
    end if;
end;
$$ LANGUAGE plpgsql;
```

### ifs

Ifs are a bit of a problem because of `elsif`. I am minded not to fix
this bug and just accept that one needs a newline before the `elsif`:

```plpgsql
CREATE OR REPLACE FUNCTION wiki_materialize () RETURNS trigger AS $$
begin
    -- we should never have a delete
    if (TG_OP = 'DELETE') then
        INSERT INTO emp_audit SELECT 'D', now(), user, OLD.*;
        RETURN OLD;

    elsif (TG_OP = 'UPDATE') then
        INSERT INTO emp_audit SELECT 'U', now(), user, NEW.*;
        RETURN NEW;

    elsif (TG_OP = 'INSERT') then
        INSERT INTO emp_audit SELECT 'I', now(), user, NEW.*;
        RETURN NEW;
    END IF;
    RETURN NULL; -- result is ignored since this is an AFTER trigger
end;
$$ LANGUAGE plpgsql;
```

`else` works too as the last thing but you need a newline in front of
that as well.

## Installing

Just eval the [sql-postgres](sql-postgres.el) file from this
repository and it will add itself to the `sql-mode` hook such that it
will be the indenter.

It's a simplistic hook, all sql will be considered postgres. That
might not be what you want.

Send me a better hook?


## Tests

The tests are appalling but were enough to help me get it done.
