from typing import Sequence
from sqlglot import parse_one

import msg.query as msg
import msg.command_complete as cc
import persist.inmem as persist


def simple_query(inst: persist.Instance, q: msg.Query) -> Sequence[msg.BackMsg]:
    ast = parse_one(q.query_string, dialect="postgres")
    print(repr(ast))

    import sqlglot.expressions as sql

    match ast:
        case sql.Create(kind="TABLE"):
            print("create table")
        case _:
            print("other SQL statement")

    return [
        msg.CommandComplete(cc.TagSelect(0)),
        msg.ReadyForQuery(),
    ]
