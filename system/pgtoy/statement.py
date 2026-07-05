import persist.inmem as persist
import sqlglot as sql
import sqlglot.expressions as expr
import persist.inmem as persist

"""
Create(
  this=Schema(
    this=Table(
      this=Identifier(this=foo, quoted=False)),
    expressions=[
      ColumnDef(
        this=Identifier(this=bar, quoted=False),
        kind=DataType(this=DType.INT, nested=False))]),
  kind=TABLE,
  replace=False,
  refresh=False,
  unique=False,
  exists=False,
  concurrently=False)
"""

def create_table(inst: persist.Database, schema: expr.Schema) -> None:
    persist.Table("foo")
    


def execute(inst: persist.Database, query: str) -> None:
    ast = sql.parse_one(query, dialect="postgres")
    print(repr(ast))

    match ast:
        case expr.Create(kind="TABLE"):
            print("create table")
        case _:
            print("other SQL statement")

    return None
