from dataclasses import dataclass, field

ValType = int | str


@dataclass
class Column:
    name: str
    kind: type[ValType]


@dataclass
class Table:
    name: str
    cols: list[Column] = field(default_factory=list)
    rows: list[list[ValType]] = field(default_factory=list)


@dataclass
class Database:
    name: str
    tables: dict[str, Table] = field(default_factory=dict)


@dataclass
class Instance:
    databases: dict[str, Database] = field(default_factory=dict)
