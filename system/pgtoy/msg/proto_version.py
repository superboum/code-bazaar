class ProtoVersion(enum.Enum):
    Unknown = (0, 0)
    GSSENCRequest = (1234, 5680)
    SSLRequest = (1234, 5679)
    Postgres20 = (2, 0)
    Postgres30 = (3, 0)
    Postgres32 = (3, 2)
