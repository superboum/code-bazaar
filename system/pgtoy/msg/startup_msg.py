import enum

class Params(enum.Enum):
    USER = b'user'
    DATABASE = b'database'
    OPTIONS = b'options'
    REPLICATION = b'replication'
    CLIENT_ENCODING = b'client_encoding'
    APPLICATION_NAME = b'application_name'
    END_OF_PARAMS = b''

@dataclass
class StartupMessage:
    params: dict[Params, bytes]
