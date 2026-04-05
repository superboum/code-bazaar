class NetworkProtocolParseError(Exception):
    pass

class UnknownPostgresProtocolVersion(NetworkProtocolParseError):
    pass

class Protocolv2NotSupported(NetworkProtocolParseError):
    pass

class InvalidMessageForState(Exception):
    pass


