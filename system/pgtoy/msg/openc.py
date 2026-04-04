# Opportunistic Encryption

class GSSRequest:
    pass

class GSSResponseOk:
    def serialize(self) -> bytes:
        return b'G'

class GSSResponseNo:
    def serialize(self) -> bytes:
        return b'N'

type GSSResponse = GSSResponseOk | GSS

class SSLRequest:
    pass

class SSLResponseOk:
    def serialize(self) -> bytes:
        return b'S'

class SSLResponseNo:
    def serialize(self) -> bytes:
        return b'N'

type SSLResponse = SSLResponseOk | SSLResponseNo
