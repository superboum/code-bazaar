class SSLRequest:
    pass

class SSLResponseOk:
    def serialize(self) -> bytes:
        return b'S'

class SSLResponseNo:
    def serialize(self) -> bytes:
        return b'N'

type SSLResponse = SSLResponseOk | SSLResponseNo
