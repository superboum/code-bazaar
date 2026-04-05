class GSSRequest:
    pass


class GSSResponseOk:
    def serialize(self) -> bytes:
        return b"G"


class GSSResponseNo:
    def serialize(self) -> bytes:
        return b"N"


type GSSResponse = GSSResponseOk | GSSResponseNo
