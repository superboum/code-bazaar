import msg.serializable as ser


class SSLRequest:
    pass


class SSLResponseOk(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        writer.write(b"S")


class SSLResponseNo(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        writer.write(b"N")


SSLResponse = SSLResponseOk | SSLResponseNo
