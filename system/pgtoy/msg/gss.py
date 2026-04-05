import msg.serializable as ser


class GSSRequest:
    pass


class GSSResponseOk(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        writer.write(b"G")


class GSSResponseNo(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        writer.write(b"N")


GSSResponse = GSSResponseOk | GSSResponseNo
