import msg.serializable as ser
import msg.cmd_id as cmd_id

class AuthenticationOk(ser.Serializable):
    msg_type = cmd_id.BackMsgType.AUTHENTICATION
    static_len = 8
    success_val = 0

    def serialize(self, writer: ser.ExtStreamWriter) -> None:
        pl = self.success_val.to_bytes(
            ser.Sz.U32.value,
            ser.ENDIAN,
        )
        writer.write_type_len_head(self.msg_type, len(pl))
        writer.write(pl)
