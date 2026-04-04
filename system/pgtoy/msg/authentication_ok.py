
class AuthenticationOk(Serializable):
    msg_type = BackMsgType.AUTHENTICATION
    static_len = 8
    success_val = 0

    def serialize(self) -> bytes:
        # msg_type = 1 byte
        # len = 4 bytes
        # success_val = 4 bytes
        return struct.pack(
            "!cII", 
            self.msg_type.value,
            self.static_len,
            self.success_val,
        )
