class TxStatus(enum.Enum):
    IDLE = b'I'
    IN_TX = b'T'
    FAILED = b'E'

@dataclass
class ReadyForQuery(Serializable):
    msg_type: BackMsgType = BackMsgType.READY_FOR_QUERY
    static_len = 5

    status: TxStatus = TxStatus.IDLE

    def serialize(self) -> bytes:
        return struct.pack(
            "!cIc",
            self.msg_type.value,
            self.static_len,
            self.status.value,
        )
