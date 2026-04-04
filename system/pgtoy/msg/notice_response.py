from dataclasses import dataclass
import enum

class NoticeField(enum.Enum):
    TERMINATOR = b'\x00'
    SEVERITY = b'S'
    EN_SEVERITY = b'V'
    SQLSTATE_CODE = b'C'
    MESSAGE = b'M'

class NoticeSeverity(enum.Enum):
    WARNING = b'WARNING'
    NOTICE = b'NOTICE'
    DEBUG = b'DEBUG'
    INFO = b'INFO'
    LOG = b'LOG'

@dataclass
class NoticeResponse(Serializable):
    msg_type: BackMsgType = BackMsgType.NOTICE_RESPONSE

    severity: NoticeSeverity = NoticeSeverity.INFO
    error_code: ErrorCode = ErrorCode.SUCCESSFUL_COMPLETION
    message: bytes = b'hello world'

    def serialize(self) -> bytes:
        MSG_LEN_SZ = 4
        EOS = b'\x00'
        pl = (
            NoticeField.SEVERITY.value + self.severity.value + EOS +
            NoticeField.EN_SEVERITY.value + self.severity.value + EOS +
            NoticeField.SQLSTATE_CODE.value + self.error_code.value + EOS +
            NoticeField.MESSAGE.value + self.message + EOS +
            NoticeField.TERMINATOR.value
        )
        msg_len = MSG_LEN_SZ + len(pl) 
        r = struct.pack(
            "!cI", 
            self.msg_type.value,
            msg_len,
        ) + pl
        return r
