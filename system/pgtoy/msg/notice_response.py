from dataclasses import dataclass
import enum
from typing import ClassVar

import msg.serializable as ser
import msg.cmd_id as cmd_id
import msg.error as err


class NoticeField(enum.Enum):
    TERMINATOR = b"\x00"
    SEVERITY = b"S"
    EN_SEVERITY = b"V"
    SQLSTATE_CODE = b"C"
    MESSAGE = b"M"


class NoticeSeverity(enum.Enum):
    WARNING = b"WARNING"
    NOTICE = b"NOTICE"
    DEBUG = b"DEBUG"
    INFO = b"INFO"
    LOG = b"LOG"


@dataclass
class NoticeResponse(ser.Serializable):
    msg_type: ClassVar[cmd_id.BackMsgType] = cmd_id.BackMsgType.NOTICE_RESPONSE

    message: str
    severity: NoticeSeverity = NoticeSeverity.INFO
    error_code: err.ErrorCode = err.ErrorCode.SUCCESSFUL_COMPLETION

    def serialize(self, writer: ser.Writer) -> None:
        EOS = b"\x00"
        pl = (
            NoticeField.SEVERITY.value
            + self.severity.value
            + EOS
            + NoticeField.EN_SEVERITY.value
            + self.severity.value
            + EOS
            + NoticeField.SQLSTATE_CODE.value
            + self.error_code.value
            + EOS
            + NoticeField.MESSAGE.value
            + self.message.encode()
            + EOS
            + NoticeField.TERMINATOR.value
        )
        writer.write_type_len_head(
            self.msg_type,
            len(pl),
        )
        writer.write(pl)
