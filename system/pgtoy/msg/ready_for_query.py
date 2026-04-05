from dataclasses import dataclass
import enum

import msg.serializable as ser
import msg.cmd_id as cmd_id


class TxStatus(enum.Enum):
    IDLE = b"I"
    IN_TX = b"T"
    FAILED = b"E"


@dataclass
class ReadyForQuery(ser.Serializable):
    msg_type: cmd_id.BackMsgType = cmd_id.BackMsgType.READY_FOR_QUERY
    status: TxStatus = TxStatus.IDLE

    def serialize(self, writer: ser.Writer) -> None:
        writer.write_type_len_head(
            self.msg_type,
            len(self.status.value),
        )
        writer.write(self.status.value)
