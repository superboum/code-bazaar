from dataclasses import dataclass
from typing import ClassVar

import msg.serializable as ser
import msg.cmd_id as cmd_id


@dataclass
class Query(ser.Serializable):
    msg_type: ClassVar[cmd_id.FrontMsgType] = cmd_id.FrontMsgType.QUERY
    query_string: str

    def serialize(self, writer: ser.Writer) -> None:
        EOS = b"\x00"
        pl = self.query_string.encode() + EOS
        writer.write_type_len_head(
            self.msg_type,
            len(pl),
        )
        writer.write(pl)
