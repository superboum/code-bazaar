from dataclasses import dataclass
from typing import ClassVar

import msg.serializable as ser
import msg.cmd_id as cmd_id


@dataclass
class TagInsert:
    oid: ClassVar[int] = 0  # old deprecated field, must be zero now
    rows: int

    def to_bytes(self) -> bytes:
        return f"INSERT {self.oid} {self.rows}".encode()


@dataclass
class TagDelete:
    rows: int

    def to_bytes(self) -> bytes:
        return f"DELETE {self.rows}".encode()


@dataclass
class TagSelect:
    rows: int

    def to_bytes(self) -> bytes:
        return f"SELECT {self.rows}".encode()


AllTags = TagInsert | TagDelete | TagSelect


@dataclass
class CommandComplete(ser.Serializable):
    msg_type: ClassVar[cmd_id.BackMsgType] = cmd_id.BackMsgType.COMMAND_COMPLETE
    cmd_tag: AllTags

    def serialize(self, writer: ser.Writer) -> None:
        EOS = b"\x00"
        pl = self.cmd_tag.to_bytes() + EOS
        writer.write_type_len_head(
            self.msg_type,
            len(pl),
        )
        writer.write(pl)
