from dataclasses import dataclass
import enum

import msg.serializable as ser


class ErrorCode(enum.Enum):
    # see: https://www.postgresql.org/docs/current/errcodes-appendix.html
    SUCCESSFUL_COMPLETION = b"00000"
    WARNING = b"00001"
    # ...


@dataclass
class ErrorResponse(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        raise Exception("Not yet implemented")
