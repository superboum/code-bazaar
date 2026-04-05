from dataclasses import dataclass

import msg.serializable as ser


@dataclass
class CommandComplete(ser.Serializable):
    def serialize(self, writer: ser.Writer) -> None:
        raise Exception("Not yet implemented")
