from dataclasses import dataclass, field
from typing import Sequence

import msg.query as msg


class Live:
    pass


class Terminated:
    pass


State = Live | Terminated


@dataclass
class Dispatcher:
    state: State = field(default_factory=lambda: Live())

    def init_msg(self) -> Sequence[msg.BackMsg]:
        return [
            msg.NoticeResponse(),
            msg.ReadyForQuery(),
        ]

    def register(self, m: msg.FrontMsg) -> Sequence[msg.BackMsg]:
        match m:
            case msg.Terminate():
                self.state = Terminated()
                return []
            case _:
                raise Exception("Not yet implemented")
