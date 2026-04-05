from dataclasses import dataclass, field
from typing import Sequence

import msg.query as msg
import msg.parameter_status as params
import ctrl.simple_query as ctrl
import persist.inmem as persist


class Live:
    pass


class Terminated:
    pass


State = Live | Terminated


@dataclass
class Dispatcher:
    inst: persist.Instance
    state: State = field(default_factory=lambda: Live())

    def init_msg(self) -> Sequence[msg.BackMsg]:
        return [
            msg.NoticeResponse("Welcome on pgtoy!"),
            msg.ParameterStatus({params.ParamField.SERVER_VERSION: "17.99"}),
            msg.ReadyForQuery(),
        ]

    def register(self, m: msg.FrontMsg) -> Sequence[msg.BackMsg]:
        match m:
            case msg.Terminate():
                self.state = Terminated()
                return []
            case msg.Query():
                return ctrl.simple_query(self.inst, m)
            case _:
                raise Exception("Not yet implemented")
