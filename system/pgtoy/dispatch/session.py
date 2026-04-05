from dataclasses import dataclass, field
from typing import Sequence, assert_never
import logging

import error as err
import ctrl.markers as markers
import dispatch.handshake as handshake
import dispatch.query as query
import msg.all as msg
import msg.handshake as hmsg
import msg.serializable as ser

logger = logging.getLogger(__name__)

class Disconnected:
    pass

AnyDispatcher = handshake.Dispatcher | query.Dispatcher | Disconnected

@dataclass
class Dispatcher:
    state: AnyDispatcher = field(default_factory=lambda: handshake.Dispatcher())

    def is_connected(self) -> bool:
        match self.state:
            case Disconnected():
                return False
            case handshake.Dispatcher() | query.Dispatcher():
                return True
            case _:
                assert_never(self.state)

    def register(self, recv_msg: msg.FrontMsg) -> Sequence[msg.BackMsg]:
        # Controller / sub-dispatcher routing
        to_send: list[msg.BackMsg] = []
        match (self.state, recv_msg):
            case (handshake.Dispatcher(), m) if isinstance(m, hmsg.FrontMsg):
                to_send = list(self.state.register(m))
            case (_, _):
                raise Exception("Not yet implemented")
            case rest:
                assert_never(rest)

        # State update (ie. state machine transition)
        match self.state:
            case handshake.Dispatcher(state=handshake.Done()):
                self.state = query.Dispatcher()
                to_add: list[msg.BackMsg] = list(self.state.init_msg())
                to_send = to_send + to_add
                logger.info("handshake -> query")
            case _:
                pass

        return to_send


