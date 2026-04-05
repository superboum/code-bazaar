from dataclasses import dataclass, field
from typing import Sequence, assert_never

import error as error
import msg.handshake as msg
import ctrl.handshake as ctrl
import ctrl.markers as mark


@dataclass
class Anonymous:
    pass


@dataclass
class FailedAuth:
    pass


@dataclass
class Done:
    pass


State = Anonymous | FailedAuth | Done


@dataclass
class Dispatcher:
    state: State = field(default_factory=lambda: Anonymous())

    def register(self, m: msg.FrontMsg) -> Sequence[msg.BackMsg]:
        # Route
        match self.state:
            case Anonymous():
                match m:
                    case msg.GSSRequest():
                        to_send, markers = ctrl.handle_gss(m)
                    case msg.SSLRequest():
                        to_send, markers = ctrl.handle_ssl(m)
                    case msg.StartupMessage():
                        to_send, markers = ctrl.handle_startup_message(m)
                    case unreachable1:
                        assert_never(unreachable1)
            case FailedAuth() | Done():
                raise error.InvalidMessageForState()
            case unreachable2:
                assert_never(unreachable2)

        # Update state
        match (self.state, markers):
            case (Anonymous(), mark.HandshakeMarkers(sent_authentication_ok=True)):
                # Handshake always terminate with AuthenticatedOk()
                self.state = Done()
            case _:
                pass

        return to_send
