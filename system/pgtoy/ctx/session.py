from dataclasses import dataclass

from .base import SessionState
from ..msg.handshake import StartupMessage

@dataclass
class SessionState:
    pass

@dataclass
class FreshConnection(SessionState):
    pass

@dataclass
class Logged(SessionState):
    connection_params: StartupMessage

@dataclass
class Disconnected(SessionState):
    pass
    

