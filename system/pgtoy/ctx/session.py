from dataclasses import dataclass

from msg.handshake import StartupMessage


@dataclass
class Ctx:
    pass


@dataclass
class Fresh(Ctx):
    pass


@dataclass
class Logged(Ctx):
    connection_params: StartupMessage


@dataclass
class Disconnected(Ctx):
    pass
