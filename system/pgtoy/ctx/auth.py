from dataclasses import dataclass

@dataclass
class State:
    pass

@dataclass
class Anonymous(State):
    pass

@dataclass
class FailedAuth(State):
    pass

@dataclass
class Logged(State):
    pass
