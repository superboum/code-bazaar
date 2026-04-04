import enum

class ErrorCode(enum.Enum):
    # see: https://www.postgresql.org/docs/current/errcodes-appendix.html
    SUCCESSFUL_COMPLETION = b'00000'
    WARNING = b'00001'
    # ...
