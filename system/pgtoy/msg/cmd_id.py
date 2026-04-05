import enum


class BackMsgType(enum.Enum):
    UNKNOWN = b""
    AUTHENTICATION = b"R"  # multiple alternatives in context
    BACKEND_KEY_DATA = b"K"
    BIND_COMPLETE = b"2"
    COMMAND_COMPLETE = b"3"  # also CLOSE_COMPLETE
    COPY_DATA = b"d"
    COPY_DONE = b"c"
    COPY_IN_RESPONSE = b"G"
    COPY_OUT_RESPONSE = b"H"
    COPY_BOTH_RESPONSE = b"W"
    DATA_ROW = b"D"
    EMPTY_RESPONSE = b"E"
    FUNCTION_CALL_RESPONSE = b"V"
    NEGOTIATE_PROTOCOL_VERSIOn = b"v"
    NO_DATA = b"n"
    NOTICE_RESPONSE = b"N"
    NOTIFICATION_RESPONSE = b"A"
    PARAMETER_DESCRIPTION = b"B"
    PARAMETER_STATUS = b"S"
    PARSE_COMPLETE = b"1"
    PORTAL_SUSPENDED = b"s"
    READY_FOR_QUERY = b"Z"
    ROW_DESCRIPTION = b"T"


class FrontMsgType(enum.Enum):
    BIND = b"B"
    CLOSE = b"C"
    COPY_DATA = b"d"
    COPY_DONE = b"c"
    COPY_FAIL = b"f"
    DESCRIBE = b"D"
    EXECUTE = b"E"
    FLUSH = b"H"
    FUNCTION_CALL = b"F"
    PARSE = b"P"
    PASSWORD_RESPONSE = b"p"
    QUERY = b"Q"
    SYNC = b"S"
    TERMINATE = b"X"


type MsgType = FrontMsgType | BackMsgType
