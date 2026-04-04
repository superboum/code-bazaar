async def startup_message(reader: io.Reader, mlen: int) -> StartupMessage:
    buf = read_many(reader, mlen)
    res = StartupMessage(params=dict())
    splitted = plain_data.split(b'\x00')
    for k, v in zip(splitted[::2], splitted[1::2]):
        try:
            res.params[Params(k)] = v
        except Exception as e:
            print(f"Skip unknown kv pair: {k}")

    return res
