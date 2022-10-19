## AsyncPool

Sometimes you have so many requests that are paralellizable that you would exhaust your memory.
For example if you have 10 000 URLs to fetch, it will be probably more efficient to always fetch
100 of them concurrently. To code such logic, you can combine async queues with coroutine tasks.

This is what I have done in `async_pool.py`. You can then use this class as follow:

```python
import asyncio, random, logging
from async_pool import AsyncPool

async def slow_io(v):
    sleep = random.randint(1,10)
    await asyncio.sleep(sleep)
    logging.debug(f"{v} slept for {sleep} sec, done.")

async def main():
    pool = AsyncPool(concurrency=3, name="example")
    for task in range(9):
        pool.put(slow_io, task)
    await pool.process()

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    asyncio.run(main())
```

Or you can run it directly with:

```bash
python3 main.py
```
