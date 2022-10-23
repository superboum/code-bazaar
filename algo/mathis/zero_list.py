# Goal: put all the zeros at the end of the list.
# The function below works but it's too slow. Can you make it faster?

def solution(nums):
    #print(nums)
    switch_left = 0
    l = len(nums)
    for i in range(l):
        if i >= l - switch_left:
            nums[i] = 0
            continue

        while nums[i+switch_left] == 0:
            switch_left += 1

        nums[i] = nums[i+switch_left]
    #print(nums)
    return nums


# ============ DO NOT EDIT BELOW THIS LINE ============

import collections
import random
import resource

def test_solution(n):
    testcase = generate_testcase(n)
    testcase_counter = collections.Counter(testcase)
    testcase_nonzero = [x for x in testcase if x]
    res_before = resource.getrusage(resource.RUSAGE_SELF)
    result = solution(testcase)
    res_after = resource.getrusage(resource.RUSAGE_SELF)
    if (collections.Counter(result) != testcase_counter
            or result[:-testcase_counter[0]] != testcase_nonzero):
        print('Wrong result.')
        if n <= 100:
            print(f'Input:  {testcase}')
            print(f'Output: {result}')
        assert False
    del testcase
    cpu_time_before = res_before[0] + res_before[1]
    cpu_time_after = res_after[0] + res_after[1]
    cpu_time_use = cpu_time_after - cpu_time_before
    memory_use = res_after[2] - res_before[1]
    return cpu_time_use, memory_use

def generate_testcase(n):
    testcase = []
    for i in range(n):
        x = random.getrandbits(16)
        if not x % min(i+1, 1000):
            testcase.append(0)
        else:
            testcase.append(x)
    return testcase


TESTCASES = [10, 100, 1_000, 5_000, 10_000, 20_000, 30_000, 50_000, 100_000, 500_000, 1_000_000, 5_000_000]
score = 'BEGINNER (testcase #6 below 2s to get intermediate)'
for i, n in enumerate(TESTCASES, start=1):
    time_seconds, mem_usage = test_solution(n)
    if time_seconds > 2:
        print(f'Testcase #{i}: Too slow! ({time_seconds:.2f}s)')
        break
    print(f'Testcase #{i}: Success ({time_seconds:.2f}s)')
    if i == 6:
        score = 'INTERMEDIATE (testcase #12 below 2s to get advanced)'
    if i == 12:
        score = 'ADVANCED (<300 MB memory to get expert)'
        print(f'Memory usage on last testcase: {mem_usage/1024:.1f} MB')
        if mem_usage < 300 * 1024:
            score = 'LEGEND. Congratulations!'

print('Your score:', score)


