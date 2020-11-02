#!/usr/bin/python3
from sdes import run
from random import randint

def infect_and_die(my_msg, my_state, current_round, my_id, node_number):
  # init
  if my_id == 0 and current_round == 1: my_msg.append("hello")

  # core
  fanout = 5
  to_send = {}
  for m in my_msg:
    if m in my_state: continue
    my_state[m] = True
    for f in range(fanout):
      to_send[randint(0, node_number-1)] = m
  return to_send

def iad_collect(states):
  for i in range(len(states)):
    if "hello" not in states[i]:
      print(f"{i} did not receive 'hello'")

run(infect_and_die, iad_collect, 1000)
