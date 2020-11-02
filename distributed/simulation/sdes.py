# Simple Discrete Event Simulator
def run(algo, collect, node_number): 
  mailbox = [[] for i in range(node_number)]
  next_mailbox = [[] for i in range(node_number)]
  node_state = [{} for i in range(node_number)]
  mailbox_dirty = True
  current_round = 0

  while mailbox_dirty:
    mailbox_dirty = False
    current_round += 1

    for i in range(node_number):
      to_send = algo(mailbox[i],node_state[i],current_round,i,node_number)
      for k,v in to_send.items():
        next_mailbox[k].append(v)
        mailbox_dirty = True


    mailbox = next_mailbox
    next_mailbox = [[] for i in range(node_number)]
  collect(node_state)
