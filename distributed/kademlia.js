const dgram = require('dgram')
const crypto = require('crypto')

const id_size = 32
const timeout = 2000

const kbuckets = Array.from({length: id_size}, () => [])
const xorbuf = (b1, b2) => b1.map((v,i) => v ^ b2[i])
const pending_requests = {}
const handle_rpc = {
  res: (fd, msg, meta) => {
    if (!pending_requests[msg.uid]) {
      console.error(`No pending request for UID ${msg.uid}`)
      return 
    }
    clearTimeout(pending_requests[msg.uid].timeout);
    pending_requests[msg.uid].resolve([fd,msg,meta])
    delete pending_requests[msg.uid]
  },
  ping: (fd, msg, meta) => {
    const res = JSON.stringify({uid: msg.uid, action: 'res'})
    fd.send(res, meta.port, meta.ip, err => err ? console.error(err) : null)
  },
  find_node: (fd, msg, meta) => null,
  find_value: (fd, msg, meta) => null,
  store: (fd, msg, meta) => null
}

let nodeid = null

const get_id = () => new Promise((resolve, reject) => 
  crypto.randomBytes(id_size, (err, buf) => err ? reject(err) : resolve(buf)))

const check_rpc_msg_format = msg => {
  if (!msg.uid) {
    console.error("Message has no UUID", msg)
    return false
  }

  if (!['res','ping','find_node','find_value','store'].includes(msg.action)) {
    console.error("Message action can't be found", msg)
    return false
  }

  return true
}

const rpc = (fd, ip, port, msg) => new Promise((resolve, reject) => {
  const timer = setTimeout(() => delete pending_requests[msg.uid], 2000)
  pending_requests[msg.uid] = { timeout: timer, resolve: resolve }
  fd.send(JSON.stringify(msg), port, ip, err => err ? reject(err) : null)
})

const start_network = port => new Promise((resolve, reject) => {
  const udpfd = dgram.createSocket('udp4')

  udpfd.on('error', err => {
    udpfd.close()
    reject(err)
  })

  udpfd.on('message', (msg, meta) => {
    try {
      const rpc_msg = JSON.parse(msg)
      if (!check_rpc_msg_format(rpc_msg)) return
      handle_rpc[rpc_msg.action](udpfd, rpc_msg, meta)
    } catch (e) {
      console.error('Unable to parse message', e)
    }
  })

  udpfd.on('listening', () => resolve(udpfd))
  udpfd.bind(port)
})

get_id()
  .then(buf => {
    nodeid = buf
    console.log(`node id is ${buf.toString('hex')}`)
    return start_network(process.env['KAD_PORT'])
  })
  .then(udpfd => {
    addr = udpfd.address()
    console.log(`node listening on ${addr.address}:${addr.port}`)
    return rpc(udpfd, '127.0.0.1', addr.port, {uid: 'a', action: 'ping'})
  })
  .then(([fd, msg, meta]) => {
    console.log('Ping success')
  })
  .catch(e => console.error('A critical error occured in the promise chain', e))
