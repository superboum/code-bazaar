const dgram = require('dgram')
const crypto = require('crypto')
const udpfd = dgram.createSocket('udp4')

const id_size = 32
const kbuckets = Array.from({length: id_size}, () => [])
const xorbuf = (b1, b2) => b1.map((v,i) => v ^ b2[i])

let nodeid = null
crypto.randomBytes(id_size, (err, buf) => {
  if (err) {
    console.error(err)
    return
  }
  nodeid = buf
  console.log(`node id is ${buf.toString('hex')}`)
})

udpfd.on('error', err => {
  console.error(err)
  udpfd.close()
})

udpfd.on('message', (msg, meta) => console.log(msg, meta))

udpfd.on('listening', () => { 
  addr = udpfd.address()
  console.log(`node listening on ${addr.address}:${addr.port}`)
})

const rpc_send = (ip, port, msg) =>
  udpfd.send(msg, port, ip, err => console.error(err))

udpfd.bind(process.env['KAD_PORT'])
