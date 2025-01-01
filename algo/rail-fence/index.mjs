import fs from 'fs';
// config
const sq=8;

// init
const sz=sq*sq;
const enc = new TextEncoder();
const dec = new TextDecoder('utf-8');

// harcoded data
const trans = new Uint8Array([
   0,  2,  3,  9, 10, 20, 21, 35,
   1,  4,  8, 11, 19, 22, 34, 36,
   5,  7, 12, 18, 23, 33, 37, 48,
   6, 13, 17, 24, 32, 38, 47, 49,
  14, 16, 25, 31, 39, 46, 50, 57,
  15, 26, 30, 40, 45, 51, 56, 58,
  27, 29, 41, 44, 52, 55, 59, 62,
  28, 42, 43, 53, 54, 60, 61, 63
]);

// fn
const str_to_buf = input => {
  const buffer=new Uint8Array(sz);
  let buf_idx=0;
  const lower = input.toLowerCase().replace(/[^a-z_ ]/gi, '');
  const charcodes = enc.encode(lower); 
  while (buf_idx < buffer.length) {
    buffer[buf_idx] = charcodes[buf_idx % charcodes.length];
    buf_idx += 1;
  }
  return buffer
}

const rotate_cc = inp_buf => {
  const out_buf=new Uint8Array(inp_buf.length);
  for (let i = 0; i < sq; i++) {
    for (let j = 0; j < sq; j++) {
      out_buf[(sq-j-1)*sq+i] = inp_buf[i*sq+j];
    }
  }
  return out_buf
}

const zig_zag = inp_buf => {
  const out_buf=new Uint8Array(inp_buf.length);
  trans.forEach((v, idx) => out_buf[idx] = inp_buf[v])
  return out_buf
}


const ret1 = str_to_buf(fs.readFileSync(0, 'utf-8'));
const ret2 = zig_zag(ret1);
const ret3 = rotate_cc(ret2);
const ret4 = dec.decode(ret3);
for (let i = 0; i < sq; i++) {
  console.log(ret4.slice(i*sq, (i+1)*sq))
}
  


/*
const inp_idx = buffer.forEach((_, idx) => lower.charCodeAt(idx))

console.log(inp_idx, buffer)*/
