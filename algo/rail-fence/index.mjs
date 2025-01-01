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

const dr = inp_str => {
  const canvas = document.getElementById("render");
  if (!canvas) return;
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  const alpha = document.getElementById("alphabet");
  if (!alpha) return;

  ctx.fillStyle = "#fff";
  ctx.fillRect(0,0, sq*64, sq*64);

  ctx.textBaseline = "middle";
  ctx.textAlign = "center";
  ctx.fillStyle = "#000";
  ctx.font = "48px serif";
  for (let i = 0; i < sq; i++) {
    for (let j = 0; j < sq; j++) {
      //ctx.fillText(inp_str[j*sq+i], 32+i*64, 32+j*64);
      const scode_n = inp_str.charCodeAt(j*sq+i) - 97;
      const scode = scode_n < 0 ? 27 : scode_n;
      const scode_x = (scode % 6) * 64;
      const scode_y = Math.floor(scode / 6) * 64;
      const sq_pos_x = i*64;
      const sq_pos_y = j*64;
      ctx.drawImage(alpha, scode_x, scode_y, 64, 64, sq_pos_x, sq_pos_y, 64, 64);
    }
  }
}

const gen = () => {
  const txt = document.getElementById('input').value || "lorem_ipsum_dolor_sit_amet";
  console.debug(txt, txt.length);
  const ret1 = str_to_buf(txt);
  const ret2 = zig_zag(ret1);
  const ret3 = rotate_cc(ret2);
  const ret4 = dec.decode(ret3);
  dr(ret4);
}
gen();
document.getElementById('gen').addEventListener('click', gen);
  
