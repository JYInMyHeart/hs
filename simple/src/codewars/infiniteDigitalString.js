function numIndex(n) {
  if (n < 10) return n - 1;
  var c = 0;
  for (let i = 1; ; i++) {
    c += i * 9 * Math.pow(10, i - 1);
    if (n < Math.pow(10, i + 1)) return c + (i + 1) * (n - Math.pow(10, i));
  }
}

function findPosition(str) {
  if (/^0+$/.test(str)) return numIndex(+(1 + str)) + 1;
  for (let l = 1; l <= str.length; l++) {
    let poss = [];
    for (let i = 0; i < l; i++) {
      let sdt = str.slice(0, l - i), end = str.slice(l - i, l);
      for (let c of (+end ? [end + sdt, (end - 1) + sdt] : [end + sdt])) {
        if (c[0] === '0') continue;
        let ds = c, n = +c;
        while (ds.length < str.length + l) ds += (++n);
        if (ds.indexOf(str) !== -1) poss.push(numIndex(+c) + ds.indexOf(str));
      }
    }
    if (poss.length) return Math.min(...poss);
  }
}