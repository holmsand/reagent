if(typeof Math.imul == "undefined" || (Math.imul(0xffffffff,5) == 0)) {
    Math.imul = function (a, b) {
        var ah  = (a >>> 16) & 0xffff;
        var al = a & 0xffff;
        var bh  = (b >>> 16) & 0xffff;
        var bl = b & 0xffff;
        // the shift by 0 fixes the sign on the high part
        // the final |0 converts the unsigned value into a signed value
        return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
    }
}

;(function(){
var g, aa = this;
function k(a) {
  var b = typeof a;
  if ("object" == b) {
    if (a) {
      if (a instanceof Array) {
        return "array";
      }
      if (a instanceof Object) {
        return b;
      }
      var c = Object.prototype.toString.call(a);
      if ("[object Window]" == c) {
        return "object";
      }
      if ("[object Array]" == c || "number" == typeof a.length && "undefined" != typeof a.splice && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("splice")) {
        return "array";
      }
      if ("[object Function]" == c || "undefined" != typeof a.call && "undefined" != typeof a.propertyIsEnumerable && !a.propertyIsEnumerable("call")) {
        return "function";
      }
    } else {
      return "null";
    }
  } else {
    if ("function" == b && "undefined" == typeof a.call) {
      return "object";
    }
  }
  return b;
}
function ba(a) {
  return "string" == typeof a;
}
function ca(a) {
  return "function" == k(a);
}
function ea(a) {
  return a[fa] || (a[fa] = ++ga);
}
var fa = "closure_uid_" + (1E9 * Math.random() >>> 0), ga = 0;
function ha(a, b, c) {
  return a.call.apply(a.bind, arguments);
}
function ia(a, b, c) {
  if (!a) {
    throw Error();
  }
  if (2 < arguments.length) {
    var d = Array.prototype.slice.call(arguments, 2);
    return function() {
      var c = Array.prototype.slice.call(arguments);
      Array.prototype.unshift.apply(c, d);
      return a.apply(b, c);
    };
  }
  return function() {
    return a.apply(b, arguments);
  };
}
function ja(a, b, c) {
  ja = Function.prototype.bind && -1 != Function.prototype.bind.toString().indexOf("native code") ? ha : ia;
  return ja.apply(null, arguments);
}
function ka(a, b) {
  var c = Array.prototype.slice.call(arguments, 1);
  return function() {
    var b = c.slice();
    b.push.apply(b, arguments);
    return a.apply(this, b);
  };
}
var la = Date.now || function() {
  return+new Date;
};
function ma(a, b) {
  var c = a.split("."), d = aa;
  c[0] in d || !d.execScript || d.execScript("var " + c[0]);
  for (var e;c.length && (e = c.shift());) {
    c.length || void 0 === b ? d = d[e] ? d[e] : d[e] = {} : d[e] = b;
  }
}
function na(a, b) {
  function c() {
  }
  c.prototype = b.prototype;
  a.Jb = b.prototype;
  a.prototype = new c;
  a.prototype.constructor = a;
  a.Sd = function(a, c, f) {
    return b.prototype[c].apply(a, Array.prototype.slice.call(arguments, 2));
  };
}
;function oa(a, b) {
  for (var c = a.split("%s"), d = "", e = Array.prototype.slice.call(arguments, 1);e.length && 1 < c.length;) {
    d += c.shift() + e.shift();
  }
  return d + c.join("%s");
}
function pa(a) {
  return a.replace(/^[\s\xa0]+|[\s\xa0]+$/g, "");
}
function qa(a) {
  if (!ra.test(a)) {
    return a;
  }
  -1 != a.indexOf("\x26") && (a = a.replace(sa, "\x26amp;"));
  -1 != a.indexOf("\x3c") && (a = a.replace(ua, "\x26lt;"));
  -1 != a.indexOf("\x3e") && (a = a.replace(va, "\x26gt;"));
  -1 != a.indexOf('"') && (a = a.replace(xa, "\x26quot;"));
  -1 != a.indexOf("'") && (a = a.replace(ya, "\x26#39;"));
  -1 != a.indexOf("\x00") && (a = a.replace(za, "\x26#0;"));
  return a;
}
var sa = /&/g, ua = /</g, va = />/g, xa = /"/g, ya = /'/g, za = /\x00/g, ra = /[\x00&<>"']/;
function Ba(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;
}
;function Ca(a, b) {
  for (var c in a) {
    b.call(void 0, a[c], c, a);
  }
}
var Da = "constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf".split(" ");
function Ea(a, b) {
  for (var c, d, e = 1;e < arguments.length;e++) {
    d = arguments[e];
    for (c in d) {
      a[c] = d[c];
    }
    for (var f = 0;f < Da.length;f++) {
      c = Da[f], Object.prototype.hasOwnProperty.call(d, c) && (a[c] = d[c]);
    }
  }
}
;function Fa(a, b) {
  null != a && this.append.apply(this, arguments);
}
Fa.prototype.rb = "";
Fa.prototype.append = function(a, b, c) {
  this.rb += a;
  if (null != b) {
    for (var d = 1;d < arguments.length;d++) {
      this.rb += arguments[d];
    }
  }
  return this;
};
Fa.prototype.toString = function() {
  return this.rb;
};
function Ha(a) {
  if (Error.captureStackTrace) {
    Error.captureStackTrace(this, Ha);
  } else {
    var b = Error().stack;
    b && (this.stack = b);
  }
  a && (this.message = String(a));
}
na(Ha, Error);
Ha.prototype.name = "CustomError";
var Ia = Array.prototype, Ja = Ia.indexOf ? function(a, b, c) {
  return Ia.indexOf.call(a, b, c);
} : function(a, b, c) {
  c = null == c ? 0 : 0 > c ? Math.max(0, a.length + c) : c;
  if (ba(a)) {
    return ba(b) && 1 == b.length ? a.indexOf(b, c) : -1;
  }
  for (;c < a.length;c++) {
    if (c in a && a[c] === b) {
      return c;
    }
  }
  return-1;
}, La = Ia.some ? function(a, b, c) {
  return Ia.some.call(a, b, c);
} : function(a, b, c) {
  for (var d = a.length, e = ba(a) ? a.split("") : a, f = 0;f < d;f++) {
    if (f in e && b.call(c, e[f], f, a)) {
      return!0;
    }
  }
  return!1;
};
var Ma = null;
function Na() {
  return new l(null, 5, [Pa, !0, Qa, !0, Ra, !1, Sa, !1, Ta, null], null);
}
function q(a) {
  return null != a && !1 !== a;
}
function Ua(a) {
  return q(a) ? !1 : !0;
}
function r(a, b) {
  return a[k(null == b ? null : b)] ? !0 : a._ ? !0 : !1;
}
function Va(a) {
  return null == a ? null : a.constructor;
}
function u(a, b) {
  var c = Va(b), c = q(q(c) ? c.Ed : c) ? c.Dd : k(b);
  return Error(["No protocol method ", a, " defined for type ", c, ": ", b].join(""));
}
function Ya(a) {
  var b = a.Dd;
  return q(b) ? b : "" + v.b(a);
}
function Za(a) {
  for (var b = a.length, c = Array(b), d = 0;;) {
    if (d < b) {
      c[d] = a[d], d += 1;
    } else {
      break;
    }
  }
  return c;
}
function ab(a) {
  return Array.prototype.slice.call(arguments);
}
var cb = function() {
  function a(a, b) {
    function c(a, b) {
      a.push(b);
      return a;
    }
    var h = [];
    return bb.c ? bb.c(c, h, b) : bb.call(null, c, h, b);
  }
  function b(a) {
    return c.a(null, a);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, 0, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), db = {}, eb = {}, hb = {};
function ib(a) {
  if (a ? a.L : a) {
    return a.L(a);
  }
  var b;
  b = ib[k(null == a ? null : a)];
  if (!b && (b = ib._, !b)) {
    throw u("ICounted.-count", a);
  }
  return b.call(null, a);
}
function jb(a) {
  if (a ? a.J : a) {
    return a.J(a);
  }
  var b;
  b = jb[k(null == a ? null : a)];
  if (!b && (b = jb._, !b)) {
    throw u("IEmptyableCollection.-empty", a);
  }
  return b.call(null, a);
}
var kb = {};
function lb(a, b) {
  if (a ? a.I : a) {
    return a.I(a, b);
  }
  var c;
  c = lb[k(null == a ? null : a)];
  if (!c && (c = lb._, !c)) {
    throw u("ICollection.-conj", a);
  }
  return c.call(null, a, b);
}
var mb = {}, x = function() {
  function a(a, b, c) {
    if (a ? a.Z : a) {
      return a.Z(a, b, c);
    }
    var h;
    h = x[k(null == a ? null : a)];
    if (!h && (h = x._, !h)) {
      throw u("IIndexed.-nth", a);
    }
    return h.call(null, a, b, c);
  }
  function b(a, b) {
    if (a ? a.C : a) {
      return a.C(a, b);
    }
    var c;
    c = x[k(null == a ? null : a)];
    if (!c && (c = x._, !c)) {
      throw u("IIndexed.-nth", a);
    }
    return c.call(null, a, b);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), nb = {};
function pb(a) {
  if (a ? a.T : a) {
    return a.T(a);
  }
  var b;
  b = pb[k(null == a ? null : a)];
  if (!b && (b = pb._, !b)) {
    throw u("ISeq.-first", a);
  }
  return b.call(null, a);
}
function qb(a) {
  if (a ? a.$ : a) {
    return a.$(a);
  }
  var b;
  b = qb[k(null == a ? null : a)];
  if (!b && (b = qb._, !b)) {
    throw u("ISeq.-rest", a);
  }
  return b.call(null, a);
}
var rb = {}, sb = {}, tb = function() {
  function a(a, b, c) {
    if (a ? a.s : a) {
      return a.s(a, b, c);
    }
    var h;
    h = tb[k(null == a ? null : a)];
    if (!h && (h = tb._, !h)) {
      throw u("ILookup.-lookup", a);
    }
    return h.call(null, a, b, c);
  }
  function b(a, b) {
    if (a ? a.r : a) {
      return a.r(a, b);
    }
    var c;
    c = tb[k(null == a ? null : a)];
    if (!c && (c = tb._, !c)) {
      throw u("ILookup.-lookup", a);
    }
    return c.call(null, a, b);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function ub(a, b) {
  if (a ? a.Rb : a) {
    return a.Rb(a, b);
  }
  var c;
  c = ub[k(null == a ? null : a)];
  if (!c && (c = ub._, !c)) {
    throw u("IAssociative.-contains-key?", a);
  }
  return c.call(null, a, b);
}
function vb(a, b, c) {
  if (a ? a.La : a) {
    return a.La(a, b, c);
  }
  var d;
  d = vb[k(null == a ? null : a)];
  if (!d && (d = vb._, !d)) {
    throw u("IAssociative.-assoc", a);
  }
  return d.call(null, a, b, c);
}
var wb = {};
function xb(a, b) {
  if (a ? a.Vb : a) {
    return a.Vb(a, b);
  }
  var c;
  c = xb[k(null == a ? null : a)];
  if (!c && (c = xb._, !c)) {
    throw u("IMap.-dissoc", a);
  }
  return c.call(null, a, b);
}
var yb = {};
function zb(a) {
  if (a ? a.ub : a) {
    return a.ub(a);
  }
  var b;
  b = zb[k(null == a ? null : a)];
  if (!b && (b = zb._, !b)) {
    throw u("IMapEntry.-key", a);
  }
  return b.call(null, a);
}
function Bb(a) {
  if (a ? a.vb : a) {
    return a.vb(a);
  }
  var b;
  b = Bb[k(null == a ? null : a)];
  if (!b && (b = Bb._, !b)) {
    throw u("IMapEntry.-val", a);
  }
  return b.call(null, a);
}
var Cb = {};
function Db(a) {
  if (a ? a.Sa : a) {
    return a.Sa(a);
  }
  var b;
  b = Db[k(null == a ? null : a)];
  if (!b && (b = Db._, !b)) {
    throw u("IStack.-peek", a);
  }
  return b.call(null, a);
}
function Eb(a) {
  if (a ? a.Ta : a) {
    return a.Ta(a);
  }
  var b;
  b = Eb[k(null == a ? null : a)];
  if (!b && (b = Eb._, !b)) {
    throw u("IStack.-pop", a);
  }
  return b.call(null, a);
}
var Fb = {};
function Gb(a, b, c) {
  if (a ? a.Ua : a) {
    return a.Ua(a, b, c);
  }
  var d;
  d = Gb[k(null == a ? null : a)];
  if (!d && (d = Gb._, !d)) {
    throw u("IVector.-assoc-n", a);
  }
  return d.call(null, a, b, c);
}
function Hb(a) {
  if (a ? a.sb : a) {
    return a.sb(a);
  }
  var b;
  b = Hb[k(null == a ? null : a)];
  if (!b && (b = Hb._, !b)) {
    throw u("IDeref.-deref", a);
  }
  return b.call(null, a);
}
var Jb = {};
function Kb(a) {
  if (a ? a.D : a) {
    return a.D(a);
  }
  var b;
  b = Kb[k(null == a ? null : a)];
  if (!b && (b = Kb._, !b)) {
    throw u("IMeta.-meta", a);
  }
  return b.call(null, a);
}
var Lb = {};
function Mb(a, b) {
  if (a ? a.H : a) {
    return a.H(a, b);
  }
  var c;
  c = Mb[k(null == a ? null : a)];
  if (!c && (c = Mb._, !c)) {
    throw u("IWithMeta.-with-meta", a);
  }
  return c.call(null, a, b);
}
var Nb = {}, Ob = function() {
  function a(a, b, c) {
    if (a ? a.P : a) {
      return a.P(a, b, c);
    }
    var h;
    h = Ob[k(null == a ? null : a)];
    if (!h && (h = Ob._, !h)) {
      throw u("IReduce.-reduce", a);
    }
    return h.call(null, a, b, c);
  }
  function b(a, b) {
    if (a ? a.O : a) {
      return a.O(a, b);
    }
    var c;
    c = Ob[k(null == a ? null : a)];
    if (!c && (c = Ob._, !c)) {
      throw u("IReduce.-reduce", a);
    }
    return c.call(null, a, b);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function Pb(a, b, c) {
  if (a ? a.tb : a) {
    return a.tb(a, b, c);
  }
  var d;
  d = Pb[k(null == a ? null : a)];
  if (!d && (d = Pb._, !d)) {
    throw u("IKVReduce.-kv-reduce", a);
  }
  return d.call(null, a, b, c);
}
function Qb(a, b) {
  if (a ? a.u : a) {
    return a.u(a, b);
  }
  var c;
  c = Qb[k(null == a ? null : a)];
  if (!c && (c = Qb._, !c)) {
    throw u("IEquiv.-equiv", a);
  }
  return c.call(null, a, b);
}
function Rb(a) {
  if (a ? a.w : a) {
    return a.w(a);
  }
  var b;
  b = Rb[k(null == a ? null : a)];
  if (!b && (b = Rb._, !b)) {
    throw u("IHash.-hash", a);
  }
  return b.call(null, a);
}
var Sb = {};
function Ub(a) {
  if (a ? a.G : a) {
    return a.G(a);
  }
  var b;
  b = Ub[k(null == a ? null : a)];
  if (!b && (b = Ub._, !b)) {
    throw u("ISeqable.-seq", a);
  }
  return b.call(null, a);
}
var Vb = {};
function Wb(a, b) {
  if (a ? a.Yc : a) {
    return a.Yc(0, b);
  }
  var c;
  c = Wb[k(null == a ? null : a)];
  if (!c && (c = Wb._, !c)) {
    throw u("IWriter.-write", a);
  }
  return c.call(null, a, b);
}
var Xb = {};
function Yb(a, b, c) {
  if (a ? a.v : a) {
    return a.v(a, b, c);
  }
  var d;
  d = Yb[k(null == a ? null : a)];
  if (!d && (d = Yb._, !d)) {
    throw u("IPrintWithWriter.-pr-writer", a);
  }
  return d.call(null, a, b, c);
}
function Zb(a, b, c) {
  if (a ? a.Yb : a) {
    return a.Yb(a, b, c);
  }
  var d;
  d = Zb[k(null == a ? null : a)];
  if (!d && (d = Zb._, !d)) {
    throw u("IWatchable.-notify-watches", a);
  }
  return d.call(null, a, b, c);
}
function $b(a, b, c) {
  if (a ? a.Xb : a) {
    return a.Xb(a, b, c);
  }
  var d;
  d = $b[k(null == a ? null : a)];
  if (!d && (d = $b._, !d)) {
    throw u("IWatchable.-add-watch", a);
  }
  return d.call(null, a, b, c);
}
function ac(a, b) {
  if (a ? a.Zb : a) {
    return a.Zb(a, b);
  }
  var c;
  c = ac[k(null == a ? null : a)];
  if (!c && (c = ac._, !c)) {
    throw u("IWatchable.-remove-watch", a);
  }
  return c.call(null, a, b);
}
function bc(a) {
  if (a ? a.cb : a) {
    return a.cb(a);
  }
  var b;
  b = bc[k(null == a ? null : a)];
  if (!b && (b = bc._, !b)) {
    throw u("IEditableCollection.-as-transient", a);
  }
  return b.call(null, a);
}
function cc(a, b) {
  if (a ? a.yb : a) {
    return a.yb(a, b);
  }
  var c;
  c = cc[k(null == a ? null : a)];
  if (!c && (c = cc._, !c)) {
    throw u("ITransientCollection.-conj!", a);
  }
  return c.call(null, a, b);
}
function dc(a) {
  if (a ? a.zb : a) {
    return a.zb(a);
  }
  var b;
  b = dc[k(null == a ? null : a)];
  if (!b && (b = dc._, !b)) {
    throw u("ITransientCollection.-persistent!", a);
  }
  return b.call(null, a);
}
function ec(a, b, c) {
  if (a ? a.xb : a) {
    return a.xb(a, b, c);
  }
  var d;
  d = ec[k(null == a ? null : a)];
  if (!d && (d = ec._, !d)) {
    throw u("ITransientAssociative.-assoc!", a);
  }
  return d.call(null, a, b, c);
}
function fc(a, b, c) {
  if (a ? a.Xc : a) {
    return a.Xc(0, b, c);
  }
  var d;
  d = fc[k(null == a ? null : a)];
  if (!d && (d = fc._, !d)) {
    throw u("ITransientVector.-assoc-n!", a);
  }
  return d.call(null, a, b, c);
}
function gc(a) {
  if (a ? a.Uc : a) {
    return a.Uc();
  }
  var b;
  b = gc[k(null == a ? null : a)];
  if (!b && (b = gc._, !b)) {
    throw u("IChunk.-drop-first", a);
  }
  return b.call(null, a);
}
function hc(a) {
  if (a ? a.wc : a) {
    return a.wc(a);
  }
  var b;
  b = hc[k(null == a ? null : a)];
  if (!b && (b = hc._, !b)) {
    throw u("IChunkedSeq.-chunked-first", a);
  }
  return b.call(null, a);
}
function ic(a) {
  if (a ? a.xc : a) {
    return a.xc(a);
  }
  var b;
  b = ic[k(null == a ? null : a)];
  if (!b && (b = ic._, !b)) {
    throw u("IChunkedSeq.-chunked-rest", a);
  }
  return b.call(null, a);
}
function jc(a) {
  if (a ? a.vc : a) {
    return a.vc(a);
  }
  var b;
  b = jc[k(null == a ? null : a)];
  if (!b && (b = jc._, !b)) {
    throw u("IChunkedNext.-chunked-next", a);
  }
  return b.call(null, a);
}
function kc(a, b) {
  if (a ? a.yc : a) {
    return a.yc(a, b);
  }
  var c;
  c = kc[k(null == a ? null : a)];
  if (!c && (c = kc._, !c)) {
    throw u("IReset.-reset!", a);
  }
  return c.call(null, a, b);
}
var mc = function() {
  function a(a, b, c, d, e) {
    if (a ? a.Cc : a) {
      return a.Cc(a, b, c, d, e);
    }
    var s;
    s = mc[k(null == a ? null : a)];
    if (!s && (s = mc._, !s)) {
      throw u("ISwap.-swap!", a);
    }
    return s.call(null, a, b, c, d, e);
  }
  function b(a, b, c, d) {
    if (a ? a.Bc : a) {
      return a.Bc(a, b, c, d);
    }
    var e;
    e = mc[k(null == a ? null : a)];
    if (!e && (e = mc._, !e)) {
      throw u("ISwap.-swap!", a);
    }
    return e.call(null, a, b, c, d);
  }
  function c(a, b, c) {
    if (a ? a.Ac : a) {
      return a.Ac(a, b, c);
    }
    var d;
    d = mc[k(null == a ? null : a)];
    if (!d && (d = mc._, !d)) {
      throw u("ISwap.-swap!", a);
    }
    return d.call(null, a, b, c);
  }
  function d(a, b) {
    if (a ? a.zc : a) {
      return a.zc(a, b);
    }
    var c;
    c = mc[k(null == a ? null : a)];
    if (!c && (c = mc._, !c)) {
      throw u("ISwap.-swap!", a);
    }
    return c.call(null, a, b);
  }
  var e = null, e = function(e, h, m, n, p) {
    switch(arguments.length) {
      case 2:
        return d.call(this, e, h);
      case 3:
        return c.call(this, e, h, m);
      case 4:
        return b.call(this, e, h, m, n);
      case 5:
        return a.call(this, e, h, m, n, p);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  e.a = d;
  e.c = c;
  e.j = b;
  e.t = a;
  return e;
}();
function nc(a) {
  this.Od = a;
  this.p = 0;
  this.i = 1073741824;
}
nc.prototype.Yc = function(a, b) {
  return this.Od.append(b);
};
function oc(a) {
  var b = new Fa;
  a.v(null, new nc(b), Na());
  return "" + v.b(b);
}
var pc = "undefined" !== typeof Math.imul && 0 !== (Math.imul.a ? Math.imul.a(4294967295, 5) : Math.imul.call(null, 4294967295, 5)) ? function(a, b) {
  return Math.imul.a ? Math.imul.a(a, b) : Math.imul.call(null, a, b);
} : function(a, b) {
  var c = a & 65535, d = b & 65535;
  return c * d + ((a >>> 16 & 65535) * d + c * (b >>> 16 & 65535) << 16 >>> 0) | 0;
};
function qc(a) {
  a = pc(a, 3432918353);
  return pc(a << 15 | a >>> -15, 461845907);
}
function rc(a, b) {
  var c = a ^ b;
  return pc(c << 13 | c >>> -13, 5) + 3864292196;
}
function sc(a, b) {
  var c = a ^ b, c = pc(c ^ c >>> 16, 2246822507), c = pc(c ^ c >>> 13, 3266489909);
  return c ^ c >>> 16;
}
function tc(a) {
  var b;
  a: {
    b = 1;
    for (var c = 0;;) {
      if (b < a.length) {
        var d = b + 2, c = rc(c, qc(a.charCodeAt(b - 1) | a.charCodeAt(b) << 16));
        b = d;
      } else {
        b = c;
        break a;
      }
    }
    b = void 0;
  }
  b = 1 === (a.length & 1) ? b ^ qc(a.charCodeAt(a.length - 1)) : b;
  return sc(b, pc(2, a.length));
}
var uc = {}, vc = 0;
function wc(a) {
  255 < vc && (uc = {}, vc = 0);
  var b = uc[a];
  if ("number" !== typeof b) {
    a: {
      if (null != a) {
        if (b = a.length, 0 < b) {
          for (var c = 0, d = 0;;) {
            if (c < b) {
              var e = c + 1, d = pc(31, d) + a.charCodeAt(c), c = e
            } else {
              b = d;
              break a;
            }
          }
          b = void 0;
        } else {
          b = 0;
        }
      } else {
        b = 0;
      }
    }
    uc[a] = b;
    vc += 1;
  }
  return a = b;
}
function xc(a) {
  a && (a.i & 4194304 || a.Xd) ? a = a.w(null) : "number" === typeof a ? a = (Math.floor.b ? Math.floor.b(a) : Math.floor.call(null, a)) % 2147483647 : !0 === a ? a = 1 : !1 === a ? a = 0 : "string" === typeof a ? (a = wc(a), 0 !== a && (a = qc(a), a = rc(0, a), a = sc(a, 4))) : a = null == a ? 0 : Rb(a);
  return a;
}
function yc(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2);
}
function zc(a, b) {
  if (q(Ac.a ? Ac.a(a, b) : Ac.call(null, a, b))) {
    return 0;
  }
  if (q(function() {
    var c = Ua(a.ma);
    return c ? b.ma : c;
  }())) {
    return-1;
  }
  if (q(a.ma)) {
    if (Ua(b.ma)) {
      return 1;
    }
    var c = function() {
      var c = a.ma, d = b.ma;
      return Bc.a ? Bc.a(c, d) : Bc.call(null, c, d);
    }();
    if (0 === c) {
      var c = a.name, d = b.name;
      return Bc.a ? Bc.a(c, d) : Bc.call(null, c, d);
    }
    return c;
  }
  c = a.name;
  d = b.name;
  return Bc.a ? Bc.a(c, d) : Bc.call(null, c, d);
}
function Cc(a, b, c, d, e) {
  this.ma = a;
  this.name = b;
  this.Qa = c;
  this.bb = d;
  this.Y = e;
  this.i = 2154168321;
  this.p = 4096;
}
g = Cc.prototype;
g.v = function(a, b) {
  return Wb(b, this.Qa);
};
g.w = function() {
  var a = this.bb;
  return null != a ? a : this.bb = a = yc(tc(this.name), wc(this.ma));
};
g.H = function(a, b) {
  return new Cc(this.ma, this.name, this.Qa, this.bb, b);
};
g.D = function() {
  return this.Y;
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return tb.c(c, this, null);
      case 3:
        return tb.c(c, this, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return tb.c(c, this, null);
  };
  a.c = function(a, c, d) {
    return tb.c(c, this, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return tb.c(a, this, null);
};
g.a = function(a, b) {
  return tb.c(a, this, b);
};
g.u = function(a, b) {
  return b instanceof Cc ? this.Qa === b.Qa : !1;
};
g.toString = function() {
  return this.Qa;
};
var Dc = function() {
  function a(a, b) {
    var c = null != a ? "" + v.b(a) + "/" + v.b(b) : b;
    return new Cc(a, b, c, null, null);
  }
  function b(a) {
    return a instanceof Cc ? a : c.a(null, a);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}();
function y(a) {
  if (null == a) {
    return null;
  }
  if (a && (a.i & 8388608 || a.Zd)) {
    return a.G(null);
  }
  if (a instanceof Array || "string" === typeof a) {
    return 0 === a.length ? null : new Ec(a, 0);
  }
  if (r(Sb, a)) {
    return Ub(a);
  }
  throw Error("" + v.b(a) + " is not ISeqable");
}
function A(a) {
  if (null == a) {
    return null;
  }
  if (a && (a.i & 64 || a.wb)) {
    return a.T(null);
  }
  a = y(a);
  return null == a ? null : pb(a);
}
function C(a) {
  return null != a ? a && (a.i & 64 || a.wb) ? a.$(null) : (a = y(a)) ? qb(a) : Fc : Fc;
}
function D(a) {
  return null == a ? null : a && (a.i & 128 || a.Wb) ? a.aa(null) : y(C(a));
}
var Ac = function() {
  function a(a, b) {
    return null == a ? null == b : a === b || Qb(a, b);
  }
  var b = null, c = function() {
    function a(b, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return c.call(this, b, d, n);
    }
    function c(a, d, e) {
      for (;;) {
        if (b.a(a, d)) {
          if (D(e)) {
            a = d, d = A(e), e = D(e);
          } else {
            return b.a(d, A(e));
          }
        } else {
          return!1;
        }
      }
    }
    a.m = 2;
    a.h = function(a) {
      var b = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return c(b, d, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 1:
        return!0;
      case 2:
        return a.call(this, b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.b = function() {
    return!0;
  };
  b.a = a;
  b.e = c.e;
  return b;
}();
function Gc(a, b) {
  var c = qc(a), c = rc(0, c);
  return sc(c, b);
}
function Hc(a) {
  var b = 0, c = 1;
  for (a = y(a);;) {
    if (null != a) {
      b += 1, c = pc(31, c) + xc(A(a)) | 0, a = D(a);
    } else {
      return Gc(c, b);
    }
  }
}
function Ic(a) {
  var b = 0, c = 0;
  for (a = y(a);;) {
    if (null != a) {
      b += 1, c = c + xc(A(a)) | 0, a = D(a);
    } else {
      return Gc(c, b);
    }
  }
}
hb["null"] = !0;
ib["null"] = function() {
  return 0;
};
Date.prototype.u = function(a, b) {
  return b instanceof Date && this.toString() === b.toString();
};
Qb.number = function(a, b) {
  return a === b;
};
Jb["function"] = !0;
Kb["function"] = function() {
  return null;
};
db["function"] = !0;
Rb._ = function(a) {
  return ea(a);
};
function Jc(a) {
  return a + 1;
}
function Kc(a) {
  this.l = a;
  this.p = 0;
  this.i = 32768;
}
Kc.prototype.sb = function() {
  return this.l;
};
function Lc(a) {
  return a instanceof Kc;
}
function G(a) {
  return Hb(a);
}
var Mc = function() {
  function a(a, b, c, d) {
    for (var n = ib(a);;) {
      if (d < n) {
        var p = x.a(a, d);
        c = b.a ? b.a(c, p) : b.call(null, c, p);
        if (Lc(c)) {
          return Hb(c);
        }
        d += 1;
      } else {
        return c;
      }
    }
  }
  function b(a, b, c) {
    var d = ib(a), n = c;
    for (c = 0;;) {
      if (c < d) {
        var p = x.a(a, c), n = b.a ? b.a(n, p) : b.call(null, n, p);
        if (Lc(n)) {
          return Hb(n);
        }
        c += 1;
      } else {
        return n;
      }
    }
  }
  function c(a, b) {
    var c = ib(a);
    if (0 === c) {
      return b.o ? b.o() : b.call(null);
    }
    for (var d = x.a(a, 0), n = 1;;) {
      if (n < c) {
        var p = x.a(a, n), d = b.a ? b.a(d, p) : b.call(null, d, p);
        if (Lc(d)) {
          return Hb(d);
        }
        n += 1;
      } else {
        return d;
      }
    }
  }
  var d = null, d = function(d, f, h, m) {
    switch(arguments.length) {
      case 2:
        return c.call(this, d, f);
      case 3:
        return b.call(this, d, f, h);
      case 4:
        return a.call(this, d, f, h, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.a = c;
  d.c = b;
  d.j = a;
  return d;
}(), Nc = function() {
  function a(a, b, c, d) {
    for (var n = a.length;;) {
      if (d < n) {
        var p = a[d];
        c = b.a ? b.a(c, p) : b.call(null, c, p);
        if (Lc(c)) {
          return Hb(c);
        }
        d += 1;
      } else {
        return c;
      }
    }
  }
  function b(a, b, c) {
    var d = a.length, n = c;
    for (c = 0;;) {
      if (c < d) {
        var p = a[c], n = b.a ? b.a(n, p) : b.call(null, n, p);
        if (Lc(n)) {
          return Hb(n);
        }
        c += 1;
      } else {
        return n;
      }
    }
  }
  function c(a, b) {
    var c = a.length;
    if (0 === a.length) {
      return b.o ? b.o() : b.call(null);
    }
    for (var d = a[0], n = 1;;) {
      if (n < c) {
        var p = a[n], d = b.a ? b.a(d, p) : b.call(null, d, p);
        if (Lc(d)) {
          return Hb(d);
        }
        n += 1;
      } else {
        return d;
      }
    }
  }
  var d = null, d = function(d, f, h, m) {
    switch(arguments.length) {
      case 2:
        return c.call(this, d, f);
      case 3:
        return b.call(this, d, f, h);
      case 4:
        return a.call(this, d, f, h, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.a = c;
  d.c = b;
  d.j = a;
  return d;
}();
function Pc(a) {
  return a ? a.i & 2 || a.td ? !0 : a.i ? !1 : r(hb, a) : r(hb, a);
}
function Qc(a) {
  return a ? a.i & 16 || a.Vc ? !0 : a.i ? !1 : r(mb, a) : r(mb, a);
}
function Ec(a, b) {
  this.d = a;
  this.q = b;
  this.i = 166199550;
  this.p = 8192;
}
g = Ec.prototype;
g.toString = function() {
  return oc(this);
};
g.C = function(a, b) {
  var c = b + this.q;
  return c < this.d.length ? this.d[c] : null;
};
g.Z = function(a, b, c) {
  a = b + this.q;
  return a < this.d.length ? this.d[a] : c;
};
g.aa = function() {
  return this.q + 1 < this.d.length ? new Ec(this.d, this.q + 1) : null;
};
g.L = function() {
  return this.d.length - this.q;
};
g.w = function() {
  return Hc(this);
};
g.u = function(a, b) {
  return Rc.a ? Rc.a(this, b) : Rc.call(null, this, b);
};
g.J = function() {
  return Fc;
};
g.O = function(a, b) {
  return Nc.j(this.d, b, this.d[this.q], this.q + 1);
};
g.P = function(a, b, c) {
  return Nc.j(this.d, b, c, this.q);
};
g.T = function() {
  return this.d[this.q];
};
g.$ = function() {
  return this.q + 1 < this.d.length ? new Ec(this.d, this.q + 1) : Fc;
};
g.G = function() {
  return this;
};
g.I = function(a, b) {
  return I.a ? I.a(b, this) : I.call(null, b, this);
};
var Sc = function() {
  function a(a, b) {
    return b < a.length ? new Ec(a, b) : null;
  }
  function b(a) {
    return c.a(a, 0);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), E = function() {
  function a(a, b) {
    return Sc.a(a, b);
  }
  function b(a) {
    return Sc.a(a, 0);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}();
function Tc(a) {
  return A(D(a));
}
Qb._ = function(a, b) {
  return a === b;
};
var Vc = function() {
  function a(a, b) {
    return null != a ? lb(a, b) : lb(Fc, b);
  }
  var b = null, c = function() {
    function a(b, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return c.call(this, b, d, n);
    }
    function c(a, d, e) {
      for (;;) {
        if (q(e)) {
          a = b.a(a, d), d = A(e), e = D(e);
        } else {
          return b.a(a, d);
        }
      }
    }
    a.m = 2;
    a.h = function(a) {
      var b = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return c(b, d, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 0:
        return Uc;
      case 1:
        return b;
      case 2:
        return a.call(this, b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.o = function() {
    return Uc;
  };
  b.b = function(a) {
    return a;
  };
  b.a = a;
  b.e = c.e;
  return b;
}();
function J(a) {
  if (null != a) {
    if (a && (a.i & 2 || a.td)) {
      a = a.L(null);
    } else {
      if (a instanceof Array) {
        a = a.length;
      } else {
        if ("string" === typeof a) {
          a = a.length;
        } else {
          if (r(hb, a)) {
            a = ib(a);
          } else {
            a: {
              a = y(a);
              for (var b = 0;;) {
                if (Pc(a)) {
                  a = b + ib(a);
                  break a;
                }
                a = D(a);
                b += 1;
              }
              a = void 0;
            }
          }
        }
      }
    }
  } else {
    a = 0;
  }
  return a;
}
var Wc = function() {
  function a(a, b, c) {
    for (;;) {
      if (null == a) {
        return c;
      }
      if (0 === b) {
        return y(a) ? A(a) : c;
      }
      if (Qc(a)) {
        return x.c(a, b, c);
      }
      if (y(a)) {
        a = D(a), b -= 1;
      } else {
        return c;
      }
    }
  }
  function b(a, b) {
    for (;;) {
      if (null == a) {
        throw Error("Index out of bounds");
      }
      if (0 === b) {
        if (y(a)) {
          return A(a);
        }
        throw Error("Index out of bounds");
      }
      if (Qc(a)) {
        return x.a(a, b);
      }
      if (y(a)) {
        var c = D(a), h = b - 1;
        a = c;
        b = h;
      } else {
        throw Error("Index out of bounds");
      }
    }
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), L = function() {
  function a(a, b, c) {
    if ("number" !== typeof b) {
      throw Error("index argument to nth must be a number.");
    }
    if (null == a) {
      return c;
    }
    if (a && (a.i & 16 || a.Vc)) {
      return a.Z(null, b, c);
    }
    if (a instanceof Array || "string" === typeof a) {
      return b < a.length ? a[b] : c;
    }
    if (r(mb, a)) {
      return x.a(a, b);
    }
    if (a ? a.i & 64 || a.wb || (a.i ? 0 : r(nb, a)) : r(nb, a)) {
      return Wc.c(a, b, c);
    }
    throw Error("nth not supported on this type " + v.b(Ya(Va(a))));
  }
  function b(a, b) {
    if ("number" !== typeof b) {
      throw Error("index argument to nth must be a number");
    }
    if (null == a) {
      return a;
    }
    if (a && (a.i & 16 || a.Vc)) {
      return a.C(null, b);
    }
    if (a instanceof Array || "string" === typeof a) {
      return b < a.length ? a[b] : null;
    }
    if (r(mb, a)) {
      return x.a(a, b);
    }
    if (a ? a.i & 64 || a.wb || (a.i ? 0 : r(nb, a)) : r(nb, a)) {
      return Wc.a(a, b);
    }
    throw Error("nth not supported on this type " + v.b(Ya(Va(a))));
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), M = function() {
  function a(a, b, c) {
    return null != a ? a && (a.i & 256 || a.Wc) ? a.s(null, b, c) : a instanceof Array ? b < a.length ? a[b] : c : "string" === typeof a ? b < a.length ? a[b] : c : r(sb, a) ? tb.c(a, b, c) : c : c;
  }
  function b(a, b) {
    return null == a ? null : a && (a.i & 256 || a.Wc) ? a.r(null, b) : a instanceof Array ? b < a.length ? a[b] : null : "string" === typeof a ? b < a.length ? a[b] : null : r(sb, a) ? tb.a(a, b) : null;
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), N = function() {
  function a(a, b, c) {
    return null != a ? vb(a, b, c) : Xc([b], [c]);
  }
  var b = null, c = function() {
    function a(b, d, m, n) {
      var p = null;
      3 < arguments.length && (p = E(Array.prototype.slice.call(arguments, 3), 0));
      return c.call(this, b, d, m, p);
    }
    function c(a, d, e, n) {
      for (;;) {
        if (a = b.c(a, d, e), q(n)) {
          d = A(n), e = Tc(n), n = D(D(n));
        } else {
          return a;
        }
      }
    }
    a.m = 3;
    a.h = function(a) {
      var b = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var n = A(a);
      a = C(a);
      return c(b, d, n, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e, f, h) {
    switch(arguments.length) {
      case 3:
        return a.call(this, b, e, f);
      default:
        return c.e(b, e, f, E(arguments, 3));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 3;
  b.h = c.h;
  b.c = a;
  b.e = c.e;
  return b;
}(), Yc = function() {
  function a(a, b) {
    return null == a ? null : xb(a, b);
  }
  var b = null, c = function() {
    function a(b, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return c.call(this, b, d, n);
    }
    function c(a, d, e) {
      for (;;) {
        if (null == a) {
          return null;
        }
        a = b.a(a, d);
        if (q(e)) {
          d = A(e), e = D(e);
        } else {
          return a;
        }
      }
    }
    a.m = 2;
    a.h = function(a) {
      var b = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return c(b, d, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 1:
        return b;
      case 2:
        return a.call(this, b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.b = function(a) {
    return a;
  };
  b.a = a;
  b.e = c.e;
  return b;
}();
function Zc(a) {
  var b = ca(a);
  return q(b) ? b : a ? q(q(null) ? null : a.sd) ? !0 : a.Dc ? !1 : r(db, a) : r(db, a);
}
function $c(a, b) {
  this.f = a;
  this.k = b;
  this.p = 0;
  this.i = 393217;
}
g = $c.prototype;
g.call = function() {
  function a(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da, ta) {
    a = this.f;
    return O.Ub ? O.Ub(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da, ta) : O.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da, ta);
  }
  function b(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da) {
    a = this;
    return a.f.Ca ? a.f.Ca(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X, da);
  }
  function c(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X) {
    a = this;
    return a.f.Ba ? a.f.Ba(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H, X);
  }
  function d(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H) {
    a = this;
    return a.f.Aa ? a.f.Aa(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, H);
  }
  function e(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) {
    a = this;
    return a.f.za ? a.f.za(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R);
  }
  function f(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) {
    a = this;
    return a.f.ya ? a.f.ya(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K);
  }
  function h(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F) {
    a = this;
    return a.f.xa ? a.f.xa(b, c, d, e, f, h, m, n, p, s, t, w, B, z, F) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F);
  }
  function m(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z) {
    a = this;
    return a.f.wa ? a.f.wa(b, c, d, e, f, h, m, n, p, s, t, w, B, z) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B, z);
  }
  function n(a, b, c, d, e, f, h, m, n, p, s, t, w, B) {
    a = this;
    return a.f.va ? a.f.va(b, c, d, e, f, h, m, n, p, s, t, w, B) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w, B);
  }
  function p(a, b, c, d, e, f, h, m, n, p, s, t, w) {
    a = this;
    return a.f.ua ? a.f.ua(b, c, d, e, f, h, m, n, p, s, t, w) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t, w);
  }
  function s(a, b, c, d, e, f, h, m, n, p, s, t) {
    a = this;
    return a.f.ta ? a.f.ta(b, c, d, e, f, h, m, n, p, s, t) : a.f.call(null, b, c, d, e, f, h, m, n, p, s, t);
  }
  function t(a, b, c, d, e, f, h, m, n, p, s) {
    a = this;
    return a.f.sa ? a.f.sa(b, c, d, e, f, h, m, n, p, s) : a.f.call(null, b, c, d, e, f, h, m, n, p, s);
  }
  function w(a, b, c, d, e, f, h, m, n, p) {
    a = this;
    return a.f.Ea ? a.f.Ea(b, c, d, e, f, h, m, n, p) : a.f.call(null, b, c, d, e, f, h, m, n, p);
  }
  function B(a, b, c, d, e, f, h, m, n) {
    a = this;
    return a.f.Da ? a.f.Da(b, c, d, e, f, h, m, n) : a.f.call(null, b, c, d, e, f, h, m, n);
  }
  function z(a, b, c, d, e, f, h, m) {
    a = this;
    return a.f.ga ? a.f.ga(b, c, d, e, f, h, m) : a.f.call(null, b, c, d, e, f, h, m);
  }
  function F(a, b, c, d, e, f, h) {
    a = this;
    return a.f.U ? a.f.U(b, c, d, e, f, h) : a.f.call(null, b, c, d, e, f, h);
  }
  function K(a, b, c, d, e, f) {
    a = this;
    return a.f.t ? a.f.t(b, c, d, e, f) : a.f.call(null, b, c, d, e, f);
  }
  function R(a, b, c, d, e) {
    a = this;
    return a.f.j ? a.f.j(b, c, d, e) : a.f.call(null, b, c, d, e);
  }
  function X(a, b, c, d) {
    a = this;
    return a.f.c ? a.f.c(b, c, d) : a.f.call(null, b, c, d);
  }
  function da(a, b, c) {
    a = this;
    return a.f.a ? a.f.a(b, c) : a.f.call(null, b, c);
  }
  function ta(a, b) {
    a = this;
    return a.f.b ? a.f.b(b) : a.f.call(null, b);
  }
  function Xa(a) {
    a = this;
    return a.f.o ? a.f.o() : a.f.call(null);
  }
  var H = null, H = function(H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne, lf, zg, Ji, Zl) {
    switch(arguments.length) {
      case 1:
        return Xa.call(this, H);
      case 2:
        return ta.call(this, H, wa);
      case 3:
        return da.call(this, H, wa, Aa);
      case 4:
        return X.call(this, H, wa, Aa, Ga);
      case 5:
        return R.call(this, H, wa, Aa, Ga, Ka);
      case 6:
        return K.call(this, H, wa, Aa, Ga, Ka, Oa);
      case 7:
        return F.call(this, H, wa, Aa, Ga, Ka, Oa, Wa);
      case 8:
        return z.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a);
      case 9:
        return B.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb);
      case 10:
        return w.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb);
      case 11:
        return t.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob);
      case 12:
        return s.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab);
      case 13:
        return p.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib);
      case 14:
        return n.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb);
      case 15:
        return m.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc);
      case 16:
        return h.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc);
      case 17:
        return f.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd);
      case 18:
        return e.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne);
      case 19:
        return d.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne, lf);
      case 20:
        return c.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne, lf, zg);
      case 21:
        return b.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne, lf, zg, Ji);
      case 22:
        return a.call(this, H, wa, Aa, Ga, Ka, Oa, Wa, $a, fb, gb, ob, Ab, Ib, Tb, lc, Oc, vd, ne, lf, zg, Ji, Zl);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  H.b = Xa;
  H.a = ta;
  H.c = da;
  H.j = X;
  H.t = R;
  H.U = K;
  H.ga = F;
  H.Da = z;
  H.Ea = B;
  H.sa = w;
  H.ta = t;
  H.ua = s;
  H.va = p;
  H.wa = n;
  H.xa = m;
  H.ya = h;
  H.za = f;
  H.Aa = e;
  H.Ba = d;
  H.Ca = c;
  H.yd = b;
  H.Ub = a;
  return H;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.o = function() {
  return this.f.o ? this.f.o() : this.f.call(null);
};
g.b = function(a) {
  return this.f.b ? this.f.b(a) : this.f.call(null, a);
};
g.a = function(a, b) {
  return this.f.a ? this.f.a(a, b) : this.f.call(null, a, b);
};
g.c = function(a, b, c) {
  return this.f.c ? this.f.c(a, b, c) : this.f.call(null, a, b, c);
};
g.j = function(a, b, c, d) {
  return this.f.j ? this.f.j(a, b, c, d) : this.f.call(null, a, b, c, d);
};
g.t = function(a, b, c, d, e) {
  return this.f.t ? this.f.t(a, b, c, d, e) : this.f.call(null, a, b, c, d, e);
};
g.U = function(a, b, c, d, e, f) {
  return this.f.U ? this.f.U(a, b, c, d, e, f) : this.f.call(null, a, b, c, d, e, f);
};
g.ga = function(a, b, c, d, e, f, h) {
  return this.f.ga ? this.f.ga(a, b, c, d, e, f, h) : this.f.call(null, a, b, c, d, e, f, h);
};
g.Da = function(a, b, c, d, e, f, h, m) {
  return this.f.Da ? this.f.Da(a, b, c, d, e, f, h, m) : this.f.call(null, a, b, c, d, e, f, h, m);
};
g.Ea = function(a, b, c, d, e, f, h, m, n) {
  return this.f.Ea ? this.f.Ea(a, b, c, d, e, f, h, m, n) : this.f.call(null, a, b, c, d, e, f, h, m, n);
};
g.sa = function(a, b, c, d, e, f, h, m, n, p) {
  return this.f.sa ? this.f.sa(a, b, c, d, e, f, h, m, n, p) : this.f.call(null, a, b, c, d, e, f, h, m, n, p);
};
g.ta = function(a, b, c, d, e, f, h, m, n, p, s) {
  return this.f.ta ? this.f.ta(a, b, c, d, e, f, h, m, n, p, s) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s);
};
g.ua = function(a, b, c, d, e, f, h, m, n, p, s, t) {
  return this.f.ua ? this.f.ua(a, b, c, d, e, f, h, m, n, p, s, t) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t);
};
g.va = function(a, b, c, d, e, f, h, m, n, p, s, t, w) {
  return this.f.va ? this.f.va(a, b, c, d, e, f, h, m, n, p, s, t, w) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w);
};
g.wa = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B) {
  return this.f.wa ? this.f.wa(a, b, c, d, e, f, h, m, n, p, s, t, w, B) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B);
};
g.xa = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z) {
  return this.f.xa ? this.f.xa(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z);
};
g.ya = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F) {
  return this.f.ya ? this.f.ya(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F);
};
g.za = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) {
  return this.f.za ? this.f.za(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K);
};
g.Aa = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) {
  return this.f.Aa ? this.f.Aa(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R);
};
g.Ba = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X) {
  return this.f.Ba ? this.f.Ba(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X);
};
g.Ca = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da) {
  return this.f.Ca ? this.f.Ca(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da) : this.f.call(null, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da);
};
g.yd = function(a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta) {
  var Xa = this.f;
  return O.Ub ? O.Ub(Xa, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta) : O.call(null, Xa, a, b, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta);
};
g.sd = !0;
g.H = function(a, b) {
  return new $c(this.f, b);
};
g.D = function() {
  return this.k;
};
function ad(a, b) {
  return Zc(a) && !(a ? a.i & 262144 || a.ce || (a.i ? 0 : r(Lb, a)) : r(Lb, a)) ? new $c(a, b) : null == a ? null : Mb(a, b);
}
function bd(a) {
  var b = null != a;
  return(b ? a ? a.i & 131072 || a.Ad || (a.i ? 0 : r(Jb, a)) : r(Jb, a) : b) ? Kb(a) : null;
}
function cd(a) {
  return null == a || Ua(y(a));
}
function dd(a) {
  return null == a ? !1 : a ? a.i & 8 || a.Ud ? !0 : a.i ? !1 : r(kb, a) : r(kb, a);
}
function ed(a) {
  return null == a ? !1 : a ? a.i & 4096 || a.ae ? !0 : a.i ? !1 : r(Cb, a) : r(Cb, a);
}
function fd(a) {
  return null == a ? !1 : a ? a.i & 1024 || a.Yd ? !0 : a.i ? !1 : r(wb, a) : r(wb, a);
}
function gd(a) {
  return a ? a.i & 16384 || a.be ? !0 : a.i ? !1 : r(Fb, a) : r(Fb, a);
}
function hd(a) {
  return a ? a.p & 512 || a.Td ? !0 : !1 : !1;
}
function id(a) {
  var b = [];
  Ca(a, function(a, b) {
    return function(a, c) {
      return b.push(c);
    };
  }(a, b));
  return b;
}
function jd(a, b, c, d, e) {
  for (;0 !== e;) {
    c[d] = a[b], d += 1, e -= 1, b += 1;
  }
}
function kd(a, b, c, d, e) {
  b += e - 1;
  for (d += e - 1;0 !== e;) {
    c[d] = a[b], d -= 1, e -= 1, b -= 1;
  }
}
var ld = {};
function md(a) {
  return null == a ? !1 : a ? a.i & 64 || a.wb ? !0 : a.i ? !1 : r(nb, a) : r(nb, a);
}
function nd(a) {
  return q(a) ? !0 : !1;
}
function od(a) {
  var b = Zc(a);
  return b ? b : a ? a.i & 1 || a.Wd ? !0 : a.i ? !1 : r(eb, a) : r(eb, a);
}
function pd(a, b) {
  return M.c(a, b, ld) === ld ? !1 : !0;
}
function Bc(a, b) {
  if (a === b) {
    return 0;
  }
  if (null == a) {
    return-1;
  }
  if (null == b) {
    return 1;
  }
  if (Va(a) === Va(b)) {
    return a && (a.p & 2048 || a.Sb) ? a.Tb(null, b) : a > b ? 1 : a < b ? -1 : 0;
  }
  throw Error("compare on non-nil objects of different types");
}
var qd = function() {
  function a(a, b, c, h) {
    for (;;) {
      var m = Bc(L.a(a, h), L.a(b, h));
      if (0 === m && h + 1 < c) {
        h += 1;
      } else {
        return m;
      }
    }
  }
  function b(a, b) {
    var f = J(a), h = J(b);
    return f < h ? -1 : f > h ? 1 : c.j(a, b, f, 0);
  }
  var c = null, c = function(c, e, f, h) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 4:
        return a.call(this, c, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.j = a;
  return c;
}(), rd = function() {
  function a(a, b, c) {
    for (c = y(c);;) {
      if (c) {
        var h = A(c);
        b = a.a ? a.a(b, h) : a.call(null, b, h);
        if (Lc(b)) {
          return Hb(b);
        }
        c = D(c);
      } else {
        return b;
      }
    }
  }
  function b(a, b) {
    var c = y(b);
    if (c) {
      var h = A(c), c = D(c);
      return bb.c ? bb.c(a, h, c) : bb.call(null, a, h, c);
    }
    return a.o ? a.o() : a.call(null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), bb = function() {
  function a(a, b, c) {
    return c && (c.i & 524288 || c.Cd) ? c.P(null, a, b) : c instanceof Array ? Nc.c(c, a, b) : "string" === typeof c ? Nc.c(c, a, b) : r(Nb, c) ? Ob.c(c, a, b) : rd.c(a, b, c);
  }
  function b(a, b) {
    return b && (b.i & 524288 || b.Cd) ? b.O(null, a) : b instanceof Array ? Nc.a(b, a) : "string" === typeof b ? Nc.a(b, a) : r(Nb, b) ? Ob.a(b, a) : rd.a(a, b);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function sd(a, b, c) {
  return null != c ? Pb(c, a, b) : b;
}
function td(a) {
  return a;
}
var ud = function() {
  function a(a, b, c, h) {
    a = a.b ? a.b(b) : a.call(null, b);
    c = bb.c(a, c, h);
    return a.b ? a.b(c) : a.call(null, c);
  }
  function b(a, b, f) {
    return c.j(a, b, b.o ? b.o() : b.call(null), f);
  }
  var c = null, c = function(c, e, f, h) {
    switch(arguments.length) {
      case 3:
        return b.call(this, c, e, f);
      case 4:
        return a.call(this, c, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.c = b;
  c.j = a;
  return c;
}();
function wd(a) {
  return a - 1;
}
function xd(a, b) {
  var c = (a - a % b) / b;
  return 0 <= c ? Math.floor.b ? Math.floor.b(c) : Math.floor.call(null, c) : Math.ceil.b ? Math.ceil.b(c) : Math.ceil.call(null, c);
}
function yd(a) {
  a -= a >> 1 & 1431655765;
  a = (a & 858993459) + (a >> 2 & 858993459);
  return 16843009 * (a + (a >> 4) & 252645135) >> 24;
}
function zd(a) {
  var b = 1;
  for (a = y(a);;) {
    if (a && 0 < b) {
      b -= 1, a = D(a);
    } else {
      return a;
    }
  }
}
var v = function() {
  function a(a) {
    return null == a ? "" : "" + a;
  }
  var b = null, c = function() {
    function a(b, d) {
      var m = null;
      1 < arguments.length && (m = E(Array.prototype.slice.call(arguments, 1), 0));
      return c.call(this, b, m);
    }
    function c(a, d) {
      for (var e = new Fa(b.b(a)), n = d;;) {
        if (q(n)) {
          e = e.append(b.b(A(n))), n = D(n);
        } else {
          return e.toString();
        }
      }
    }
    a.m = 1;
    a.h = function(a) {
      var b = A(a);
      a = C(a);
      return c(b, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e) {
    switch(arguments.length) {
      case 0:
        return "";
      case 1:
        return a.call(this, b);
      default:
        return c.e(b, E(arguments, 1));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 1;
  b.h = c.h;
  b.o = function() {
    return "";
  };
  b.b = a;
  b.e = c.e;
  return b;
}(), Ad = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return a.substring(c);
      case 3:
        return a.substring(c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return a.substring(c);
  };
  a.c = function(a, c, d) {
    return a.substring(c, d);
  };
  return a;
}();
function Rc(a, b) {
  var c;
  if (b ? b.i & 16777216 || b.$d || (b.i ? 0 : r(Vb, b)) : r(Vb, b)) {
    if (Pc(a) && Pc(b) && J(a) !== J(b)) {
      c = !1;
    } else {
      a: {
        c = y(a);
        for (var d = y(b);;) {
          if (null == c) {
            c = null == d;
            break a;
          }
          if (null != d && Ac.a(A(c), A(d))) {
            c = D(c), d = D(d);
          } else {
            c = !1;
            break a;
          }
        }
        c = void 0;
      }
    }
  } else {
    c = null;
  }
  return nd(c);
}
function Bd(a, b, c, d, e) {
  this.k = a;
  this.first = b;
  this.Ia = c;
  this.count = d;
  this.n = e;
  this.i = 65937646;
  this.p = 8192;
}
g = Bd.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  return 1 === this.count ? null : this.Ia;
};
g.L = function() {
  return this.count;
};
g.Sa = function() {
  return this.first;
};
g.Ta = function() {
  return qb(this);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return Fc;
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return this.first;
};
g.$ = function() {
  return 1 === this.count ? Fc : this.Ia;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new Bd(b, this.first, this.Ia, this.count, this.n);
};
g.I = function(a, b) {
  return new Bd(this.k, b, this, this.count + 1, null);
};
function Cd(a) {
  this.k = a;
  this.i = 65937614;
  this.p = 8192;
}
g = Cd.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  return null;
};
g.L = function() {
  return 0;
};
g.Sa = function() {
  return null;
};
g.Ta = function() {
  throw Error("Can't pop empty list");
};
g.w = function() {
  return 0;
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return this;
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return null;
};
g.$ = function() {
  return Fc;
};
g.G = function() {
  return null;
};
g.H = function(a, b) {
  return new Cd(b);
};
g.I = function(a, b) {
  return new Bd(this.k, b, null, 1, null);
};
var Fc = new Cd(null);
function Dd(a, b, c, d) {
  this.k = a;
  this.first = b;
  this.Ia = c;
  this.n = d;
  this.i = 65929452;
  this.p = 8192;
}
g = Dd.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  return null == this.Ia ? null : y(this.Ia);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return this.first;
};
g.$ = function() {
  return null == this.Ia ? Fc : this.Ia;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new Dd(b, this.first, this.Ia, this.n);
};
g.I = function(a, b) {
  return new Dd(null, b, this, this.n);
};
function I(a, b) {
  var c = null == b;
  return(c ? c : b && (b.i & 64 || b.wb)) ? new Dd(null, a, b, null) : new Dd(null, a, y(b), null);
}
function P(a, b, c, d) {
  this.ma = a;
  this.name = b;
  this.da = c;
  this.bb = d;
  this.i = 2153775105;
  this.p = 4096;
}
g = P.prototype;
g.v = function(a, b) {
  return Wb(b, ":" + v.b(this.da));
};
g.w = function() {
  var a = this.bb;
  return null != a ? a : this.bb = a = yc(tc(this.name), wc(this.ma)) + 2654435769 | 0;
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return M.a(c, this);
      case 3:
        return M.c(c, this, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return M.a(c, this);
  };
  a.c = function(a, c, d) {
    return M.c(c, this, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return M.a(a, this);
};
g.a = function(a, b) {
  return M.c(a, this, b);
};
g.u = function(a, b) {
  return b instanceof P ? this.da === b.da : !1;
};
g.toString = function() {
  return ":" + v.b(this.da);
};
var Fd = function() {
  function a(a, b) {
    return new P(a, b, "" + v.b(q(a) ? "" + v.b(a) + "/" : null) + v.b(b), null);
  }
  function b(a) {
    if (a instanceof P) {
      return a;
    }
    if (a instanceof Cc) {
      var b;
      if (a && (a.p & 4096 || a.Bd)) {
        b = a.ma;
      } else {
        throw Error("Doesn't support namespace: " + v.b(a));
      }
      return new P(b, Ed.b ? Ed.b(a) : Ed.call(null, a), a.Qa, null);
    }
    return "string" === typeof a ? (b = a.split("/"), 2 === b.length ? new P(b[0], b[1], a, null) : new P(null, b[0], a, null)) : null;
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}();
function Gd(a, b, c, d) {
  this.k = a;
  this.Na = b;
  this.K = c;
  this.n = d;
  this.p = 0;
  this.i = 32374988;
}
g = Gd.prototype;
g.toString = function() {
  return oc(this);
};
function Hd(a) {
  null != a.Na && (a.K = a.Na.o ? a.Na.o() : a.Na.call(null), a.Na = null);
  return a.K;
}
g.D = function() {
  return this.k;
};
g.aa = function() {
  Ub(this);
  return null == this.K ? null : D(this.K);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  Ub(this);
  return null == this.K ? null : A(this.K);
};
g.$ = function() {
  Ub(this);
  return null != this.K ? C(this.K) : Fc;
};
g.G = function() {
  Hd(this);
  if (null == this.K) {
    return null;
  }
  for (var a = this.K;;) {
    if (a instanceof Gd) {
      a = Hd(a);
    } else {
      return this.K = a, y(this.K);
    }
  }
};
g.H = function(a, b) {
  return new Gd(b, this.Na, this.K, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
function Id(a, b) {
  this.uc = a;
  this.end = b;
  this.p = 0;
  this.i = 2;
}
Id.prototype.L = function() {
  return this.end;
};
Id.prototype.add = function(a) {
  this.uc[this.end] = a;
  return this.end += 1;
};
Id.prototype.N = function() {
  var a = new Jd(this.uc, 0, this.end);
  this.uc = null;
  return a;
};
function Kd(a) {
  return new Id(Array(a), 0);
}
function Jd(a, b, c) {
  this.d = a;
  this.S = b;
  this.end = c;
  this.p = 0;
  this.i = 524306;
}
g = Jd.prototype;
g.O = function(a, b) {
  return Nc.j(this.d, b, this.d[this.S], this.S + 1);
};
g.P = function(a, b, c) {
  return Nc.j(this.d, b, c, this.S);
};
g.Uc = function() {
  if (this.S === this.end) {
    throw Error("-drop-first of empty chunk");
  }
  return new Jd(this.d, this.S + 1, this.end);
};
g.C = function(a, b) {
  return this.d[this.S + b];
};
g.Z = function(a, b, c) {
  return 0 <= b && b < this.end - this.S ? this.d[this.S + b] : c;
};
g.L = function() {
  return this.end - this.S;
};
var Ld = function() {
  function a(a, b, c) {
    return new Jd(a, b, c);
  }
  function b(a, b) {
    return new Jd(a, b, a.length);
  }
  function c(a) {
    return new Jd(a, 0, a.length);
  }
  var d = null, d = function(d, f, h) {
    switch(arguments.length) {
      case 1:
        return c.call(this, d);
      case 2:
        return b.call(this, d, f);
      case 3:
        return a.call(this, d, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.b = c;
  d.a = b;
  d.c = a;
  return d;
}();
function Md(a, b, c, d) {
  this.N = a;
  this.pa = b;
  this.k = c;
  this.n = d;
  this.i = 31850732;
  this.p = 1536;
}
g = Md.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  if (1 < ib(this.N)) {
    return new Md(gc(this.N), this.pa, this.k, null);
  }
  var a = Ub(this.pa);
  return null == a ? null : a;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.T = function() {
  return x.a(this.N, 0);
};
g.$ = function() {
  return 1 < ib(this.N) ? new Md(gc(this.N), this.pa, this.k, null) : null == this.pa ? Fc : this.pa;
};
g.G = function() {
  return this;
};
g.wc = function() {
  return this.N;
};
g.xc = function() {
  return null == this.pa ? Fc : this.pa;
};
g.H = function(a, b) {
  return new Md(this.N, this.pa, b, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
g.vc = function() {
  return null == this.pa ? null : this.pa;
};
function Nd(a, b) {
  return 0 === ib(a) ? b : new Md(a, b, null, null);
}
function Od(a, b) {
  a.add(b);
}
function Pd(a) {
  for (var b = [];;) {
    if (y(a)) {
      b.push(A(a)), a = D(a);
    } else {
      return b;
    }
  }
}
function Qd(a, b) {
  if (Pc(a)) {
    return J(a);
  }
  for (var c = a, d = b, e = 0;;) {
    if (0 < d && y(c)) {
      c = D(c), d -= 1, e += 1;
    } else {
      return e;
    }
  }
}
var Sd = function Rd(b) {
  return null == b ? null : null == D(b) ? y(A(b)) : I(A(b), Rd(D(b)));
}, Td = function() {
  function a(a, b) {
    return new Gd(null, function() {
      var c = y(a);
      return c ? hd(c) ? Nd(hc(c), d.a(ic(c), b)) : I(A(c), d.a(C(c), b)) : b;
    }, null, null);
  }
  function b(a) {
    return new Gd(null, function() {
      return a;
    }, null, null);
  }
  function c() {
    return new Gd(null, function() {
      return null;
    }, null, null);
  }
  var d = null, e = function() {
    function a(c, d, e) {
      var f = null;
      2 < arguments.length && (f = E(Array.prototype.slice.call(arguments, 2), 0));
      return b.call(this, c, d, f);
    }
    function b(a, c, e) {
      return function t(a, b) {
        return new Gd(null, function() {
          var c = y(a);
          return c ? hd(c) ? Nd(hc(c), t(ic(c), b)) : I(A(c), t(C(c), b)) : q(b) ? t(A(b), D(b)) : null;
        }, null, null);
      }(d.a(a, c), e);
    }
    a.m = 2;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return b(c, d, a);
    };
    a.e = b;
    return a;
  }(), d = function(d, h, m) {
    switch(arguments.length) {
      case 0:
        return c.call(this);
      case 1:
        return b.call(this, d);
      case 2:
        return a.call(this, d, h);
      default:
        return e.e(d, h, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.m = 2;
  d.h = e.h;
  d.o = c;
  d.b = b;
  d.a = a;
  d.e = e.e;
  return d;
}(), Ud = function() {
  function a(a, b, c, d) {
    return I(a, I(b, I(c, d)));
  }
  function b(a, b, c) {
    return I(a, I(b, c));
  }
  var c = null, d = function() {
    function a(c, d, e, p, s) {
      var t = null;
      4 < arguments.length && (t = E(Array.prototype.slice.call(arguments, 4), 0));
      return b.call(this, c, d, e, p, t);
    }
    function b(a, c, d, e, f) {
      return I(a, I(c, I(d, I(e, Sd(f)))));
    }
    a.m = 4;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var s = A(a);
      a = C(a);
      return b(c, d, e, s, a);
    };
    a.e = b;
    return a;
  }(), c = function(c, f, h, m, n) {
    switch(arguments.length) {
      case 1:
        return y(c);
      case 2:
        return I(c, f);
      case 3:
        return b.call(this, c, f, h);
      case 4:
        return a.call(this, c, f, h, m);
      default:
        return d.e(c, f, h, m, E(arguments, 4));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.m = 4;
  c.h = d.h;
  c.b = function(a) {
    return y(a);
  };
  c.a = function(a, b) {
    return I(a, b);
  };
  c.c = b;
  c.j = a;
  c.e = d.e;
  return c;
}(), Vd = function() {
  function a() {
    return bc(Uc);
  }
  var b = null, c = function() {
    function a(c, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return b.call(this, c, d, n);
    }
    function b(a, c, d) {
      for (;;) {
        if (a = cc(a, c), q(d)) {
          c = A(d), d = D(d);
        } else {
          return a;
        }
      }
    }
    a.m = 2;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return b(c, d, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 0:
        return a.call(this);
      case 1:
        return b;
      case 2:
        return cc(b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.o = a;
  b.b = function(a) {
    return a;
  };
  b.a = function(a, b) {
    return cc(a, b);
  };
  b.e = c.e;
  return b;
}(), Wd = function() {
  var a = null, b = function() {
    function a(c, f, h, m) {
      var n = null;
      3 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 3), 0));
      return b.call(this, c, f, h, n);
    }
    function b(a, c, d, m) {
      for (;;) {
        if (a = ec(a, c, d), q(m)) {
          c = A(m), d = Tc(m), m = D(D(m));
        } else {
          return a;
        }
      }
    }
    a.m = 3;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var h = A(a);
      a = D(a);
      var m = A(a);
      a = C(a);
      return b(c, h, m, a);
    };
    a.e = b;
    return a;
  }(), a = function(a, d, e, f) {
    switch(arguments.length) {
      case 3:
        return ec(a, d, e);
      default:
        return b.e(a, d, e, E(arguments, 3));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.m = 3;
  a.h = b.h;
  a.c = function(a, b, e) {
    return ec(a, b, e);
  };
  a.e = b.e;
  return a;
}();
function Xd(a, b, c) {
  var d = y(c);
  if (0 === b) {
    return a.o ? a.o() : a.call(null);
  }
  c = pb(d);
  var e = qb(d);
  if (1 === b) {
    return a.b ? a.b(c) : a.b ? a.b(c) : a.call(null, c);
  }
  var d = pb(e), f = qb(e);
  if (2 === b) {
    return a.a ? a.a(c, d) : a.a ? a.a(c, d) : a.call(null, c, d);
  }
  var e = pb(f), h = qb(f);
  if (3 === b) {
    return a.c ? a.c(c, d, e) : a.c ? a.c(c, d, e) : a.call(null, c, d, e);
  }
  var f = pb(h), m = qb(h);
  if (4 === b) {
    return a.j ? a.j(c, d, e, f) : a.j ? a.j(c, d, e, f) : a.call(null, c, d, e, f);
  }
  var h = pb(m), n = qb(m);
  if (5 === b) {
    return a.t ? a.t(c, d, e, f, h) : a.t ? a.t(c, d, e, f, h) : a.call(null, c, d, e, f, h);
  }
  var m = pb(n), p = qb(n);
  if (6 === b) {
    return a.U ? a.U(c, d, e, f, h, m) : a.U ? a.U(c, d, e, f, h, m) : a.call(null, c, d, e, f, h, m);
  }
  var n = pb(p), s = qb(p);
  if (7 === b) {
    return a.ga ? a.ga(c, d, e, f, h, m, n) : a.ga ? a.ga(c, d, e, f, h, m, n) : a.call(null, c, d, e, f, h, m, n);
  }
  var p = pb(s), t = qb(s);
  if (8 === b) {
    return a.Da ? a.Da(c, d, e, f, h, m, n, p) : a.Da ? a.Da(c, d, e, f, h, m, n, p) : a.call(null, c, d, e, f, h, m, n, p);
  }
  var s = pb(t), w = qb(t);
  if (9 === b) {
    return a.Ea ? a.Ea(c, d, e, f, h, m, n, p, s) : a.Ea ? a.Ea(c, d, e, f, h, m, n, p, s) : a.call(null, c, d, e, f, h, m, n, p, s);
  }
  var t = pb(w), B = qb(w);
  if (10 === b) {
    return a.sa ? a.sa(c, d, e, f, h, m, n, p, s, t) : a.sa ? a.sa(c, d, e, f, h, m, n, p, s, t) : a.call(null, c, d, e, f, h, m, n, p, s, t);
  }
  var w = pb(B), z = qb(B);
  if (11 === b) {
    return a.ta ? a.ta(c, d, e, f, h, m, n, p, s, t, w) : a.ta ? a.ta(c, d, e, f, h, m, n, p, s, t, w) : a.call(null, c, d, e, f, h, m, n, p, s, t, w);
  }
  var B = pb(z), F = qb(z);
  if (12 === b) {
    return a.ua ? a.ua(c, d, e, f, h, m, n, p, s, t, w, B) : a.ua ? a.ua(c, d, e, f, h, m, n, p, s, t, w, B) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B);
  }
  var z = pb(F), K = qb(F);
  if (13 === b) {
    return a.va ? a.va(c, d, e, f, h, m, n, p, s, t, w, B, z) : a.va ? a.va(c, d, e, f, h, m, n, p, s, t, w, B, z) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z);
  }
  var F = pb(K), R = qb(K);
  if (14 === b) {
    return a.wa ? a.wa(c, d, e, f, h, m, n, p, s, t, w, B, z, F) : a.wa ? a.wa(c, d, e, f, h, m, n, p, s, t, w, B, z, F) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F);
  }
  var K = pb(R), X = qb(R);
  if (15 === b) {
    return a.xa ? a.xa(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) : a.xa ? a.xa(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K);
  }
  var R = pb(X), da = qb(X);
  if (16 === b) {
    return a.ya ? a.ya(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) : a.ya ? a.ya(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R);
  }
  var X = pb(da), ta = qb(da);
  if (17 === b) {
    return a.za ? a.za(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X) : a.za ? a.za(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X);
  }
  var da = pb(ta), Xa = qb(ta);
  if (18 === b) {
    return a.Aa ? a.Aa(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da) : a.Aa ? a.Aa(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da);
  }
  ta = pb(Xa);
  Xa = qb(Xa);
  if (19 === b) {
    return a.Ba ? a.Ba(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta) : a.Ba ? a.Ba(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta);
  }
  var H = pb(Xa);
  qb(Xa);
  if (20 === b) {
    return a.Ca ? a.Ca(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta, H) : a.Ca ? a.Ca(c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta, H) : a.call(null, c, d, e, f, h, m, n, p, s, t, w, B, z, F, K, R, X, da, ta, H);
  }
  throw Error("Only up to 20 arguments supported on functions");
}
var O = function() {
  function a(a, b, c, d, e) {
    b = Ud.j(b, c, d, e);
    c = a.m;
    return a.h ? (d = Qd(b, c + 1), d <= c ? Xd(a, d, b) : a.h(b)) : a.apply(a, Pd(b));
  }
  function b(a, b, c, d) {
    b = Ud.c(b, c, d);
    c = a.m;
    return a.h ? (d = Qd(b, c + 1), d <= c ? Xd(a, d, b) : a.h(b)) : a.apply(a, Pd(b));
  }
  function c(a, b, c) {
    b = Ud.a(b, c);
    c = a.m;
    if (a.h) {
      var d = Qd(b, c + 1);
      return d <= c ? Xd(a, d, b) : a.h(b);
    }
    return a.apply(a, Pd(b));
  }
  function d(a, b) {
    var c = a.m;
    if (a.h) {
      var d = Qd(b, c + 1);
      return d <= c ? Xd(a, d, b) : a.h(b);
    }
    return a.apply(a, Pd(b));
  }
  var e = null, f = function() {
    function a(c, d, e, f, h, B) {
      var z = null;
      5 < arguments.length && (z = E(Array.prototype.slice.call(arguments, 5), 0));
      return b.call(this, c, d, e, f, h, z);
    }
    function b(a, c, d, e, f, h) {
      c = I(c, I(d, I(e, I(f, Sd(h)))));
      d = a.m;
      return a.h ? (e = Qd(c, d + 1), e <= d ? Xd(a, e, c) : a.h(c)) : a.apply(a, Pd(c));
    }
    a.m = 5;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var f = A(a);
      a = D(a);
      var h = A(a);
      a = C(a);
      return b(c, d, e, f, h, a);
    };
    a.e = b;
    return a;
  }(), e = function(e, m, n, p, s, t) {
    switch(arguments.length) {
      case 2:
        return d.call(this, e, m);
      case 3:
        return c.call(this, e, m, n);
      case 4:
        return b.call(this, e, m, n, p);
      case 5:
        return a.call(this, e, m, n, p, s);
      default:
        return f.e(e, m, n, p, s, E(arguments, 5));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  e.m = 5;
  e.h = f.h;
  e.a = d;
  e.c = c;
  e.j = b;
  e.t = a;
  e.e = f.e;
  return e;
}(), Yd = function() {
  function a(a, b) {
    return!Ac.a(a, b);
  }
  var b = null, c = function() {
    function a(c, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return b.call(this, c, d, n);
    }
    function b(a, c, d) {
      return Ua(O.j(Ac, a, c, d));
    }
    a.m = 2;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return b(c, d, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 1:
        return!1;
      case 2:
        return a.call(this, b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.b = function() {
    return!1;
  };
  b.a = a;
  b.e = c.e;
  return b;
}();
function Zd(a, b) {
  for (;;) {
    if (null == y(b)) {
      return!0;
    }
    var c;
    c = A(b);
    c = a.b ? a.b(c) : a.call(null, c);
    if (q(c)) {
      c = a;
      var d = D(b);
      a = c;
      b = d;
    } else {
      return!1;
    }
  }
}
function $d(a) {
  for (var b = td;;) {
    if (y(a)) {
      var c;
      c = A(a);
      c = b.b ? b.b(c) : b.call(null, c);
      if (q(c)) {
        return c;
      }
      a = D(a);
    } else {
      return null;
    }
  }
}
function ae(a) {
  return function() {
    function b(b, c) {
      return Ua(a.a ? a.a(b, c) : a.call(null, b, c));
    }
    function c(b) {
      return Ua(a.b ? a.b(b) : a.call(null, b));
    }
    function d() {
      return Ua(a.o ? a.o() : a.call(null));
    }
    var e = null, f = function() {
      function b(a, d, e) {
        var f = null;
        2 < arguments.length && (f = E(Array.prototype.slice.call(arguments, 2), 0));
        return c.call(this, a, d, f);
      }
      function c(b, d, e) {
        return Ua(O.j(a, b, d, e));
      }
      b.m = 2;
      b.h = function(a) {
        var b = A(a);
        a = D(a);
        var d = A(a);
        a = C(a);
        return c(b, d, a);
      };
      b.e = c;
      return b;
    }(), e = function(a, e, n) {
      switch(arguments.length) {
        case 0:
          return d.call(this);
        case 1:
          return c.call(this, a);
        case 2:
          return b.call(this, a, e);
        default:
          return f.e(a, e, E(arguments, 2));
      }
      throw Error("Invalid arity: " + arguments.length);
    };
    e.m = 2;
    e.h = f.h;
    e.o = d;
    e.b = c;
    e.a = b;
    e.e = f.e;
    return e;
  }();
}
var be = function() {
  function a(a, b, c, d) {
    return function() {
      function e(a) {
        var b = null;
        0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
        return s.call(this, b);
      }
      function s(e) {
        return O.t(a, b, c, d, e);
      }
      e.m = 0;
      e.h = function(a) {
        a = y(a);
        return s(a);
      };
      e.e = s;
      return e;
    }();
  }
  function b(a, b, c) {
    return function() {
      function d(a) {
        var b = null;
        0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
        return e.call(this, b);
      }
      function e(d) {
        return O.j(a, b, c, d);
      }
      d.m = 0;
      d.h = function(a) {
        a = y(a);
        return e(a);
      };
      d.e = e;
      return d;
    }();
  }
  function c(a, b) {
    return function() {
      function c(a) {
        var b = null;
        0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
        return d.call(this, b);
      }
      function d(c) {
        return O.c(a, b, c);
      }
      c.m = 0;
      c.h = function(a) {
        a = y(a);
        return d(a);
      };
      c.e = d;
      return c;
    }();
  }
  var d = null, e = function() {
    function a(c, d, e, f, t) {
      var w = null;
      4 < arguments.length && (w = E(Array.prototype.slice.call(arguments, 4), 0));
      return b.call(this, c, d, e, f, w);
    }
    function b(a, c, d, e, f) {
      return function() {
        function b(a) {
          var c = null;
          0 < arguments.length && (c = E(Array.prototype.slice.call(arguments, 0), 0));
          return h.call(this, c);
        }
        function h(b) {
          return O.t(a, c, d, e, Td.a(f, b));
        }
        b.m = 0;
        b.h = function(a) {
          a = y(a);
          return h(a);
        };
        b.e = h;
        return b;
      }();
    }
    a.m = 4;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var f = A(a);
      a = C(a);
      return b(c, d, e, f, a);
    };
    a.e = b;
    return a;
  }(), d = function(d, h, m, n, p) {
    switch(arguments.length) {
      case 1:
        return d;
      case 2:
        return c.call(this, d, h);
      case 3:
        return b.call(this, d, h, m);
      case 4:
        return a.call(this, d, h, m, n);
      default:
        return e.e(d, h, m, n, E(arguments, 4));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.m = 4;
  d.h = e.h;
  d.b = function(a) {
    return a;
  };
  d.a = c;
  d.c = b;
  d.j = a;
  d.e = e.e;
  return d;
}();
function ce(a, b) {
  return function d(b, f) {
    return new Gd(null, function() {
      var h = y(f);
      if (h) {
        if (hd(h)) {
          for (var m = hc(h), n = J(m), p = Kd(n), s = 0;;) {
            if (s < n) {
              Od(p, function() {
                var d = b + s, f = x.a(m, s);
                return a.a ? a.a(d, f) : a.call(null, d, f);
              }()), s += 1;
            } else {
              break;
            }
          }
          return Nd(p.N(), d(b + n, ic(h)));
        }
        return I(function() {
          var d = A(h);
          return a.a ? a.a(b, d) : a.call(null, b, d);
        }(), d(b + 1, C(h)));
      }
      return null;
    }, null, null);
  }(0, b);
}
function de(a, b, c, d) {
  this.state = a;
  this.k = b;
  this.R = d;
  this.i = 6455296;
  this.p = 16386;
}
g = de.prototype;
g.w = function() {
  return ea(this);
};
g.Yb = function(a, b, c) {
  for (var d = y(this.R), e = null, f = 0, h = 0;;) {
    if (h < f) {
      a = e.C(null, h);
      var m = L.c(a, 0, null);
      a = L.c(a, 1, null);
      var n = b, p = c;
      a.j ? a.j(m, this, n, p) : a.call(null, m, this, n, p);
      h += 1;
    } else {
      if (a = y(d)) {
        d = a, hd(d) ? (e = hc(d), d = ic(d), a = e, f = J(e), e = a) : (a = A(d), m = L.c(a, 0, null), a = L.c(a, 1, null), e = m, f = b, h = c, a.j ? a.j(e, this, f, h) : a.call(null, e, this, f, h), d = D(d), e = null, f = 0), h = 0;
      } else {
        return null;
      }
    }
  }
};
g.Xb = function(a, b, c) {
  this.R = N.c(this.R, b, c);
  return this;
};
g.Zb = function(a, b) {
  return this.R = Yc.a(this.R, b);
};
g.D = function() {
  return this.k;
};
g.sb = function() {
  return this.state;
};
g.u = function(a, b) {
  return this === b;
};
var ge = function() {
  function a(a) {
    return new de(a, null, 0, null);
  }
  var b = null, c = function() {
    function a(c, d) {
      var m = null;
      1 < arguments.length && (m = E(Array.prototype.slice.call(arguments, 1), 0));
      return b.call(this, c, m);
    }
    function b(a, c) {
      var d = md(c) ? O.a(ee, c) : c;
      M.a(d, fe);
      d = M.a(d, Ra);
      return new de(a, d, 0, null);
    }
    a.m = 1;
    a.h = function(a) {
      var c = A(a);
      a = C(a);
      return b(c, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e) {
    switch(arguments.length) {
      case 1:
        return a.call(this, b);
      default:
        return c.e(b, E(arguments, 1));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 1;
  b.h = c.h;
  b.b = a;
  b.e = c.e;
  return b;
}();
function Q(a, b) {
  if (a instanceof de) {
    var c = a.state;
    a.state = b;
    null != a.R && Zb(a, c, b);
    return b;
  }
  return kc(a, b);
}
var S = function() {
  function a(a, b, c, d) {
    if (a instanceof de) {
      var e = a.state;
      b = b.c ? b.c(e, c, d) : b.call(null, e, c, d);
      a = Q(a, b);
    } else {
      a = mc.j(a, b, c, d);
    }
    return a;
  }
  function b(a, b, c) {
    if (a instanceof de) {
      var d = a.state;
      b = b.a ? b.a(d, c) : b.call(null, d, c);
      a = Q(a, b);
    } else {
      a = mc.c(a, b, c);
    }
    return a;
  }
  function c(a, b) {
    var c;
    a instanceof de ? (c = a.state, c = b.b ? b.b(c) : b.call(null, c), c = Q(a, c)) : c = mc.a(a, b);
    return c;
  }
  var d = null, e = function() {
    function a(c, d, e, f, t) {
      var w = null;
      4 < arguments.length && (w = E(Array.prototype.slice.call(arguments, 4), 0));
      return b.call(this, c, d, e, f, w);
    }
    function b(a, c, d, e, f) {
      return a instanceof de ? Q(a, O.t(c, a.state, d, e, f)) : mc.t(a, c, d, e, f);
    }
    a.m = 4;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var f = A(a);
      a = C(a);
      return b(c, d, e, f, a);
    };
    a.e = b;
    return a;
  }(), d = function(d, h, m, n, p) {
    switch(arguments.length) {
      case 2:
        return c.call(this, d, h);
      case 3:
        return b.call(this, d, h, m);
      case 4:
        return a.call(this, d, h, m, n);
      default:
        return e.e(d, h, m, n, E(arguments, 4));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.m = 4;
  d.h = e.h;
  d.a = c;
  d.c = b;
  d.j = a;
  d.e = e.e;
  return d;
}(), he = function() {
  function a(a, b, c, d) {
    return new Gd(null, function() {
      var f = y(b), t = y(c), w = y(d);
      if (f && t && w) {
        var B = I, z;
        z = A(f);
        var F = A(t), K = A(w);
        z = a.c ? a.c(z, F, K) : a.call(null, z, F, K);
        f = B(z, e.j(a, C(f), C(t), C(w)));
      } else {
        f = null;
      }
      return f;
    }, null, null);
  }
  function b(a, b, c) {
    return new Gd(null, function() {
      var d = y(b), f = y(c);
      if (d && f) {
        var t = I, w;
        w = A(d);
        var B = A(f);
        w = a.a ? a.a(w, B) : a.call(null, w, B);
        d = t(w, e.c(a, C(d), C(f)));
      } else {
        d = null;
      }
      return d;
    }, null, null);
  }
  function c(a, b) {
    return new Gd(null, function() {
      var c = y(b);
      if (c) {
        if (hd(c)) {
          for (var d = hc(c), f = J(d), t = Kd(f), w = 0;;) {
            if (w < f) {
              Od(t, function() {
                var b = x.a(d, w);
                return a.b ? a.b(b) : a.call(null, b);
              }()), w += 1;
            } else {
              break;
            }
          }
          return Nd(t.N(), e.a(a, ic(c)));
        }
        return I(function() {
          var b = A(c);
          return a.b ? a.b(b) : a.call(null, b);
        }(), e.a(a, C(c)));
      }
      return null;
    }, null, null);
  }
  function d(a) {
    return function(b) {
      return function() {
        function c(d, e) {
          var f = a.b ? a.b(e) : a.call(null, e);
          return b.a ? b.a(d, f) : b.call(null, d, f);
        }
        function d(a) {
          return b.b ? b.b(a) : b.call(null, a);
        }
        function e() {
          return b.o ? b.o() : b.call(null);
        }
        var f = null, w = function() {
          function c(a, b, e) {
            var f = null;
            2 < arguments.length && (f = E(Array.prototype.slice.call(arguments, 2), 0));
            return d.call(this, a, b, f);
          }
          function d(c, e, f) {
            e = O.c(a, e, f);
            return b.a ? b.a(c, e) : b.call(null, c, e);
          }
          c.m = 2;
          c.h = function(a) {
            var b = A(a);
            a = D(a);
            var c = A(a);
            a = C(a);
            return d(b, c, a);
          };
          c.e = d;
          return c;
        }(), f = function(a, b, f) {
          switch(arguments.length) {
            case 0:
              return e.call(this);
            case 1:
              return d.call(this, a);
            case 2:
              return c.call(this, a, b);
            default:
              return w.e(a, b, E(arguments, 2));
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        f.m = 2;
        f.h = w.h;
        f.o = e;
        f.b = d;
        f.a = c;
        f.e = w.e;
        return f;
      }();
    };
  }
  var e = null, f = function() {
    function a(c, d, e, f, h) {
      var B = null;
      4 < arguments.length && (B = E(Array.prototype.slice.call(arguments, 4), 0));
      return b.call(this, c, d, e, f, B);
    }
    function b(a, c, d, f, h) {
      var m = function F(a) {
        return new Gd(null, function() {
          var b = e.a(y, a);
          return Zd(td, b) ? I(e.a(A, b), F(e.a(C, b))) : null;
        }, null, null);
      };
      return e.a(function() {
        return function(b) {
          return O.a(a, b);
        };
      }(m), m(Vc.e(h, f, E([d, c], 0))));
    }
    a.m = 4;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var f = A(a);
      a = C(a);
      return b(c, d, e, f, a);
    };
    a.e = b;
    return a;
  }(), e = function(e, m, n, p, s) {
    switch(arguments.length) {
      case 1:
        return d.call(this, e);
      case 2:
        return c.call(this, e, m);
      case 3:
        return b.call(this, e, m, n);
      case 4:
        return a.call(this, e, m, n, p);
      default:
        return f.e(e, m, n, p, E(arguments, 4));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  e.m = 4;
  e.h = f.h;
  e.b = d;
  e.a = c;
  e.c = b;
  e.j = a;
  e.e = f.e;
  return e;
}(), ie = function() {
  function a(a, b) {
    return new Gd(null, function() {
      if (0 < a) {
        var f = y(b);
        return f ? I(A(f), c.a(a - 1, C(f))) : null;
      }
      return null;
    }, null, null);
  }
  function b(a) {
    return function(b) {
      return function(a) {
        return function() {
          function c(d, h) {
            var m = Hb(a), n = S.a(a, wd), m = 0 < m ? b.a ? b.a(d, h) : b.call(null, d, h) : d;
            return 0 < n ? m : new Kc(m);
          }
          function d(a) {
            return b.b ? b.b(a) : b.call(null, a);
          }
          function n() {
            return b.o ? b.o() : b.call(null);
          }
          var p = null, p = function(a, b) {
            switch(arguments.length) {
              case 0:
                return n.call(this);
              case 1:
                return d.call(this, a);
              case 2:
                return c.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          p.o = n;
          p.b = d;
          p.a = c;
          return p;
        }();
      }(ge.b(a));
    };
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), je = function() {
  function a(a, b) {
    return new Gd(null, function(c) {
      return function() {
        return c(a, b);
      };
    }(function(a, b) {
      for (;;) {
        var c = y(b);
        if (0 < a && c) {
          var d = a - 1, c = C(c);
          a = d;
          b = c;
        } else {
          return c;
        }
      }
    }), null, null);
  }
  function b(a) {
    return function(b) {
      return function(a) {
        return function() {
          function c(d, h) {
            var m = Hb(a);
            S.a(a, wd);
            return 0 < m ? d : b.a ? b.a(d, h) : b.call(null, d, h);
          }
          function d(a) {
            return b.b ? b.b(a) : b.call(null, a);
          }
          function n() {
            return b.o ? b.o() : b.call(null);
          }
          var p = null, p = function(a, b) {
            switch(arguments.length) {
              case 0:
                return n.call(this);
              case 1:
                return d.call(this, a);
              case 2:
                return c.call(this, a, b);
            }
            throw Error("Invalid arity: " + arguments.length);
          };
          p.o = n;
          p.b = d;
          p.a = c;
          return p;
        }();
      }(ge.b(a));
    };
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), ke = function() {
  function a(a, b) {
    return he.c(function(a) {
      return a;
    }, b, je.a(a, b));
  }
  function b(a) {
    return c.a(1, a);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), le = function() {
  function a(a, b) {
    return ie.a(a, c.b(b));
  }
  function b(a) {
    return new Gd(null, function() {
      return I(a, c.b(a));
    }, null, null);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), me = function() {
  function a(a, b) {
    return ie.a(a, c.b(b));
  }
  function b(a) {
    return new Gd(null, function() {
      return I(a.o ? a.o() : a.call(null), c.b(a));
    }, null, null);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), oe = function() {
  function a(a, c) {
    return new Gd(null, function() {
      var f = y(a), h = y(c);
      return f && h ? I(A(f), I(A(h), b.a(C(f), C(h)))) : null;
    }, null, null);
  }
  var b = null, c = function() {
    function a(b, d, m) {
      var n = null;
      2 < arguments.length && (n = E(Array.prototype.slice.call(arguments, 2), 0));
      return c.call(this, b, d, n);
    }
    function c(a, d, e) {
      return new Gd(null, function() {
        var c = he.a(y, Vc.e(e, d, E([a], 0)));
        return Zd(td, c) ? Td.a(he.a(A, c), O.a(b, he.a(C, c))) : null;
      }, null, null);
    }
    a.m = 2;
    a.h = function(a) {
      var b = A(a);
      a = D(a);
      var d = A(a);
      a = C(a);
      return c(b, d, a);
    };
    a.e = c;
    return a;
  }(), b = function(b, e, f) {
    switch(arguments.length) {
      case 2:
        return a.call(this, b, e);
      default:
        return c.e(b, e, E(arguments, 2));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 2;
  b.h = c.h;
  b.a = a;
  b.e = c.e;
  return b;
}();
function pe(a, b) {
  return je.a(1, oe.a(le.b(a), b));
}
var qe = function() {
  function a(a, b) {
    return new Gd(null, function() {
      var f = y(b);
      if (f) {
        if (hd(f)) {
          for (var h = hc(f), m = J(h), n = Kd(m), p = 0;;) {
            if (p < m) {
              var s;
              s = x.a(h, p);
              s = a.b ? a.b(s) : a.call(null, s);
              q(s) && (s = x.a(h, p), n.add(s));
              p += 1;
            } else {
              break;
            }
          }
          return Nd(n.N(), c.a(a, ic(f)));
        }
        h = A(f);
        f = C(f);
        return q(a.b ? a.b(h) : a.call(null, h)) ? I(h, c.a(a, f)) : c.a(a, f);
      }
      return null;
    }, null, null);
  }
  function b(a) {
    return function(b) {
      return function() {
        function c(f, h) {
          return q(a.b ? a.b(h) : a.call(null, h)) ? b.a ? b.a(f, h) : b.call(null, f, h) : f;
        }
        function h(a) {
          return b.b ? b.b(a) : b.call(null, a);
        }
        function m() {
          return b.o ? b.o() : b.call(null);
        }
        var n = null, n = function(a, b) {
          switch(arguments.length) {
            case 0:
              return m.call(this);
            case 1:
              return h.call(this, a);
            case 2:
              return c.call(this, a, b);
          }
          throw Error("Invalid arity: " + arguments.length);
        };
        n.o = m;
        n.b = h;
        n.a = c;
        return n;
      }();
    };
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), re = function() {
  function a(a, b) {
    return qe.a(ae(a), b);
  }
  function b(a) {
    return qe.b(ae(a));
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), se = function() {
  function a(a, b, c) {
    a && (a.p & 4 || a.ud) ? (b = ud.j(b, Vd, bc(a), c), b = dc(b), a = ad(b, bd(a))) : a = ud.j(b, Vc, a, c);
    return a;
  }
  function b(a, b) {
    var c;
    null != a ? a && (a.p & 4 || a.ud) ? (c = bb.c(cc, bc(a), b), c = dc(c), c = ad(c, bd(a))) : c = bb.c(lb, a, b) : c = bb.c(Vc, Fc, b);
    return c;
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), te = function() {
  function a(a, b, c) {
    var h = ld;
    for (b = y(b);;) {
      if (b) {
        var m = a;
        if (m ? m.i & 256 || m.Wc || (m.i ? 0 : r(sb, m)) : r(sb, m)) {
          a = M.c(a, A(b), h);
          if (h === a) {
            return c;
          }
          b = D(b);
        } else {
          return c;
        }
      } else {
        return a;
      }
    }
  }
  function b(a, b) {
    return c.c(a, b, null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), ve = function ue(b, c, d) {
  var e = L.c(c, 0, null);
  return(c = zd(c)) ? N.c(b, e, ue(M.a(b, e), c, d)) : N.c(b, e, d);
}, we = function() {
  function a(a, b, c, d, f, t) {
    var w = L.c(b, 0, null);
    return(b = zd(b)) ? N.c(a, w, e.U(M.a(a, w), b, c, d, f, t)) : N.c(a, w, function() {
      var b = M.a(a, w);
      return c.j ? c.j(b, d, f, t) : c.call(null, b, d, f, t);
    }());
  }
  function b(a, b, c, d, f) {
    var t = L.c(b, 0, null);
    return(b = zd(b)) ? N.c(a, t, e.t(M.a(a, t), b, c, d, f)) : N.c(a, t, function() {
      var b = M.a(a, t);
      return c.c ? c.c(b, d, f) : c.call(null, b, d, f);
    }());
  }
  function c(a, b, c, d) {
    var f = L.c(b, 0, null);
    return(b = zd(b)) ? N.c(a, f, e.j(M.a(a, f), b, c, d)) : N.c(a, f, function() {
      var b = M.a(a, f);
      return c.a ? c.a(b, d) : c.call(null, b, d);
    }());
  }
  function d(a, b, c) {
    var d = L.c(b, 0, null);
    return(b = zd(b)) ? N.c(a, d, e.c(M.a(a, d), b, c)) : N.c(a, d, function() {
      var b = M.a(a, d);
      return c.b ? c.b(b) : c.call(null, b);
    }());
  }
  var e = null, f = function() {
    function a(c, d, e, f, h, B, z) {
      var F = null;
      6 < arguments.length && (F = E(Array.prototype.slice.call(arguments, 6), 0));
      return b.call(this, c, d, e, f, h, B, F);
    }
    function b(a, c, d, f, h, m, z) {
      var F = L.c(c, 0, null);
      return(c = zd(c)) ? N.c(a, F, O.e(e, M.a(a, F), c, d, f, E([h, m, z], 0))) : N.c(a, F, O.e(d, M.a(a, F), f, h, m, E([z], 0)));
    }
    a.m = 6;
    a.h = function(a) {
      var c = A(a);
      a = D(a);
      var d = A(a);
      a = D(a);
      var e = A(a);
      a = D(a);
      var f = A(a);
      a = D(a);
      var h = A(a);
      a = D(a);
      var z = A(a);
      a = C(a);
      return b(c, d, e, f, h, z, a);
    };
    a.e = b;
    return a;
  }(), e = function(e, m, n, p, s, t, w) {
    switch(arguments.length) {
      case 3:
        return d.call(this, e, m, n);
      case 4:
        return c.call(this, e, m, n, p);
      case 5:
        return b.call(this, e, m, n, p, s);
      case 6:
        return a.call(this, e, m, n, p, s, t);
      default:
        return f.e(e, m, n, p, s, t, E(arguments, 6));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  e.m = 6;
  e.h = f.h;
  e.c = d;
  e.j = c;
  e.t = b;
  e.U = a;
  e.e = f.e;
  return e;
}();
function xe(a, b) {
  this.A = a;
  this.d = b;
}
function ye(a) {
  return new xe(a, [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null]);
}
function ze(a) {
  return new xe(a.A, Za(a.d));
}
function Ae(a) {
  a = a.g;
  return 32 > a ? 0 : a - 1 >>> 5 << 5;
}
function Be(a, b, c) {
  for (;;) {
    if (0 === b) {
      return c;
    }
    var d = ye(a);
    d.d[0] = c;
    c = d;
    b -= 5;
  }
}
var De = function Ce(b, c, d, e) {
  var f = ze(d), h = b.g - 1 >>> c & 31;
  5 === c ? f.d[h] = e : (d = d.d[h], b = null != d ? Ce(b, c - 5, d, e) : Be(null, c - 5, e), f.d[h] = b);
  return f;
};
function Ee(a, b) {
  throw Error("No item " + v.b(a) + " in vector of length " + v.b(b));
}
function Fe(a, b) {
  if (b >= Ae(a)) {
    return a.X;
  }
  for (var c = a.root, d = a.shift;;) {
    if (0 < d) {
      var e = d - 5, c = c.d[b >>> d & 31], d = e
    } else {
      return c.d;
    }
  }
}
function Ge(a, b) {
  return 0 <= b && b < a.g ? Fe(a, b) : Ee(b, a.g);
}
var Ie = function He(b, c, d, e, f) {
  var h = ze(d);
  if (0 === c) {
    h.d[e & 31] = f;
  } else {
    var m = e >>> c & 31;
    b = He(b, c - 5, d.d[m], e, f);
    h.d[m] = b;
  }
  return h;
}, Ke = function Je(b, c, d) {
  var e = b.g - 2 >>> c & 31;
  if (5 < c) {
    b = Je(b, c - 5, d.d[e]);
    if (null == b && 0 === e) {
      return null;
    }
    d = ze(d);
    d.d[e] = b;
    return d;
  }
  if (0 === e) {
    return null;
  }
  d = ze(d);
  d.d[e] = null;
  return d;
};
function T(a, b, c, d, e, f) {
  this.k = a;
  this.g = b;
  this.shift = c;
  this.root = d;
  this.X = e;
  this.n = f;
  this.i = 167668511;
  this.p = 8196;
}
g = T.prototype;
g.toString = function() {
  return oc(this);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return "number" === typeof b ? x.c(this, b, c) : c;
};
g.tb = function(a, b, c) {
  a = [0, c];
  for (c = 0;;) {
    if (c < this.g) {
      var d = Fe(this, c), e = d.length;
      a: {
        for (var f = 0, h = a[1];;) {
          if (f < e) {
            var m = f + c, n = d[f], h = b.c ? b.c(h, m, n) : b.call(null, h, m, n);
            if (Lc(h)) {
              d = h;
              break a;
            }
            f += 1;
          } else {
            a[0] = e;
            d = a[1] = h;
            break a;
          }
        }
        d = void 0;
      }
      if (Lc(d)) {
        return b = d, G.b ? G.b(b) : G.call(null, b);
      }
      c += a[0];
    } else {
      return a[1];
    }
  }
};
g.C = function(a, b) {
  return Ge(this, b)[b & 31];
};
g.Z = function(a, b, c) {
  return 0 <= b && b < this.g ? Fe(this, b)[b & 31] : c;
};
g.Ua = function(a, b, c) {
  if (0 <= b && b < this.g) {
    return Ae(this) <= b ? (a = Za(this.X), a[b & 31] = c, new T(this.k, this.g, this.shift, this.root, a, null)) : new T(this.k, this.g, this.shift, Ie(this, this.shift, this.root, b, c), this.X, null);
  }
  if (b === this.g) {
    return lb(this, c);
  }
  throw Error("Index " + v.b(b) + " out of bounds  [0," + v.b(this.g) + "]");
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return this.g;
};
g.ub = function() {
  return x.a(this, 0);
};
g.vb = function() {
  return x.a(this, 1);
};
g.Sa = function() {
  return 0 < this.g ? x.a(this, this.g - 1) : null;
};
g.Ta = function() {
  if (0 === this.g) {
    throw Error("Can't pop empty vector");
  }
  if (1 === this.g) {
    return Mb(Uc, this.k);
  }
  if (1 < this.g - Ae(this)) {
    return new T(this.k, this.g - 1, this.shift, this.root, this.X.slice(0, -1), null);
  }
  var a = Fe(this, this.g - 2), b = Ke(this, this.shift, this.root), b = null == b ? U : b, c = this.g - 1;
  return 5 < this.shift && null == b.d[1] ? new T(this.k, c, this.shift - 5, b.d[0], a, null) : new T(this.k, c, this.shift, b, a, null);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.cb = function() {
  var a = this;
  return new Le(a.g, a.shift, function() {
    var b = a.root;
    return Me.b ? Me.b(b) : Me.call(null, b);
  }(), function() {
    var b = a.X;
    return Ne.b ? Ne.b(b) : Ne.call(null, b);
  }());
};
g.J = function() {
  return ad(Uc, this.k);
};
g.O = function(a, b) {
  return Mc.a(this, b);
};
g.P = function(a, b, c) {
  return Mc.c(this, b, c);
};
g.La = function(a, b, c) {
  if ("number" === typeof b) {
    return Gb(this, b, c);
  }
  throw Error("Vector's key for assoc must be a number.");
};
g.G = function() {
  if (0 === this.g) {
    return null;
  }
  if (32 >= this.g) {
    return new Ec(this.X, 0);
  }
  var a;
  a: {
    a = this.root;
    for (var b = this.shift;;) {
      if (0 < b) {
        b -= 5, a = a.d[0];
      } else {
        a = a.d;
        break a;
      }
    }
    a = void 0;
  }
  return Oe.j ? Oe.j(this, a, 0, 0) : Oe.call(null, this, a, 0, 0);
};
g.H = function(a, b) {
  return new T(b, this.g, this.shift, this.root, this.X, this.n);
};
g.I = function(a, b) {
  if (32 > this.g - Ae(this)) {
    for (var c = this.X.length, d = Array(c + 1), e = 0;;) {
      if (e < c) {
        d[e] = this.X[e], e += 1;
      } else {
        break;
      }
    }
    d[c] = b;
    return new T(this.k, this.g + 1, this.shift, this.root, d, null);
  }
  c = (d = this.g >>> 5 > 1 << this.shift) ? this.shift + 5 : this.shift;
  d ? (d = ye(null), d.d[0] = this.root, e = Be(null, this.shift, new xe(null, this.X)), d.d[1] = e) : d = De(this, this.shift, this.root, new xe(null, this.X));
  return new T(this.k, this.g + 1, c, d, [b], null);
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.C(null, c);
      case 3:
        return this.Z(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.C(null, c);
  };
  a.c = function(a, c, d) {
    return this.Z(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.C(null, a);
};
g.a = function(a, b) {
  return this.Z(null, a, b);
};
var U = new xe(null, [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null]), Uc = new T(null, 0, 5, U, [], 0);
function Pe(a) {
  return dc(bb.c(cc, bc(Uc), a));
}
var Qe = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    if (a instanceof Ec && 0 === a.q) {
      a: {
        a = a.d;
        var b = a.length;
        if (32 > b) {
          a = new T(null, b, 5, U, a, null);
        } else {
          for (var e = 32, f = (new T(null, 32, 5, U, a.slice(0, 32), null)).cb(null);;) {
            if (e < b) {
              var h = e + 1, f = Vd.a(f, a[e]), e = h
            } else {
              a = dc(f);
              break a;
            }
          }
          a = void 0;
        }
      }
    } else {
      a = Pe(a);
    }
    return a;
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function Re(a, b, c, d, e, f) {
  this.ea = a;
  this.Ha = b;
  this.q = c;
  this.S = d;
  this.k = e;
  this.n = f;
  this.i = 32375020;
  this.p = 1536;
}
g = Re.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  if (this.S + 1 < this.Ha.length) {
    var a;
    a = this.ea;
    var b = this.Ha, c = this.q, d = this.S + 1;
    a = Oe.j ? Oe.j(a, b, c, d) : Oe.call(null, a, b, c, d);
    return null == a ? null : a;
  }
  return jc(this);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Uc, this.k);
};
g.O = function(a, b) {
  var c = this;
  return Mc.a(function() {
    var a = c.ea, b = c.q + c.S, f = J(c.ea);
    return Se.c ? Se.c(a, b, f) : Se.call(null, a, b, f);
  }(), b);
};
g.P = function(a, b, c) {
  var d = this;
  return Mc.c(function() {
    var a = d.ea, b = d.q + d.S, c = J(d.ea);
    return Se.c ? Se.c(a, b, c) : Se.call(null, a, b, c);
  }(), b, c);
};
g.T = function() {
  return this.Ha[this.S];
};
g.$ = function() {
  if (this.S + 1 < this.Ha.length) {
    var a;
    a = this.ea;
    var b = this.Ha, c = this.q, d = this.S + 1;
    a = Oe.j ? Oe.j(a, b, c, d) : Oe.call(null, a, b, c, d);
    return null == a ? Fc : a;
  }
  return ic(this);
};
g.G = function() {
  return this;
};
g.wc = function() {
  return Ld.a(this.Ha, this.S);
};
g.xc = function() {
  var a = this.q + this.Ha.length;
  if (a < ib(this.ea)) {
    var b = this.ea, c = Fe(this.ea, a);
    return Oe.j ? Oe.j(b, c, a, 0) : Oe.call(null, b, c, a, 0);
  }
  return Fc;
};
g.H = function(a, b) {
  var c = this.ea, d = this.Ha, e = this.q, f = this.S;
  return Oe.t ? Oe.t(c, d, e, f, b) : Oe.call(null, c, d, e, f, b);
};
g.I = function(a, b) {
  return I(b, this);
};
g.vc = function() {
  var a = this.q + this.Ha.length;
  if (a < ib(this.ea)) {
    var b = this.ea, c = Fe(this.ea, a);
    return Oe.j ? Oe.j(b, c, a, 0) : Oe.call(null, b, c, a, 0);
  }
  return null;
};
var Oe = function() {
  function a(a, b, c, d, n) {
    return new Re(a, b, c, d, n, null);
  }
  function b(a, b, c, d) {
    return new Re(a, b, c, d, null, null);
  }
  function c(a, b, c) {
    return new Re(a, Ge(a, b), b, c, null, null);
  }
  var d = null, d = function(d, f, h, m, n) {
    switch(arguments.length) {
      case 3:
        return c.call(this, d, f, h);
      case 4:
        return b.call(this, d, f, h, m);
      case 5:
        return a.call(this, d, f, h, m, n);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  d.c = c;
  d.j = b;
  d.t = a;
  return d;
}();
function Te(a, b, c, d, e) {
  this.k = a;
  this.Ka = b;
  this.start = c;
  this.end = d;
  this.n = e;
  this.i = 166617887;
  this.p = 8192;
}
g = Te.prototype;
g.toString = function() {
  return oc(this);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return "number" === typeof b ? x.c(this, b, c) : c;
};
g.C = function(a, b) {
  return 0 > b || this.end <= this.start + b ? Ee(b, this.end - this.start) : x.a(this.Ka, this.start + b);
};
g.Z = function(a, b, c) {
  return 0 > b || this.end <= this.start + b ? c : x.c(this.Ka, this.start + b, c);
};
g.Ua = function(a, b, c) {
  var d = this.start + b;
  a = this.k;
  c = N.c(this.Ka, d, c);
  b = this.start;
  var e = this.end, d = d + 1, d = e > d ? e : d;
  return Ue.t ? Ue.t(a, c, b, d, null) : Ue.call(null, a, c, b, d, null);
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return this.end - this.start;
};
g.Sa = function() {
  return x.a(this.Ka, this.end - 1);
};
g.Ta = function() {
  if (this.start === this.end) {
    throw Error("Can't pop empty vector");
  }
  var a = this.k, b = this.Ka, c = this.start, d = this.end - 1;
  return Ue.t ? Ue.t(a, b, c, d, null) : Ue.call(null, a, b, c, d, null);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Uc, this.k);
};
g.O = function(a, b) {
  return Mc.a(this, b);
};
g.P = function(a, b, c) {
  return Mc.c(this, b, c);
};
g.La = function(a, b, c) {
  if ("number" === typeof b) {
    return Gb(this, b, c);
  }
  throw Error("Subvec's key for assoc must be a number.");
};
g.G = function() {
  var a = this;
  return function(b) {
    return function d(e) {
      return e === a.end ? null : I(x.a(a.Ka, e), new Gd(null, function() {
        return function() {
          return d(e + 1);
        };
      }(b), null, null));
    };
  }(this)(a.start);
};
g.H = function(a, b) {
  var c = this.Ka, d = this.start, e = this.end, f = this.n;
  return Ue.t ? Ue.t(b, c, d, e, f) : Ue.call(null, b, c, d, e, f);
};
g.I = function(a, b) {
  var c = this.k, d = Gb(this.Ka, this.end, b), e = this.start, f = this.end + 1;
  return Ue.t ? Ue.t(c, d, e, f, null) : Ue.call(null, c, d, e, f, null);
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.C(null, c);
      case 3:
        return this.Z(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.C(null, c);
  };
  a.c = function(a, c, d) {
    return this.Z(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.C(null, a);
};
g.a = function(a, b) {
  return this.Z(null, a, b);
};
function Ue(a, b, c, d, e) {
  for (;;) {
    if (b instanceof Te) {
      c = b.start + c, d = b.start + d, b = b.Ka;
    } else {
      var f = J(b);
      if (0 > c || 0 > d || c > f || d > f) {
        throw Error("Index out of bounds");
      }
      return new Te(a, b, c, d, e);
    }
  }
}
var Se = function() {
  function a(a, b, c) {
    return Ue(null, a, b, c, null);
  }
  function b(a, b) {
    return c.c(a, b, J(a));
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function Ve(a, b) {
  return a === b.A ? b : new xe(a, Za(b.d));
}
function Me(a) {
  return new xe({}, Za(a.d));
}
function Ne(a) {
  var b = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
  jd(a, 0, b, 0, a.length);
  return b;
}
var Xe = function We(b, c, d, e) {
  d = Ve(b.root.A, d);
  var f = b.g - 1 >>> c & 31;
  if (5 === c) {
    b = e;
  } else {
    var h = d.d[f];
    b = null != h ? We(b, c - 5, h, e) : Be(b.root.A, c - 5, e);
  }
  d.d[f] = b;
  return d;
};
function Le(a, b, c, d) {
  this.g = a;
  this.shift = b;
  this.root = c;
  this.X = d;
  this.i = 275;
  this.p = 88;
}
g = Le.prototype;
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return "number" === typeof b ? x.c(this, b, c) : c;
};
g.C = function(a, b) {
  if (this.root.A) {
    return Ge(this, b)[b & 31];
  }
  throw Error("nth after persistent!");
};
g.Z = function(a, b, c) {
  return 0 <= b && b < this.g ? x.a(this, b) : c;
};
g.L = function() {
  if (this.root.A) {
    return this.g;
  }
  throw Error("count after persistent!");
};
g.Xc = function(a, b, c) {
  var d = this;
  if (d.root.A) {
    if (0 <= b && b < d.g) {
      return Ae(this) <= b ? d.X[b & 31] = c : (a = function() {
        return function f(a, m) {
          var n = Ve(d.root.A, m);
          if (0 === a) {
            n.d[b & 31] = c;
          } else {
            var p = b >>> a & 31, s = f(a - 5, n.d[p]);
            n.d[p] = s;
          }
          return n;
        };
      }(this).call(null, d.shift, d.root), d.root = a), this;
    }
    if (b === d.g) {
      return cc(this, c);
    }
    throw Error("Index " + v.b(b) + " out of bounds for TransientVector of length" + v.b(d.g));
  }
  throw Error("assoc! after persistent!");
};
g.xb = function(a, b, c) {
  if ("number" === typeof b) {
    return fc(this, b, c);
  }
  throw Error("TransientVector's key for assoc! must be a number.");
};
g.yb = function(a, b) {
  if (this.root.A) {
    if (32 > this.g - Ae(this)) {
      this.X[this.g & 31] = b;
    } else {
      var c = new xe(this.root.A, this.X), d = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      d[0] = b;
      this.X = d;
      if (this.g >>> 5 > 1 << this.shift) {
        var d = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null], e = this.shift + 5;
        d[0] = this.root;
        d[1] = Be(this.root.A, this.shift, c);
        this.root = new xe(this.root.A, d);
        this.shift = e;
      } else {
        this.root = Xe(this, this.shift, this.root, c);
      }
    }
    this.g += 1;
    return this;
  }
  throw Error("conj! after persistent!");
};
g.zb = function() {
  if (this.root.A) {
    this.root.A = null;
    var a = this.g - Ae(this), b = Array(a);
    jd(this.X, 0, b, 0, a);
    return new T(null, this.g, this.shift, this.root, b, null);
  }
  throw Error("persistent! called twice");
};
function Ye() {
  this.p = 0;
  this.i = 2097152;
}
Ye.prototype.u = function() {
  return!1;
};
var Ze = new Ye;
function $e(a, b) {
  return nd(fd(b) ? J(a) === J(b) ? Zd(td, he.a(function(a) {
    return Ac.a(M.c(b, A(a), Ze), Tc(a));
  }, a)) : null : null);
}
function af(a, b) {
  var c = a.d;
  if (b instanceof P) {
    a: {
      for (var d = c.length, e = b.da, f = 0;;) {
        if (d <= f) {
          c = -1;
          break a;
        }
        var h = c[f];
        if (h instanceof P && e === h.da) {
          c = f;
          break a;
        }
        f += 2;
      }
      c = void 0;
    }
  } else {
    if (d = ba(b), q(q(d) ? d : "number" === typeof b)) {
      a: {
        d = c.length;
        for (e = 0;;) {
          if (d <= e) {
            c = -1;
            break a;
          }
          if (b === c[e]) {
            c = e;
            break a;
          }
          e += 2;
        }
        c = void 0;
      }
    } else {
      if (b instanceof Cc) {
        a: {
          d = c.length;
          e = b.Qa;
          for (f = 0;;) {
            if (d <= f) {
              c = -1;
              break a;
            }
            h = c[f];
            if (h instanceof Cc && e === h.Qa) {
              c = f;
              break a;
            }
            f += 2;
          }
          c = void 0;
        }
      } else {
        if (null == b) {
          a: {
            d = c.length;
            for (e = 0;;) {
              if (d <= e) {
                c = -1;
                break a;
              }
              if (null == c[e]) {
                c = e;
                break a;
              }
              e += 2;
            }
            c = void 0;
          }
        } else {
          a: {
            d = c.length;
            for (e = 0;;) {
              if (d <= e) {
                c = -1;
                break a;
              }
              if (Ac.a(b, c[e])) {
                c = e;
                break a;
              }
              e += 2;
            }
            c = void 0;
          }
        }
      }
    }
  }
  return c;
}
function bf(a, b, c) {
  this.d = a;
  this.q = b;
  this.Y = c;
  this.p = 0;
  this.i = 32374990;
}
g = bf.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.Y;
};
g.aa = function() {
  return this.q < this.d.length - 2 ? new bf(this.d, this.q + 2, this.Y) : null;
};
g.L = function() {
  return(this.d.length - this.q) / 2;
};
g.w = function() {
  return Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.Y);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return new T(null, 2, 5, U, [this.d[this.q], this.d[this.q + 1]], null);
};
g.$ = function() {
  return this.q < this.d.length - 2 ? new bf(this.d, this.q + 2, this.Y) : Fc;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new bf(this.d, this.q, b);
};
g.I = function(a, b) {
  return I(b, this);
};
function l(a, b, c, d) {
  this.k = a;
  this.g = b;
  this.d = c;
  this.n = d;
  this.i = 16647951;
  this.p = 8196;
}
g = l.prototype;
g.toString = function() {
  return oc(this);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  a = af(this, b);
  return-1 === a ? c : this.d[a + 1];
};
g.tb = function(a, b, c) {
  a = this.d.length;
  for (var d = 0;;) {
    if (d < a) {
      var e = this.d[d], f = this.d[d + 1];
      c = b.c ? b.c(c, e, f) : b.call(null, c, e, f);
      if (Lc(c)) {
        return b = c, G.b ? G.b(b) : G.call(null, b);
      }
      d += 2;
    } else {
      return c;
    }
  }
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return this.g;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Ic(this);
};
g.u = function(a, b) {
  return $e(this, b);
};
g.cb = function() {
  return new cf({}, this.d.length, Za(this.d));
};
g.J = function() {
  return Mb(df, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.Vb = function(a, b) {
  if (0 <= af(this, b)) {
    var c = this.d.length, d = c - 2;
    if (0 === d) {
      return jb(this);
    }
    for (var d = Array(d), e = 0, f = 0;;) {
      if (e >= c) {
        return new l(this.k, this.g - 1, d, null);
      }
      Ac.a(b, this.d[e]) || (d[f] = this.d[e], d[f + 1] = this.d[e + 1], f += 2);
      e += 2;
    }
  } else {
    return this;
  }
};
g.La = function(a, b, c) {
  a = af(this, b);
  if (-1 === a) {
    if (this.g < ef) {
      a = this.d;
      for (var d = a.length, e = Array(d + 2), f = 0;;) {
        if (f < d) {
          e[f] = a[f], f += 1;
        } else {
          break;
        }
      }
      e[d] = b;
      e[d + 1] = c;
      return new l(this.k, this.g + 1, e, null);
    }
    return Mb(vb(se.a(ff, this), b, c), this.k);
  }
  if (c === this.d[a + 1]) {
    return this;
  }
  b = Za(this.d);
  b[a + 1] = c;
  return new l(this.k, this.g, b, null);
};
g.Rb = function(a, b) {
  return-1 !== af(this, b);
};
g.G = function() {
  var a = this.d;
  return 0 <= a.length - 2 ? new bf(a, 0, null) : null;
};
g.H = function(a, b) {
  return new l(b, this.g, this.d, this.n);
};
g.I = function(a, b) {
  if (gd(b)) {
    return vb(this, x.a(b, 0), x.a(b, 1));
  }
  for (var c = this, d = y(b);;) {
    if (null == d) {
      return c;
    }
    var e = A(d);
    if (gd(e)) {
      c = vb(c, x.a(e, 0), x.a(e, 1)), d = D(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
var df = new l(null, 0, [], null), ef = 8;
function cf(a, b, c) {
  this.eb = a;
  this.jb = b;
  this.d = c;
  this.p = 56;
  this.i = 258;
}
g = cf.prototype;
g.xb = function(a, b, c) {
  var d = this;
  if (q(d.eb)) {
    a = af(this, b);
    if (-1 === a) {
      return d.jb + 2 <= 2 * ef ? (d.jb += 2, d.d.push(b), d.d.push(c), this) : Wd.c(function() {
        var a = d.jb, b = d.d;
        return gf.a ? gf.a(a, b) : gf.call(null, a, b);
      }(), b, c);
    }
    c !== d.d[a + 1] && (d.d[a + 1] = c);
    return this;
  }
  throw Error("assoc! after persistent!");
};
g.yb = function(a, b) {
  if (q(this.eb)) {
    if (b ? b.i & 2048 || b.zd || (b.i ? 0 : r(yb, b)) : r(yb, b)) {
      return ec(this, hf.b ? hf.b(b) : hf.call(null, b), jf.b ? jf.b(b) : jf.call(null, b));
    }
    for (var c = y(b), d = this;;) {
      var e = A(c);
      if (q(e)) {
        var f = e, c = D(c), d = ec(d, function() {
          var a = f;
          return hf.b ? hf.b(a) : hf.call(null, a);
        }(), function() {
          var a = f;
          return jf.b ? jf.b(a) : jf.call(null, a);
        }())
      } else {
        return d;
      }
    }
  } else {
    throw Error("conj! after persistent!");
  }
};
g.zb = function() {
  if (q(this.eb)) {
    return this.eb = !1, new l(null, xd(this.jb, 2), this.d, null);
  }
  throw Error("persistent! called twice");
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  if (q(this.eb)) {
    return a = af(this, b), -1 === a ? c : this.d[a + 1];
  }
  throw Error("lookup after persistent!");
};
g.L = function() {
  if (q(this.eb)) {
    return xd(this.jb, 2);
  }
  throw Error("count after persistent!");
};
function gf(a, b) {
  for (var c = bc(ff), d = 0;;) {
    if (d < a) {
      c = Wd.c(c, b[d], b[d + 1]), d += 2;
    } else {
      return c;
    }
  }
}
function kf() {
  this.l = !1;
}
function mf(a, b) {
  return a === b ? !0 : a === b || a instanceof P && b instanceof P && a.da === b.da ? !0 : Ac.a(a, b);
}
var nf = function() {
  function a(a, b, c, h, m) {
    a = Za(a);
    a[b] = c;
    a[h] = m;
    return a;
  }
  function b(a, b, c) {
    a = Za(a);
    a[b] = c;
    return a;
  }
  var c = null, c = function(c, e, f, h, m) {
    switch(arguments.length) {
      case 3:
        return b.call(this, c, e, f);
      case 5:
        return a.call(this, c, e, f, h, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.c = b;
  c.t = a;
  return c;
}();
function of(a, b) {
  var c = Array(a.length - 2);
  jd(a, 0, c, 0, 2 * b);
  jd(a, 2 * (b + 1), c, 2 * b, c.length - 2 * b);
  return c;
}
var pf = function() {
  function a(a, b, c, h, m, n) {
    a = a.fb(b);
    a.d[c] = h;
    a.d[m] = n;
    return a;
  }
  function b(a, b, c, h) {
    a = a.fb(b);
    a.d[c] = h;
    return a;
  }
  var c = null, c = function(c, e, f, h, m, n) {
    switch(arguments.length) {
      case 4:
        return b.call(this, c, e, f, h);
      case 6:
        return a.call(this, c, e, f, h, m, n);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.j = b;
  c.U = a;
  return c;
}();
function qf(a, b, c) {
  for (var d = a.length, e = 0, f = c;;) {
    if (e < d) {
      c = a[e];
      if (null != c) {
        var h = a[e + 1];
        c = b.c ? b.c(f, c, h) : b.call(null, f, c, h);
      } else {
        c = a[e + 1], c = null != c ? c.Xa(b, f) : f;
      }
      if (Lc(c)) {
        return a = c, G.b ? G.b(a) : G.call(null, a);
      }
      e += 2;
      f = c;
    } else {
      return f;
    }
  }
}
function rf(a, b, c) {
  this.A = a;
  this.B = b;
  this.d = c;
}
g = rf.prototype;
g.fb = function(a) {
  if (a === this.A) {
    return this;
  }
  var b = yd(this.B), c = Array(0 > b ? 4 : 2 * (b + 1));
  jd(this.d, 0, c, 0, 2 * b);
  return new rf(a, this.B, c);
};
g.Db = function() {
  var a = this.d;
  return sf.b ? sf.b(a) : sf.call(null, a);
};
g.Xa = function(a, b) {
  return qf(this.d, a, b);
};
g.Oa = function(a, b, c, d) {
  var e = 1 << (b >>> a & 31);
  if (0 === (this.B & e)) {
    return d;
  }
  var f = yd(this.B & e - 1), e = this.d[2 * f], f = this.d[2 * f + 1];
  return null == e ? f.Oa(a + 5, b, c, d) : mf(c, e) ? f : d;
};
g.la = function(a, b, c, d, e, f) {
  var h = 1 << (c >>> b & 31), m = yd(this.B & h - 1);
  if (0 === (this.B & h)) {
    var n = yd(this.B);
    if (2 * n < this.d.length) {
      var p = this.fb(a), s = p.d;
      f.l = !0;
      kd(s, 2 * m, s, 2 * (m + 1), 2 * (n - m));
      s[2 * m] = d;
      s[2 * m + 1] = e;
      p.B |= h;
      return p;
    }
    if (16 <= n) {
      h = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      h[c >>> b & 31] = tf.la(a, b + 5, c, d, e, f);
      for (p = m = 0;;) {
        if (32 > m) {
          0 !== (this.B >>> m & 1) && (h[m] = null != this.d[p] ? tf.la(a, b + 5, xc(this.d[p]), this.d[p], this.d[p + 1], f) : this.d[p + 1], p += 2), m += 1;
        } else {
          break;
        }
      }
      return new uf(a, n + 1, h);
    }
    s = Array(2 * (n + 4));
    jd(this.d, 0, s, 0, 2 * m);
    s[2 * m] = d;
    s[2 * m + 1] = e;
    jd(this.d, 2 * m, s, 2 * (m + 1), 2 * (n - m));
    f.l = !0;
    p = this.fb(a);
    p.d = s;
    p.B |= h;
    return p;
  }
  var t = this.d[2 * m], w = this.d[2 * m + 1];
  if (null == t) {
    return n = w.la(a, b + 5, c, d, e, f), n === w ? this : pf.j(this, a, 2 * m + 1, n);
  }
  if (mf(d, t)) {
    return e === w ? this : pf.j(this, a, 2 * m + 1, e);
  }
  f.l = !0;
  return pf.U(this, a, 2 * m, null, 2 * m + 1, function() {
    var f = b + 5;
    return vf.ga ? vf.ga(a, f, t, w, c, d, e) : vf.call(null, a, f, t, w, c, d, e);
  }());
};
g.ka = function(a, b, c, d, e) {
  var f = 1 << (b >>> a & 31), h = yd(this.B & f - 1);
  if (0 === (this.B & f)) {
    var m = yd(this.B);
    if (16 <= m) {
      f = [null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null];
      f[b >>> a & 31] = tf.ka(a + 5, b, c, d, e);
      for (var n = h = 0;;) {
        if (32 > h) {
          0 !== (this.B >>> h & 1) && (f[h] = null != this.d[n] ? tf.ka(a + 5, xc(this.d[n]), this.d[n], this.d[n + 1], e) : this.d[n + 1], n += 2), h += 1;
        } else {
          break;
        }
      }
      return new uf(null, m + 1, f);
    }
    n = Array(2 * (m + 1));
    jd(this.d, 0, n, 0, 2 * h);
    n[2 * h] = c;
    n[2 * h + 1] = d;
    jd(this.d, 2 * h, n, 2 * (h + 1), 2 * (m - h));
    e.l = !0;
    return new rf(null, this.B | f, n);
  }
  var p = this.d[2 * h], s = this.d[2 * h + 1];
  if (null == p) {
    return m = s.ka(a + 5, b, c, d, e), m === s ? this : new rf(null, this.B, nf.c(this.d, 2 * h + 1, m));
  }
  if (mf(c, p)) {
    return d === s ? this : new rf(null, this.B, nf.c(this.d, 2 * h + 1, d));
  }
  e.l = !0;
  return new rf(null, this.B, nf.t(this.d, 2 * h, null, 2 * h + 1, function() {
    var e = a + 5;
    return vf.U ? vf.U(e, p, s, b, c, d) : vf.call(null, e, p, s, b, c, d);
  }()));
};
g.Eb = function(a, b, c) {
  var d = 1 << (b >>> a & 31);
  if (0 === (this.B & d)) {
    return this;
  }
  var e = yd(this.B & d - 1), f = this.d[2 * e], h = this.d[2 * e + 1];
  return null == f ? (a = h.Eb(a + 5, b, c), a === h ? this : null != a ? new rf(null, this.B, nf.c(this.d, 2 * e + 1, a)) : this.B === d ? null : new rf(null, this.B ^ d, of(this.d, e))) : mf(c, f) ? new rf(null, this.B ^ d, of(this.d, e)) : this;
};
var tf = new rf(null, 0, []);
function uf(a, b, c) {
  this.A = a;
  this.g = b;
  this.d = c;
}
g = uf.prototype;
g.fb = function(a) {
  return a === this.A ? this : new uf(a, this.g, Za(this.d));
};
g.Db = function() {
  var a = this.d;
  return wf.b ? wf.b(a) : wf.call(null, a);
};
g.Xa = function(a, b) {
  for (var c = this.d.length, d = 0, e = b;;) {
    if (d < c) {
      var f = this.d[d];
      if (null != f && (e = f.Xa(a, e), Lc(e))) {
        return c = e, G.b ? G.b(c) : G.call(null, c);
      }
      d += 1;
    } else {
      return e;
    }
  }
};
g.Oa = function(a, b, c, d) {
  var e = this.d[b >>> a & 31];
  return null != e ? e.Oa(a + 5, b, c, d) : d;
};
g.la = function(a, b, c, d, e, f) {
  var h = c >>> b & 31, m = this.d[h];
  if (null == m) {
    return a = pf.j(this, a, h, tf.la(a, b + 5, c, d, e, f)), a.g += 1, a;
  }
  b = m.la(a, b + 5, c, d, e, f);
  return b === m ? this : pf.j(this, a, h, b);
};
g.ka = function(a, b, c, d, e) {
  var f = b >>> a & 31, h = this.d[f];
  if (null == h) {
    return new uf(null, this.g + 1, nf.c(this.d, f, tf.ka(a + 5, b, c, d, e)));
  }
  a = h.ka(a + 5, b, c, d, e);
  return a === h ? this : new uf(null, this.g, nf.c(this.d, f, a));
};
g.Eb = function(a, b, c) {
  var d = b >>> a & 31, e = this.d[d];
  if (null != e) {
    a = e.Eb(a + 5, b, c);
    if (a === e) {
      d = this;
    } else {
      if (null == a) {
        if (8 >= this.g) {
          a: {
            e = this.d;
            a = e.length;
            b = Array(2 * (this.g - 1));
            c = 0;
            for (var f = 1, h = 0;;) {
              if (c < a) {
                c !== d && null != e[c] && (b[f] = e[c], f += 2, h |= 1 << c), c += 1;
              } else {
                d = new rf(null, h, b);
                break a;
              }
            }
            d = void 0;
          }
        } else {
          d = new uf(null, this.g - 1, nf.c(this.d, d, a));
        }
      } else {
        d = new uf(null, this.g, nf.c(this.d, d, a));
      }
    }
    return d;
  }
  return this;
};
function xf(a, b, c) {
  b *= 2;
  for (var d = 0;;) {
    if (d < b) {
      if (mf(c, a[d])) {
        return d;
      }
      d += 2;
    } else {
      return-1;
    }
  }
}
function yf(a, b, c, d) {
  this.A = a;
  this.Fa = b;
  this.g = c;
  this.d = d;
}
g = yf.prototype;
g.fb = function(a) {
  if (a === this.A) {
    return this;
  }
  var b = Array(2 * (this.g + 1));
  jd(this.d, 0, b, 0, 2 * this.g);
  return new yf(a, this.Fa, this.g, b);
};
g.Db = function() {
  var a = this.d;
  return sf.b ? sf.b(a) : sf.call(null, a);
};
g.Xa = function(a, b) {
  return qf(this.d, a, b);
};
g.Oa = function(a, b, c, d) {
  a = xf(this.d, this.g, c);
  return 0 > a ? d : mf(c, this.d[a]) ? this.d[a + 1] : d;
};
g.la = function(a, b, c, d, e, f) {
  if (c === this.Fa) {
    b = xf(this.d, this.g, d);
    if (-1 === b) {
      if (this.d.length > 2 * this.g) {
        return a = pf.U(this, a, 2 * this.g, d, 2 * this.g + 1, e), f.l = !0, a.g += 1, a;
      }
      c = this.d.length;
      b = Array(c + 2);
      jd(this.d, 0, b, 0, c);
      b[c] = d;
      b[c + 1] = e;
      f.l = !0;
      f = this.g + 1;
      a === this.A ? (this.d = b, this.g = f, a = this) : a = new yf(this.A, this.Fa, f, b);
      return a;
    }
    return this.d[b + 1] === e ? this : pf.j(this, a, b + 1, e);
  }
  return(new rf(a, 1 << (this.Fa >>> b & 31), [null, this, null, null])).la(a, b, c, d, e, f);
};
g.ka = function(a, b, c, d, e) {
  return b === this.Fa ? (a = xf(this.d, this.g, c), -1 === a ? (a = 2 * this.g, b = Array(a + 2), jd(this.d, 0, b, 0, a), b[a] = c, b[a + 1] = d, e.l = !0, new yf(null, this.Fa, this.g + 1, b)) : Ac.a(this.d[a], d) ? this : new yf(null, this.Fa, this.g, nf.c(this.d, a + 1, d))) : (new rf(null, 1 << (this.Fa >>> a & 31), [null, this])).ka(a, b, c, d, e);
};
g.Eb = function(a, b, c) {
  a = xf(this.d, this.g, c);
  return-1 === a ? this : 1 === this.g ? null : new yf(null, this.Fa, this.g - 1, of(this.d, xd(a, 2)));
};
var vf = function() {
  function a(a, b, c, h, m, n, p) {
    var s = xc(c);
    if (s === m) {
      return new yf(null, s, 2, [c, h, n, p]);
    }
    var t = new kf;
    return tf.la(a, b, s, c, h, t).la(a, b, m, n, p, t);
  }
  function b(a, b, c, h, m, n) {
    var p = xc(b);
    if (p === h) {
      return new yf(null, p, 2, [b, c, m, n]);
    }
    var s = new kf;
    return tf.ka(a, p, b, c, s).ka(a, h, m, n, s);
  }
  var c = null, c = function(c, e, f, h, m, n, p) {
    switch(arguments.length) {
      case 6:
        return b.call(this, c, e, f, h, m, n);
      case 7:
        return a.call(this, c, e, f, h, m, n, p);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.U = b;
  c.ga = a;
  return c;
}();
function zf(a, b, c, d, e) {
  this.k = a;
  this.Pa = b;
  this.q = c;
  this.K = d;
  this.n = e;
  this.p = 0;
  this.i = 32374860;
}
g = zf.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return null == this.K ? new T(null, 2, 5, U, [this.Pa[this.q], this.Pa[this.q + 1]], null) : A(this.K);
};
g.$ = function() {
  if (null == this.K) {
    var a = this.Pa, b = this.q + 2;
    return sf.c ? sf.c(a, b, null) : sf.call(null, a, b, null);
  }
  var a = this.Pa, b = this.q, c = D(this.K);
  return sf.c ? sf.c(a, b, c) : sf.call(null, a, b, c);
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new zf(b, this.Pa, this.q, this.K, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
var sf = function() {
  function a(a, b, c) {
    if (null == c) {
      for (c = a.length;;) {
        if (b < c) {
          if (null != a[b]) {
            return new zf(null, a, b, null, null);
          }
          var h = a[b + 1];
          if (q(h) && (h = h.Db(), q(h))) {
            return new zf(null, a, b + 2, h, null);
          }
          b += 2;
        } else {
          return null;
        }
      }
    } else {
      return new zf(null, a, b, c, null);
    }
  }
  function b(a) {
    return c.c(a, 0, null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.c = a;
  return c;
}();
function Af(a, b, c, d, e) {
  this.k = a;
  this.Pa = b;
  this.q = c;
  this.K = d;
  this.n = e;
  this.p = 0;
  this.i = 32374860;
}
g = Af.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return A(this.K);
};
g.$ = function() {
  var a = this.Pa, b = this.q, c = D(this.K);
  return wf.j ? wf.j(null, a, b, c) : wf.call(null, null, a, b, c);
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new Af(b, this.Pa, this.q, this.K, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
var wf = function() {
  function a(a, b, c, h) {
    if (null == h) {
      for (h = b.length;;) {
        if (c < h) {
          var m = b[c];
          if (q(m) && (m = m.Db(), q(m))) {
            return new Af(a, b, c + 1, m, null);
          }
          c += 1;
        } else {
          return null;
        }
      }
    } else {
      return new Af(a, b, c, h, null);
    }
  }
  function b(a) {
    return c.j(null, a, 0, null);
  }
  var c = null, c = function(c, e, f, h) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 4:
        return a.call(this, c, e, f, h);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.j = a;
  return c;
}();
function Bf(a, b, c, d, e, f) {
  this.k = a;
  this.g = b;
  this.root = c;
  this.V = d;
  this.ca = e;
  this.n = f;
  this.i = 16123663;
  this.p = 8196;
}
g = Bf.prototype;
g.toString = function() {
  return oc(this);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return null == b ? this.V ? this.ca : c : null == this.root ? c : this.root.Oa(0, xc(b), b, c);
};
g.tb = function(a, b, c) {
  this.V && (a = this.ca, c = b.c ? b.c(c, null, a) : b.call(null, c, null, a));
  return Lc(c) ? G.b ? G.b(c) : G.call(null, c) : null != this.root ? this.root.Xa(b, c) : c;
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return this.g;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Ic(this);
};
g.u = function(a, b) {
  return $e(this, b);
};
g.cb = function() {
  return new Cf({}, this.root, this.g, this.V, this.ca);
};
g.J = function() {
  return Mb(ff, this.k);
};
g.Vb = function(a, b) {
  if (null == b) {
    return this.V ? new Bf(this.k, this.g - 1, this.root, !1, null, null) : this;
  }
  if (null == this.root) {
    return this;
  }
  var c = this.root.Eb(0, xc(b), b);
  return c === this.root ? this : new Bf(this.k, this.g - 1, c, this.V, this.ca, null);
};
g.La = function(a, b, c) {
  if (null == b) {
    return this.V && c === this.ca ? this : new Bf(this.k, this.V ? this.g : this.g + 1, this.root, !0, c, null);
  }
  a = new kf;
  b = (null == this.root ? tf : this.root).ka(0, xc(b), b, c, a);
  return b === this.root ? this : new Bf(this.k, a.l ? this.g + 1 : this.g, b, this.V, this.ca, null);
};
g.Rb = function(a, b) {
  return null == b ? this.V : null == this.root ? !1 : this.root.Oa(0, xc(b), b, ld) !== ld;
};
g.G = function() {
  if (0 < this.g) {
    var a = null != this.root ? this.root.Db() : null;
    return this.V ? I(new T(null, 2, 5, U, [null, this.ca], null), a) : a;
  }
  return null;
};
g.H = function(a, b) {
  return new Bf(b, this.g, this.root, this.V, this.ca, this.n);
};
g.I = function(a, b) {
  if (gd(b)) {
    return vb(this, x.a(b, 0), x.a(b, 1));
  }
  for (var c = this, d = y(b);;) {
    if (null == d) {
      return c;
    }
    var e = A(d);
    if (gd(e)) {
      c = vb(c, x.a(e, 0), x.a(e, 1)), d = D(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
var ff = new Bf(null, 0, null, !1, null, 0);
function Xc(a, b) {
  for (var c = a.length, d = 0, e = bc(ff);;) {
    if (d < c) {
      var f = d + 1, e = e.xb(null, a[d], b[d]), d = f
    } else {
      return dc(e);
    }
  }
}
function Cf(a, b, c, d, e) {
  this.A = a;
  this.root = b;
  this.count = c;
  this.V = d;
  this.ca = e;
  this.p = 56;
  this.i = 258;
}
g = Cf.prototype;
g.xb = function(a, b, c) {
  return Df(this, b, c);
};
g.yb = function(a, b) {
  return Ef(this, b);
};
g.zb = function() {
  var a;
  if (this.A) {
    this.A = null, a = new Bf(null, this.count, this.root, this.V, this.ca, null);
  } else {
    throw Error("persistent! called twice");
  }
  return a;
};
g.r = function(a, b) {
  return null == b ? this.V ? this.ca : null : null == this.root ? null : this.root.Oa(0, xc(b), b);
};
g.s = function(a, b, c) {
  return null == b ? this.V ? this.ca : c : null == this.root ? c : this.root.Oa(0, xc(b), b, c);
};
g.L = function() {
  if (this.A) {
    return this.count;
  }
  throw Error("count after persistent!");
};
function Ef(a, b) {
  if (a.A) {
    if (b ? b.i & 2048 || b.zd || (b.i ? 0 : r(yb, b)) : r(yb, b)) {
      return Df(a, hf.b ? hf.b(b) : hf.call(null, b), jf.b ? jf.b(b) : jf.call(null, b));
    }
    for (var c = y(b), d = a;;) {
      var e = A(c);
      if (q(e)) {
        var f = e, c = D(c), d = Df(d, function() {
          var a = f;
          return hf.b ? hf.b(a) : hf.call(null, a);
        }(), function() {
          var a = f;
          return jf.b ? jf.b(a) : jf.call(null, a);
        }())
      } else {
        return d;
      }
    }
  } else {
    throw Error("conj! after persistent");
  }
}
function Df(a, b, c) {
  if (a.A) {
    if (null == b) {
      a.ca !== c && (a.ca = c), a.V || (a.count += 1, a.V = !0);
    } else {
      var d = new kf;
      b = (null == a.root ? tf : a.root).la(a.A, 0, xc(b), b, c, d);
      b !== a.root && (a.root = b);
      d.l && (a.count += 1);
    }
    return a;
  }
  throw Error("assoc! after persistent!");
}
function Ff(a, b, c) {
  for (var d = b;;) {
    if (null != a) {
      b = c ? a.left : a.right, d = Vc.a(d, a), a = b;
    } else {
      return d;
    }
  }
}
function Gf(a, b, c, d, e) {
  this.k = a;
  this.stack = b;
  this.Mb = c;
  this.g = d;
  this.n = e;
  this.p = 0;
  this.i = 32374862;
}
g = Gf.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return 0 > this.g ? J(D(this)) + 1 : this.g;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  var a = this.stack;
  return null == a ? null : Db(a);
};
g.$ = function() {
  var a = A(this.stack), a = Ff(this.Mb ? a.right : a.left, D(this.stack), this.Mb);
  return null != a ? new Gf(null, a, this.Mb, this.g - 1, null) : Fc;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new Gf(b, this.stack, this.Mb, this.g, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
function Hf(a, b, c, d) {
  return c instanceof V ? c.left instanceof V ? new V(c.key, c.l, c.left.ra(), new W(a, b, c.right, d, null), null) : c.right instanceof V ? new V(c.right.key, c.right.l, new W(c.key, c.l, c.left, c.right.left, null), new W(a, b, c.right.right, d, null), null) : new W(a, b, c, d, null) : new W(a, b, c, d, null);
}
function If(a, b, c, d) {
  return d instanceof V ? d.right instanceof V ? new V(d.key, d.l, new W(a, b, c, d.left, null), d.right.ra(), null) : d.left instanceof V ? new V(d.left.key, d.left.l, new W(a, b, c, d.left.left, null), new W(d.key, d.l, d.left.right, d.right, null), null) : new W(a, b, c, d, null) : new W(a, b, c, d, null);
}
function Jf(a, b, c, d) {
  if (c instanceof V) {
    return new V(a, b, c.ra(), d, null);
  }
  if (d instanceof W) {
    return If(a, b, c, d.Ib());
  }
  if (d instanceof V && d.left instanceof W) {
    return new V(d.left.key, d.left.l, new W(a, b, c, d.left.left, null), If(d.key, d.l, d.left.right, d.right.Ib()), null);
  }
  throw Error("red-black tree invariant violation");
}
var Lf = function Kf(b, c, d) {
  d = null != b.left ? Kf(b.left, c, d) : d;
  if (Lc(d)) {
    return G.b ? G.b(d) : G.call(null, d);
  }
  var e = b.key, f = b.l;
  d = c.c ? c.c(d, e, f) : c.call(null, d, e, f);
  if (Lc(d)) {
    return G.b ? G.b(d) : G.call(null, d);
  }
  b = null != b.right ? Kf(b.right, c, d) : d;
  return Lc(b) ? G.b ? G.b(b) : G.call(null, b) : b;
};
function W(a, b, c, d, e) {
  this.key = a;
  this.l = b;
  this.left = c;
  this.right = d;
  this.n = e;
  this.p = 0;
  this.i = 32402207;
}
g = W.prototype;
g.Qc = function(a) {
  return a.Sc(this);
};
g.Ib = function() {
  return new V(this.key, this.l, this.left, this.right, null);
};
g.ra = function() {
  return this;
};
g.Pc = function(a) {
  return a.Rc(this);
};
g.replace = function(a, b, c, d) {
  return new W(a, b, c, d, null);
};
g.Rc = function(a) {
  return new W(a.key, a.l, this, a.right, null);
};
g.Sc = function(a) {
  return new W(a.key, a.l, a.left, this, null);
};
g.Xa = function(a, b) {
  return Lf(this, a, b);
};
g.r = function(a, b) {
  return x.c(this, b, null);
};
g.s = function(a, b, c) {
  return x.c(this, b, c);
};
g.C = function(a, b) {
  return 0 === b ? this.key : 1 === b ? this.l : null;
};
g.Z = function(a, b, c) {
  return 0 === b ? this.key : 1 === b ? this.l : c;
};
g.Ua = function(a, b, c) {
  return(new T(null, 2, 5, U, [this.key, this.l], null)).Ua(null, b, c);
};
g.D = function() {
  return null;
};
g.L = function() {
  return 2;
};
g.ub = function() {
  return this.key;
};
g.vb = function() {
  return this.l;
};
g.Sa = function() {
  return this.l;
};
g.Ta = function() {
  return new T(null, 1, 5, U, [this.key], null);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return Uc;
};
g.O = function(a, b) {
  return Mc.a(this, b);
};
g.P = function(a, b, c) {
  return Mc.c(this, b, c);
};
g.La = function(a, b, c) {
  return N.c(new T(null, 2, 5, U, [this.key, this.l], null), b, c);
};
g.G = function() {
  return lb(lb(Fc, this.l), this.key);
};
g.H = function(a, b) {
  return ad(new T(null, 2, 5, U, [this.key, this.l], null), b);
};
g.I = function(a, b) {
  return new T(null, 3, 5, U, [this.key, this.l, b], null);
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
function V(a, b, c, d, e) {
  this.key = a;
  this.l = b;
  this.left = c;
  this.right = d;
  this.n = e;
  this.p = 0;
  this.i = 32402207;
}
g = V.prototype;
g.Qc = function(a) {
  return new V(this.key, this.l, this.left, a, null);
};
g.Ib = function() {
  throw Error("red-black tree invariant violation");
};
g.ra = function() {
  return new W(this.key, this.l, this.left, this.right, null);
};
g.Pc = function(a) {
  return new V(this.key, this.l, a, this.right, null);
};
g.replace = function(a, b, c, d) {
  return new V(a, b, c, d, null);
};
g.Rc = function(a) {
  return this.left instanceof V ? new V(this.key, this.l, this.left.ra(), new W(a.key, a.l, this.right, a.right, null), null) : this.right instanceof V ? new V(this.right.key, this.right.l, new W(this.key, this.l, this.left, this.right.left, null), new W(a.key, a.l, this.right.right, a.right, null), null) : new W(a.key, a.l, this, a.right, null);
};
g.Sc = function(a) {
  return this.right instanceof V ? new V(this.key, this.l, new W(a.key, a.l, a.left, this.left, null), this.right.ra(), null) : this.left instanceof V ? new V(this.left.key, this.left.l, new W(a.key, a.l, a.left, this.left.left, null), new W(this.key, this.l, this.left.right, this.right, null), null) : new W(a.key, a.l, a.left, this, null);
};
g.Xa = function(a, b) {
  return Lf(this, a, b);
};
g.r = function(a, b) {
  return x.c(this, b, null);
};
g.s = function(a, b, c) {
  return x.c(this, b, c);
};
g.C = function(a, b) {
  return 0 === b ? this.key : 1 === b ? this.l : null;
};
g.Z = function(a, b, c) {
  return 0 === b ? this.key : 1 === b ? this.l : c;
};
g.Ua = function(a, b, c) {
  return(new T(null, 2, 5, U, [this.key, this.l], null)).Ua(null, b, c);
};
g.D = function() {
  return null;
};
g.L = function() {
  return 2;
};
g.ub = function() {
  return this.key;
};
g.vb = function() {
  return this.l;
};
g.Sa = function() {
  return this.l;
};
g.Ta = function() {
  return new T(null, 1, 5, U, [this.key], null);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return Uc;
};
g.O = function(a, b) {
  return Mc.a(this, b);
};
g.P = function(a, b, c) {
  return Mc.c(this, b, c);
};
g.La = function(a, b, c) {
  return N.c(new T(null, 2, 5, U, [this.key, this.l], null), b, c);
};
g.G = function() {
  return lb(lb(Fc, this.l), this.key);
};
g.H = function(a, b) {
  return ad(new T(null, 2, 5, U, [this.key, this.l], null), b);
};
g.I = function(a, b) {
  return new T(null, 3, 5, U, [this.key, this.l, b], null);
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
var Nf = function Mf(b, c, d, e, f) {
  if (null == c) {
    return new V(d, e, null, null, null);
  }
  var h;
  h = c.key;
  h = b.a ? b.a(d, h) : b.call(null, d, h);
  if (0 === h) {
    return f[0] = c, null;
  }
  if (0 > h) {
    return b = Mf(b, c.left, d, e, f), null != b ? c.Pc(b) : null;
  }
  b = Mf(b, c.right, d, e, f);
  return null != b ? c.Qc(b) : null;
}, Pf = function Of(b, c) {
  if (null == b) {
    return c;
  }
  if (null == c) {
    return b;
  }
  if (b instanceof V) {
    if (c instanceof V) {
      var d = Of(b.right, c.left);
      return d instanceof V ? new V(d.key, d.l, new V(b.key, b.l, b.left, d.left, null), new V(c.key, c.l, d.right, c.right, null), null) : new V(b.key, b.l, b.left, new V(c.key, c.l, d, c.right, null), null);
    }
    return new V(b.key, b.l, b.left, Of(b.right, c), null);
  }
  if (c instanceof V) {
    return new V(c.key, c.l, Of(b, c.left), c.right, null);
  }
  d = Of(b.right, c.left);
  return d instanceof V ? new V(d.key, d.l, new W(b.key, b.l, b.left, d.left, null), new W(c.key, c.l, d.right, c.right, null), null) : Jf(b.key, b.l, b.left, new W(c.key, c.l, d, c.right, null));
}, Rf = function Qf(b, c, d, e) {
  if (null != c) {
    var f;
    f = c.key;
    f = b.a ? b.a(d, f) : b.call(null, d, f);
    if (0 === f) {
      return e[0] = c, Pf(c.left, c.right);
    }
    if (0 > f) {
      return b = Qf(b, c.left, d, e), null != b || null != e[0] ? c.left instanceof W ? Jf(c.key, c.l, b, c.right) : new V(c.key, c.l, b, c.right, null) : null;
    }
    b = Qf(b, c.right, d, e);
    if (null != b || null != e[0]) {
      if (c.right instanceof W) {
        if (e = c.key, d = c.l, c = c.left, b instanceof V) {
          c = new V(e, d, c, b.ra(), null);
        } else {
          if (c instanceof W) {
            c = Hf(e, d, c.Ib(), b);
          } else {
            if (c instanceof V && c.right instanceof W) {
              c = new V(c.right.key, c.right.l, Hf(c.key, c.l, c.left.Ib(), c.right.left), new W(e, d, c.right.right, b, null), null);
            } else {
              throw Error("red-black tree invariant violation");
            }
          }
        }
      } else {
        c = new V(c.key, c.l, c.left, b, null);
      }
    } else {
      c = null;
    }
    return c;
  }
  return null;
}, Tf = function Sf(b, c, d, e) {
  var f = c.key, h = b.a ? b.a(d, f) : b.call(null, d, f);
  return 0 === h ? c.replace(f, e, c.left, c.right) : 0 > h ? c.replace(f, c.l, Sf(b, c.left, d, e), c.right) : c.replace(f, c.l, c.left, Sf(b, c.right, d, e));
};
function Uf(a, b, c, d, e) {
  this.ja = a;
  this.Ra = b;
  this.g = c;
  this.k = d;
  this.n = e;
  this.i = 418776847;
  this.p = 8192;
}
g = Uf.prototype;
g.toString = function() {
  return oc(this);
};
function Vf(a, b) {
  for (var c = a.Ra;;) {
    if (null != c) {
      var d;
      d = c.key;
      d = a.ja.a ? a.ja.a(b, d) : a.ja.call(null, b, d);
      if (0 === d) {
        return c;
      }
      c = 0 > d ? c.left : c.right;
    } else {
      return null;
    }
  }
}
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  a = Vf(this, b);
  return null != a ? a.l : c;
};
g.tb = function(a, b, c) {
  return null != this.Ra ? Lf(this.Ra, b, c) : c;
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return this.g;
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Ic(this);
};
g.u = function(a, b) {
  return $e(this, b);
};
g.J = function() {
  return ad(Wf, this.k);
};
g.Vb = function(a, b) {
  var c = [null], d = Rf(this.ja, this.Ra, b, c);
  return null == d ? null == L.a(c, 0) ? this : new Uf(this.ja, null, 0, this.k, null) : new Uf(this.ja, d.ra(), this.g - 1, this.k, null);
};
g.La = function(a, b, c) {
  a = [null];
  var d = Nf(this.ja, this.Ra, b, c, a);
  return null == d ? (a = L.a(a, 0), Ac.a(c, a.l) ? this : new Uf(this.ja, Tf(this.ja, this.Ra, b, c), this.g, this.k, null)) : new Uf(this.ja, d.ra(), this.g + 1, this.k, null);
};
g.Rb = function(a, b) {
  return null != Vf(this, b);
};
g.G = function() {
  var a;
  0 < this.g ? (a = this.g, a = new Gf(null, Ff(this.Ra, null, !0), !0, a, null)) : a = null;
  return a;
};
g.H = function(a, b) {
  return new Uf(this.ja, this.Ra, this.g, b, this.n);
};
g.I = function(a, b) {
  if (gd(b)) {
    return vb(this, x.a(b, 0), x.a(b, 1));
  }
  for (var c = this, d = y(b);;) {
    if (null == d) {
      return c;
    }
    var e = A(d);
    if (gd(e)) {
      c = vb(c, x.a(e, 0), x.a(e, 1)), d = D(d);
    } else {
      throw Error("conj on a map takes map entries or seqables of map entries");
    }
  }
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
var Wf = new Uf(Bc, null, 0, null, 0), ee = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    a = y(a);
    for (var b = bc(ff);;) {
      if (a) {
        var e = D(D(a)), b = Wd.c(b, A(a), Tc(a));
        a = e;
      } else {
        return dc(b);
      }
    }
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}(), Xf = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    return new l(null, xd(J(a), 2), O.a(ab, a), null);
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}(), Yf = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    a = y(a);
    for (var b = Wf;;) {
      if (a) {
        var e = D(D(a)), b = N.c(b, A(a), Tc(a));
        a = e;
      } else {
        return b;
      }
    }
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function Zf(a, b) {
  this.W = a;
  this.Y = b;
  this.p = 0;
  this.i = 32374988;
}
g = Zf.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.Y;
};
g.aa = function() {
  var a = this.W, a = (a ? a.i & 128 || a.Wb || (a.i ? 0 : r(rb, a)) : r(rb, a)) ? this.W.aa(null) : D(this.W);
  return null == a ? null : new Zf(a, this.Y);
};
g.w = function() {
  return Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.Y);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return this.W.T(null).ub(null);
};
g.$ = function() {
  var a = this.W, a = (a ? a.i & 128 || a.Wb || (a.i ? 0 : r(rb, a)) : r(rb, a)) ? this.W.aa(null) : D(this.W);
  return null != a ? new Zf(a, this.Y) : Fc;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new Zf(this.W, b);
};
g.I = function(a, b) {
  return I(b, this);
};
function $f(a) {
  return(a = y(a)) ? new Zf(a, null) : null;
}
function hf(a) {
  return zb(a);
}
function ag(a, b) {
  this.W = a;
  this.Y = b;
  this.p = 0;
  this.i = 32374988;
}
g = ag.prototype;
g.toString = function() {
  return oc(this);
};
g.D = function() {
  return this.Y;
};
g.aa = function() {
  var a = this.W, a = (a ? a.i & 128 || a.Wb || (a.i ? 0 : r(rb, a)) : r(rb, a)) ? this.W.aa(null) : D(this.W);
  return null == a ? null : new ag(a, this.Y);
};
g.w = function() {
  return Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.Y);
};
g.O = function(a, b) {
  return rd.a(b, this);
};
g.P = function(a, b, c) {
  return rd.c(b, c, this);
};
g.T = function() {
  return this.W.T(null).vb(null);
};
g.$ = function() {
  var a = this.W, a = (a ? a.i & 128 || a.Wb || (a.i ? 0 : r(rb, a)) : r(rb, a)) ? this.W.aa(null) : D(this.W);
  return null != a ? new ag(a, this.Y) : Fc;
};
g.G = function() {
  return this;
};
g.H = function(a, b) {
  return new ag(this.W, b);
};
g.I = function(a, b) {
  return I(b, this);
};
function bg(a) {
  return(a = y(a)) ? new ag(a, null) : null;
}
function jf(a) {
  return Bb(a);
}
var cg = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    return q($d(a)) ? bb.a(function(a, b) {
      return Vc.a(q(a) ? a : df, b);
    }, a) : null;
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function dg(a, b, c) {
  this.k = a;
  this.hb = b;
  this.n = c;
  this.i = 15077647;
  this.p = 8196;
}
g = dg.prototype;
g.toString = function() {
  return oc(this);
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return ub(this.hb, b) ? b : c;
};
g.D = function() {
  return this.k;
};
g.L = function() {
  return ib(this.hb);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Ic(this);
};
g.u = function(a, b) {
  return ed(b) && J(this) === J(b) && Zd(function(a) {
    return function(b) {
      return pd(a, b);
    };
  }(this), b);
};
g.cb = function() {
  return new eg(bc(this.hb));
};
g.J = function() {
  return ad(fg, this.k);
};
g.G = function() {
  return $f(this.hb);
};
g.H = function(a, b) {
  return new dg(b, this.hb, this.n);
};
g.I = function(a, b) {
  return new dg(this.k, N.c(this.hb, b, null), null);
};
g.call = function() {
  var a = null, a = function(a, c, d) {
    switch(arguments.length) {
      case 2:
        return this.r(null, c);
      case 3:
        return this.s(null, c, d);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  a.a = function(a, c) {
    return this.r(null, c);
  };
  a.c = function(a, c, d) {
    return this.s(null, c, d);
  };
  return a;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return this.r(null, a);
};
g.a = function(a, b) {
  return this.s(null, a, b);
};
var fg = new dg(null, df, 0);
function eg(a) {
  this.Ja = a;
  this.i = 259;
  this.p = 136;
}
g = eg.prototype;
g.call = function() {
  function a(a, b, c) {
    return tb.c(this.Ja, b, ld) === ld ? c : b;
  }
  function b(a, b) {
    return tb.c(this.Ja, b, ld) === ld ? null : b;
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
g.apply = function(a, b) {
  return this.call.apply(this, [this].concat(Za(b)));
};
g.b = function(a) {
  return tb.c(this.Ja, a, ld) === ld ? null : a;
};
g.a = function(a, b) {
  return tb.c(this.Ja, a, ld) === ld ? b : a;
};
g.r = function(a, b) {
  return tb.c(this, b, null);
};
g.s = function(a, b, c) {
  return tb.c(this.Ja, b, ld) === ld ? c : b;
};
g.L = function() {
  return J(this.Ja);
};
g.yb = function(a, b) {
  this.Ja = Wd.c(this.Ja, b, null);
  return this;
};
g.zb = function() {
  return new dg(null, dc(this.Ja), null);
};
function Ed(a) {
  if (a && (a.p & 4096 || a.Bd)) {
    return a.name;
  }
  if ("string" === typeof a) {
    return a;
  }
  throw Error("Doesn't support name: " + v.b(a));
}
function gg(a, b, c, d, e) {
  this.k = a;
  this.start = b;
  this.end = c;
  this.step = d;
  this.n = e;
  this.i = 32375006;
  this.p = 8192;
}
g = gg.prototype;
g.toString = function() {
  return oc(this);
};
g.C = function(a, b) {
  if (b < ib(this)) {
    return this.start + b * this.step;
  }
  if (this.start > this.end && 0 === this.step) {
    return this.start;
  }
  throw Error("Index out of bounds");
};
g.Z = function(a, b, c) {
  return b < ib(this) ? this.start + b * this.step : this.start > this.end && 0 === this.step ? this.start : c;
};
g.D = function() {
  return this.k;
};
g.aa = function() {
  return 0 < this.step ? this.start + this.step < this.end ? new gg(this.k, this.start + this.step, this.end, this.step, null) : null : this.start + this.step > this.end ? new gg(this.k, this.start + this.step, this.end, this.step, null) : null;
};
g.L = function() {
  if (Ua(Ub(this))) {
    return 0;
  }
  var a = (this.end - this.start) / this.step;
  return Math.ceil.b ? Math.ceil.b(a) : Math.ceil.call(null, a);
};
g.w = function() {
  var a = this.n;
  return null != a ? a : this.n = a = Hc(this);
};
g.u = function(a, b) {
  return Rc(this, b);
};
g.J = function() {
  return ad(Fc, this.k);
};
g.O = function(a, b) {
  return Mc.a(this, b);
};
g.P = function(a, b, c) {
  return Mc.c(this, b, c);
};
g.T = function() {
  return null == Ub(this) ? null : this.start;
};
g.$ = function() {
  return null != Ub(this) ? new gg(this.k, this.start + this.step, this.end, this.step, null) : Fc;
};
g.G = function() {
  return 0 < this.step ? this.start < this.end ? this : null : this.start > this.end ? this : null;
};
g.H = function(a, b) {
  return new gg(b, this.start, this.end, this.step, this.n);
};
g.I = function(a, b) {
  return I(b, this);
};
var hg = function() {
  function a(a, b, c) {
    return new gg(null, a, b, c, null);
  }
  function b(a, b) {
    return e.c(a, b, 1);
  }
  function c(a) {
    return e.c(0, a, 1);
  }
  function d() {
    return e.c(0, Number.MAX_VALUE, 1);
  }
  var e = null, e = function(e, h, m) {
    switch(arguments.length) {
      case 0:
        return d.call(this);
      case 1:
        return c.call(this, e);
      case 2:
        return b.call(this, e, h);
      case 3:
        return a.call(this, e, h, m);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  e.o = d;
  e.b = c;
  e.a = b;
  e.c = a;
  return e;
}(), ig = function() {
  function a(a, b, f) {
    return I(b, new Gd(null, function() {
      var h = y(f);
      return h ? c.c(a, function() {
        var c = A(h);
        return a.a ? a.a(b, c) : a.call(null, b, c);
      }(), C(h)) : null;
    }, null, null));
  }
  function b(a, b) {
    return new Gd(null, function() {
      var f = y(b);
      return f ? c.c(a, A(f), C(f)) : lb(Fc, a.o ? a.o() : a.call(null));
    }, null, null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}(), jg = function() {
  function a(a, b) {
    for (;;) {
      if (y(b) && 0 < a) {
        var c = a - 1, h = D(b);
        a = c;
        b = h;
      } else {
        return null;
      }
    }
  }
  function b(a) {
    for (;;) {
      if (y(a)) {
        a = D(a);
      } else {
        return null;
      }
    }
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}(), kg = function() {
  function a(a, b) {
    jg.a(a, b);
    return b;
  }
  function b(a) {
    jg.b(a);
    return a;
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}();
function lg(a, b) {
  if ("string" === typeof b) {
    var c = a.exec(b);
    return null == c ? null : 1 === J(c) ? A(c) : Pe(c);
  }
  throw new TypeError("re-find must match against a string.");
}
var ng = function mg(b, c) {
  var d = lg(b, c), e = c.search(b), f = dd(d) ? A(d) : d, h = Ad.a(c, e + J(f));
  return q(d) ? new Gd(null, function(c, d, e, f) {
    return function() {
      return I(c, y(f) ? mg(b, f) : null);
    };
  }(d, e, f, h), null, null) : null;
};
function og(a) {
  var b = lg(/^(?:\(\?([idmsux]*)\))?(.*)/, a);
  L.c(b, 0, null);
  a = L.c(b, 1, null);
  b = L.c(b, 2, null);
  return new RegExp(b, a);
}
function pg(a, b, c, d, e, f, h) {
  var m = Ma;
  try {
    Ma = null == Ma ? null : Ma - 1;
    if (null != Ma && 0 > Ma) {
      return Wb(a, "#");
    }
    Wb(a, c);
    if (y(h)) {
      var n = A(h);
      b.c ? b.c(n, a, f) : b.call(null, n, a, f);
    }
    for (var p = D(h), s = Ta.b(f) - 1;;) {
      if (!p || null != s && 0 === s) {
        y(p) && 0 === s && (Wb(a, d), Wb(a, "..."));
        break;
      } else {
        Wb(a, d);
        var t = A(p);
        c = a;
        h = f;
        b.c ? b.c(t, c, h) : b.call(null, t, c, h);
        var w = D(p);
        c = s - 1;
        p = w;
        s = c;
      }
    }
    return Wb(a, e);
  } finally {
    Ma = m;
  }
}
var qg = function() {
  function a(a, d) {
    var e = null;
    1 < arguments.length && (e = E(Array.prototype.slice.call(arguments, 1), 0));
    return b.call(this, a, e);
  }
  function b(a, b) {
    for (var e = y(b), f = null, h = 0, m = 0;;) {
      if (m < h) {
        var n = f.C(null, m);
        Wb(a, n);
        m += 1;
      } else {
        if (e = y(e)) {
          f = e, hd(f) ? (e = hc(f), h = ic(f), f = e, n = J(e), e = h, h = n) : (n = A(f), Wb(a, n), e = D(f), f = null, h = 0), m = 0;
        } else {
          return null;
        }
      }
    }
  }
  a.m = 1;
  a.h = function(a) {
    var d = A(a);
    a = C(a);
    return b(d, a);
  };
  a.e = b;
  return a;
}(), rg = {'"':'\\"', "\\":"\\\\", "\b":"\\b", "\f":"\\f", "\n":"\\n", "\r":"\\r", "\t":"\\t"};
function sg(a) {
  return'"' + v.b(a.replace(RegExp('[\\\\"\b\f\n\r\t]', "g"), function(a) {
    return rg[a];
  })) + '"';
}
var vg = function tg(b, c, d) {
  if (null == b) {
    return Wb(c, "nil");
  }
  if (void 0 === b) {
    return Wb(c, "#\x3cundefined\x3e");
  }
  q(function() {
    var c = M.a(d, Ra);
    return q(c) ? (c = b ? b.i & 131072 || b.Ad ? !0 : b.i ? !1 : r(Jb, b) : r(Jb, b)) ? bd(b) : c : c;
  }()) && (Wb(c, "^"), tg(bd(b), c, d), Wb(c, " "));
  if (null == b) {
    return Wb(c, "nil");
  }
  if (b.Ed) {
    return b.de(b, c, d);
  }
  if (b && (b.i & 2147483648 || b.F)) {
    return b.v(null, c, d);
  }
  if (Va(b) === Boolean || "number" === typeof b) {
    return Wb(c, "" + v.b(b));
  }
  if (null != b && b.constructor === Object) {
    Wb(c, "#js ");
    var e = he.a(function(c) {
      return new T(null, 2, 5, U, [Fd.b(c), b[c]], null);
    }, id(b));
    return ug.j ? ug.j(e, tg, c, d) : ug.call(null, e, tg, c, d);
  }
  return b instanceof Array ? pg(c, tg, "#js [", " ", "]", d, b) : q(ba(b)) ? q(Qa.b(d)) ? Wb(c, sg(b)) : Wb(c, b) : Zc(b) ? qg.e(c, E(["#\x3c", "" + v.b(b), "\x3e"], 0)) : b instanceof Date ? (e = function(b, c) {
    for (var d = "" + v.b(b);;) {
      if (J(d) < c) {
        d = "0" + v.b(d);
      } else {
        return d;
      }
    }
  }, qg.e(c, E(['#inst "', "" + v.b(b.getUTCFullYear()), "-", e(b.getUTCMonth() + 1, 2), "-", e(b.getUTCDate(), 2), "T", e(b.getUTCHours(), 2), ":", e(b.getUTCMinutes(), 2), ":", e(b.getUTCSeconds(), 2), ".", e(b.getUTCMilliseconds(), 3), "-", '00:00"'], 0))) : b instanceof RegExp ? qg.e(c, E(['#"', b.source, '"'], 0)) : (b ? b.i & 2147483648 || b.F || (b.i ? 0 : r(Xb, b)) : r(Xb, b)) ? Yb(b, c, d) : qg.e(c, E(["#\x3c", "" + v.b(b), "\x3e"], 0));
};
function wg(a, b) {
  var c = new Fa;
  a: {
    var d = new nc(c);
    vg(A(a), d, b);
    for (var e = y(D(a)), f = null, h = 0, m = 0;;) {
      if (m < h) {
        var n = f.C(null, m);
        Wb(d, " ");
        vg(n, d, b);
        m += 1;
      } else {
        if (e = y(e)) {
          f = e, hd(f) ? (e = hc(f), h = ic(f), f = e, n = J(e), e = h, h = n) : (n = A(f), Wb(d, " "), vg(n, d, b), e = D(f), f = null, h = 0), m = 0;
        } else {
          break a;
        }
      }
    }
  }
  return c;
}
var xg = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    var b = Na();
    return cd(a) ? "" : "" + v.b(wg(a, b));
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function ug(a, b, c, d) {
  return pg(c, function(a, c, d) {
    var m = zb(a);
    b.c ? b.c(m, c, d) : b.call(null, m, c, d);
    Wb(c, " ");
    a = Bb(a);
    return b.c ? b.c(a, c, d) : b.call(null, a, c, d);
  }, "{", ", ", "}", d, y(a));
}
Ec.prototype.F = !0;
Ec.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Gd.prototype.F = !0;
Gd.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Gf.prototype.F = !0;
Gf.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
zf.prototype.F = !0;
zf.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
W.prototype.F = !0;
W.prototype.v = function(a, b, c) {
  return pg(b, vg, "[", " ", "]", c, this);
};
bf.prototype.F = !0;
bf.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Re.prototype.F = !0;
Re.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Dd.prototype.F = !0;
Dd.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Bf.prototype.F = !0;
Bf.prototype.v = function(a, b, c) {
  return ug(this, vg, b, c);
};
Af.prototype.F = !0;
Af.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Te.prototype.F = !0;
Te.prototype.v = function(a, b, c) {
  return pg(b, vg, "[", " ", "]", c, this);
};
Uf.prototype.F = !0;
Uf.prototype.v = function(a, b, c) {
  return ug(this, vg, b, c);
};
dg.prototype.F = !0;
dg.prototype.v = function(a, b, c) {
  return pg(b, vg, "#{", " ", "}", c, this);
};
Md.prototype.F = !0;
Md.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
de.prototype.F = !0;
de.prototype.v = function(a, b, c) {
  Wb(b, "#\x3cAtom: ");
  vg(this.state, b, c);
  return Wb(b, "\x3e");
};
ag.prototype.F = !0;
ag.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
V.prototype.F = !0;
V.prototype.v = function(a, b, c) {
  return pg(b, vg, "[", " ", "]", c, this);
};
T.prototype.F = !0;
T.prototype.v = function(a, b, c) {
  return pg(b, vg, "[", " ", "]", c, this);
};
Cd.prototype.F = !0;
Cd.prototype.v = function(a, b) {
  return Wb(b, "()");
};
l.prototype.F = !0;
l.prototype.v = function(a, b, c) {
  return ug(this, vg, b, c);
};
gg.prototype.F = !0;
gg.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Zf.prototype.F = !0;
Zf.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
Bd.prototype.F = !0;
Bd.prototype.v = function(a, b, c) {
  return pg(b, vg, "(", " ", ")", c, this);
};
T.prototype.Sb = !0;
T.prototype.Tb = function(a, b) {
  return qd.a(this, b);
};
Te.prototype.Sb = !0;
Te.prototype.Tb = function(a, b) {
  return qd.a(this, b);
};
P.prototype.Sb = !0;
P.prototype.Tb = function(a, b) {
  return zc(this, b);
};
Cc.prototype.Sb = !0;
Cc.prototype.Tb = function(a, b) {
  return zc(this, b);
};
function yg(a, b, c) {
  $b(a, b, c);
}
var Ag = null, Bg = function() {
  function a(a) {
    null == Ag && (Ag = ge.b ? ge.b(0) : ge.call(null, 0));
    return Dc.b("" + v.b(a) + v.b(S.a(Ag, Jc)));
  }
  function b() {
    return c.b("G__");
  }
  var c = null, c = function(c) {
    switch(arguments.length) {
      case 0:
        return b.call(this);
      case 1:
        return a.call(this, c);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.o = b;
  c.b = a;
  return c;
}(), Cg = {};
function Dg(a) {
  if (a ? a.xd : a) {
    return a.xd(a);
  }
  var b;
  b = Dg[k(null == a ? null : a)];
  if (!b && (b = Dg._, !b)) {
    throw u("IEncodeJS.-clj-\x3ejs", a);
  }
  return b.call(null, a);
}
function Eg(a) {
  return(a ? q(q(null) ? null : a.wd) || (a.Dc ? 0 : r(Cg, a)) : r(Cg, a)) ? Dg(a) : "string" === typeof a || "number" === typeof a || a instanceof P || a instanceof Cc ? Fg.b ? Fg.b(a) : Fg.call(null, a) : xg.e(E([a], 0));
}
var Fg = function Gg(b) {
  if (null == b) {
    return null;
  }
  if (b ? q(q(null) ? null : b.wd) || (b.Dc ? 0 : r(Cg, b)) : r(Cg, b)) {
    return Dg(b);
  }
  if (b instanceof P) {
    return Ed(b);
  }
  if (b instanceof Cc) {
    return "" + v.b(b);
  }
  if (fd(b)) {
    var c = {};
    b = y(b);
    for (var d = null, e = 0, f = 0;;) {
      if (f < e) {
        var h = d.C(null, f), m = L.c(h, 0, null), h = L.c(h, 1, null);
        c[Eg(m)] = Gg(h);
        f += 1;
      } else {
        if (b = y(b)) {
          hd(b) ? (e = hc(b), b = ic(b), d = e, e = J(e)) : (e = A(b), d = L.c(e, 0, null), e = L.c(e, 1, null), c[Eg(d)] = Gg(e), b = D(b), d = null, e = 0), f = 0;
        } else {
          break;
        }
      }
    }
    return c;
  }
  if (dd(b)) {
    c = [];
    b = y(he.a(Gg, b));
    d = null;
    for (f = e = 0;;) {
      if (f < e) {
        m = d.C(null, f), c.push(m), f += 1;
      } else {
        if (b = y(b)) {
          d = b, hd(d) ? (b = hc(d), f = ic(d), d = b, e = J(b), b = f) : (b = A(d), c.push(b), b = D(d), d = null, e = 0), f = 0;
        } else {
          break;
        }
      }
    }
    return c;
  }
  return b;
}, Hg = {};
function Ig(a, b) {
  if (a ? a.vd : a) {
    return a.vd(a, b);
  }
  var c;
  c = Ig[k(null == a ? null : a)];
  if (!c && (c = Ig._, !c)) {
    throw u("IEncodeClojure.-js-\x3eclj", a);
  }
  return c.call(null, a, b);
}
var Kg = function() {
  function a(a) {
    return b.e(a, E([new l(null, 1, [Jg, !1], null)], 0));
  }
  var b = null, c = function() {
    function a(c, d) {
      var m = null;
      1 < arguments.length && (m = E(Array.prototype.slice.call(arguments, 1), 0));
      return b.call(this, c, m);
    }
    function b(a, c) {
      if (a ? q(q(null) ? null : a.Vd) || (a.Dc ? 0 : r(Hg, a)) : r(Hg, a)) {
        return Ig(a, O.a(Xf, c));
      }
      if (y(c)) {
        var d = md(c) ? O.a(ee, c) : c, e = M.a(d, Jg);
        return function(a, b, c, d) {
          return function z(e) {
            return md(e) ? kg.b(he.a(z, e)) : dd(e) ? se.a(null == e ? null : jb(e), he.a(z, e)) : e instanceof Array ? Pe(he.a(z, e)) : Va(e) === Object ? se.a(df, function() {
              return function(a, b, c, d) {
                return function Xa(f) {
                  return new Gd(null, function(a, b, c, d) {
                    return function() {
                      for (;;) {
                        var a = y(f);
                        if (a) {
                          if (hd(a)) {
                            var b = hc(a), c = J(b), h = Kd(c);
                            return function() {
                              for (var a = 0;;) {
                                if (a < c) {
                                  var f = x.a(b, a), m = h, n = U, p;
                                  p = f;
                                  p = d.b ? d.b(p) : d.call(null, p);
                                  f = new T(null, 2, 5, n, [p, z(e[f])], null);
                                  m.add(f);
                                  a += 1;
                                } else {
                                  return!0;
                                }
                              }
                            }() ? Nd(h.N(), Xa(ic(a))) : Nd(h.N(), null);
                          }
                          var m = A(a);
                          return I(new T(null, 2, 5, U, [function() {
                            var a = m;
                            return d.b ? d.b(a) : d.call(null, a);
                          }(), z(e[m])], null), Xa(C(a)));
                        }
                        return null;
                      }
                    };
                  }(a, b, c, d), null, null);
                };
              }(a, b, c, d)(id(e));
            }()) : e;
          };
        }(c, d, e, q(e) ? Fd : v)(a);
      }
      return null;
    }
    a.m = 1;
    a.h = function(a) {
      var c = A(a);
      a = C(a);
      return b(c, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e) {
    switch(arguments.length) {
      case 1:
        return a.call(this, b);
      default:
        return c.e(b, E(arguments, 1));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 1;
  b.h = c.h;
  b.b = a;
  b.e = c.e;
  return b;
}();
var Lg = new P(null, "div.simple-demo", "div.simple-demo", -1263176704), Mg = new P(null, "y", "y", -1757859776), Ng = new P(null, "css-infiles", "css-infiles", 1248716896), Og = new P(null, "div.example-clock", "div.example-clock", 1559289088), Pg = new P(null, "rel", "rel", 1378823488), Qg = new P(null, "div.custom", "div.custom", 1900136768), Rg = new P(null, "clock-state", "clock-state", -1232687680), Sg = new P(null, "column-pair", "column-pair", 1374695904), Tg = new P(null, "font-style", "font-style", 
-773672352), Ug = new P(null, "click-count", "click-count", -1016274336), Vg = new P(null, "open", "open", -1763596448), Wg = new P(null, "random-colors", "random-colors", 1337434241), Xg = new P(null, "p2", "p2", 905500641), Yg = new P(null, "min", "min", 444991522), Zg = new P(null, "show-100s", "show-100s", 1072817186), $g = new P(null, "on-set", "on-set", -140953470), ah = new P(null, "js-file", "js-file", 671190498), bh = new P(null, "page-name", "page-name", 974981762), ch = new P(null, "div.clock-col.clock-legend", 
"div.clock-col.clock-legend", -1934406846), dh = new P(null, "r", "r", -471384190), eh = new P(null, "p3", "p3", 1731040739), fh = new P(null, "stroke", "stroke", 1741823555), gh = new P(null, "calc-bmi", "calc-bmi", -378373501), hh = new P(null, "section#main", "section#main", 559170339), ih = new P(null, "div.color-plate", "div.color-plate", 915488579), jh = new P(null, "timing-wrapper", "timing-wrapper", 1605931939), kh = new P(null, "done", "done", -889844188), Ra = new P(null, "meta", "meta", 
1499536964), lh = new P(null, "str-litt", "str-litt", -1280399676), mh = new P(null, "input#toggle-all", "input#toggle-all", -512330812), nh = new P(null, "call-my-div", "call-my-div", -1621837852), oh = new P(null, "ul", "ul", -1349521403), ph = new P(null, "color", "color", 1011675173), qh = new P(null, "h3.demo-heading", "h3.demo-heading", -42694427), Sa = new P(null, "dup", "dup", 556298533), rh = new P(null, "allow-html5-history", "allow-html5-history", 1717267941), sh = new P(null, "pre", "pre", 
2118456869), th = new P(null, "key", "key", -1516042587), uh = new P(null, "div.demo-example.clearfix", "div.demo-example.clearfix", 1804704421), vh = new P("reagentdemo.news.undodemo", "undo-watcher", "reagentdemo.news.undodemo/undo-watcher", -1793759515), wh = new P(null, "iden", "iden", -109046011), xh = new P(null, "ul#todo-list", "ul#todo-list", -1648361723), yh = new P(null, "placeholder", "placeholder", -104873083), zh = new P(null, "disabled", "disabled", -1529784218), Ah = new P(null, "js-dir", 
"js-dir", -799146714), Bh = new P(null, "font-size", "font-size", -1847940346), Ch = new P(null, "alt", "alt", -3214426), Dh = new P(null, "script", "script", -1304443801), Eh = new P(null, "a.demo-example-hide", "a.demo-example-hide", -63572697), Fh = new P(null, "top", "top", -1856271961), Gh = new P(null, "derefed", "derefed", 590684583), Hh = new P(null, "font-weight", "font-weight", 2085804583), Ih = new P(null, "displayName", "displayName", -809144601), fe = new P(null, "validator", "validator", 
-1966190681), Jh = new P(null, "content", "content", 15833224), Kh = new P(null, "ncolors", "ncolors", 1737431336), Lh = new P(null, "cljsRender", "cljsRender", 247449928), Mh = new P(null, "to-rgb", "to-rgb", -293124504), Nh = new P(null, "other", "other", 995793544), Oh = new P("sitetools", "history", "sitetools/history", -790384888), Ph = new P(null, "ns", "ns", 441598760), Qh = new P(null, "div.nav", "div.nav", -1805454552), Rh = new P(null, "strong", "strong", 269529E3), Sh = new P(null, "-moz-user-select", 
"-moz-user-select", -342302744), Th = new P(null, "name", "name", 1843675177), Uh = new P(null, "update-time", "update-time", -1745455895), Vh = new P(null, "li", "li", 723558921), Wh = new P(null, "fill", "fill", 883462889), Xh = new P(null, "palette", "palette", -456203511), Yh = new P(null, "value", "value", 305978217), Zh = new P("sitetools", "title-watch", "sitetools/title-watch", 1784133513), $h = new P(null, "green", "green", -945526839), ai = new P(null, "lister-user", "lister-user", -1447101430), 
bi = new P(null, "time", "time", 1385887882), ci = new P(null, "on-save", "on-save", 1618176266), di = new P(null, "-webkit-user-select", "-webkit-user-select", -651687510), ei = new P(null, "component-did-mount", "component-did-mount", -1126910518), fi = new P(null, "background-color", "background-color", 570434026), gi = new P(null, "button#clear-completed", "button#clear-completed", -1698725142), hi = new P(null, "circle", "circle", 1903212362), ii = new P(null, "y1", "y1", 589123466), ji = new P(null, 
"input.toggle", "input.toggle", -519545942), ki = new P(null, "width", "width", -384071477), li = new P(null, "on-blur", "on-blur", 814300747), mi = new P(null, "legend", "legend", -1027192245), ni = new P(null, "em", "em", 707813035), oi = new P(null, "bmi-data", "bmi-data", 370723531), pi = new P(null, "component-did-update", "component-did-update", -1468549173), qi = new P(null, "page-map", "page-map", 57430124), ri = new P(null, "type", "type", 1174270348), si = new P(null, "src", "src", -1651076051), 
ti = new P(null, "state", "state", -1988618099), ui = new P(null, "ncolors-choose", "ncolors-choose", -499386099), vi = new P(null, "div.color-samples", "div.color-samples", 1209771565), wi = new P(null, "on-drag", "on-drag", -69159091), Pa = new P(null, "flush-on-newline", "flush-on-newline", -151457939), xi = new P(null, "div.clock-col", "div.clock-col", 1418748813), yi = new P(null, "componentWillUnmount", "componentWillUnmount", 1573788814), zi = new P(null, "component-will-update", "component-will-update", 
335247566), Ai = new P(null, "undo-button", "undo-button", -543825682), Bi = new P(null, "close", "close", 1835149582), Ci = new P(null, "p1", "p1", -936759954), Di = new P(null, "all", "all", 892129742), Ei = new P(null, "div.clock-main", "div.clock-main", -402520242), Fi = new P(null, "on-mouse-down", "on-mouse-down", 1147755470), Gi = new P(null, "lister", "lister", -6951889), Hi = new P(null, "charset", "charset", -1063822193), Ii = new P(null, "on-click", "on-click", 1632826543), Ki = new P(null, 
"p.someclass", "p.someclass", -1904646929), Li = new P(null, "a.github-badge", "a.github-badge", -1294957169), Mi = new P(null, "title", "title", 636505583), Ni = new P(null, "column", "column", 2078222095), Oi = new P(null, "shouldComponentUpdate", "shouldComponentUpdate", 1795750960), Pi = new P(null, "div.view", "div.view", -1680900976), Qi = new P(null, "style", "style", -496642736), Ri = new P(null, "div", "div", 1057191632), Si = new P(null, "has-history", "has-history", -297347344), Qa = new P(null, 
"readably", "readably", 1129599760), Ti = new P(null, "summary", "summary", 380847952), Ui = new P(null, "head", "head", -771383919), Vi = new P(null, "g", "g", 1738089905), Wi = new P(null, "div.clock-cell", "div.clock-cell", -12507663), Xi = new P(null, "counting-component", "counting-component", 2062884465), Yi = new P(null, "c", "c", -1763192079), Zi = new P(null, "req", "req", -326448303), $i = new P(null, "for", "for", -1323786319), aj = new P(null, "render", "render", -1408033454), bj = new P(null, 
"keyw", "keyw", -1031955854), cj = new P(null, "undo", "undo", -1818036302), dj = new P(null, "line", "line", 212345235), ej = new P(null, "simple-component", "simple-component", -990785005), fj = new P(null, "stroke-width", "stroke-width", 716836435), gj = new P(null, "weight", "weight", -1262796205), hj = new P(null, "opt-none", "opt-none", 290474611), ij = new P(null, "on-stop", "on-stop", 1520114515), jj = new P(null, "color-demo", "color-demo", -2135648333), kj = new P(null, "span#todo-count", 
"span#todo-count", -1116128108), Ta = new P(null, "print-length", "print-length", 1931866356), lj = new P(null, "max", "max", 61366548), mj = new P(null, "link", "link", -1769163468), nj = new P(null, "page-titles", "page-titles", -2088509132), oj = new P(null, "active", "active", 1895962068), pj = new P(null, "on-double-click", "on-double-click", 1434856980), qj = new P(null, "cx", "cx", 1272694324), rj = new P(null, "label", "label", 1718410804), sj = new P(null, "id", "id", -1388402092), tj = 
new P(null, "div.clock-pair", "div.clock-pair", -65283468), uj = new P(null, "base-path", "base-path", 495760020), vj = new P(null, "class", "class", -2030961996), wj = new P(null, "red", "red", -969428204), xj = new P(null, "todomvc-with-undo", "todomvc-with-undo", -41768108), yj = new P(null, "blue", "blue", -622100620), zj = new P(null, "bmi", "bmi", 1421979636), Aj = new P(null, "cy", "cy", 755331060), Bj = new P(null, "ul.nav", "ul.nav", 845787189), Cj = new P(null, "auto-run", "auto-run", 1958400437), 
Dj = new P(null, "checked", "checked", -50955819), Ej = new P(null, "comment", "comment", 532206069), Fj = new P(null, "footer#info", "footer#info", 1634811413), Gj = new P(null, "my-div", "my-div", 578248245), Hj = new P(null, "div.demo-text", "div.demo-text", 1676555125), Ij = new P(null, "component-will-unmount", "component-will-unmount", -2058314698), Jj = new P(null, "save-state", "save-state", -1689196426), Kj = new P(null, "svg", "svg", 856789142), Y = new P(null, "code", "code", 1586293142), 
Lj = new P(null, "timer-component", "timer-component", -1786326090), Mj = new P(null, "undo-list", "undo-list", 1944773622), Nj = new P(null, "nsr", "nsr", -336218697), Oj = new P(null, "no-heading", "no-heading", -172020073), Pj = new P(null, "say-hello", "say-hello", -1427060073), Qj = new P(null, "button.destroy", "button.destroy", 1044866871), Rj = new P(null, "component-will-mount", "component-will-mount", 209708855), Sj = new P(null, "position", "position", -2011731912), Tj = new P(null, "simple-parent", 
"simple-parent", -1317913448), Uj = new P(null, "on-dispose", "on-dispose", 2105306360), Vj = new P(null, "componentDidMount", "componentDidMount", 955710936), Wj = new P(null, "h2", "h2", -372662728), Xj = new P(null, "css-file", "css-file", 1072140120), Yj = new P(null, "def", "def", -1043430536), Zj = new P(null, "componentFunction", "componentFunction", 825866104), ak = new P(null, "section#todoapp", "section#todoapp", 41606040), bk = new P(null, "tweak-color", "tweak-color", -162122824), ck = 
new P(null, "clock", "clock", -894301127), dk = new P(null, "complete", "complete", -500388775), ek = new P(null, "div.reagent-demo", "div.reagent-demo", -2027348807), fk = new P(null, "x", "x", 2099068185), gk = new P(null, "__html", "__html", 674048345), hk = new P(null, "x1", "x1", -1863922247), ik = new P(null, "footer#footer", "footer#footer", -1164052935), jk = new P(null, "input", "input", 556931961), kk = new P(null, "reset-random-colors", "reset-random-colors", 187102810), lk = new P(null, 
"component-function", "component-function", 654728922), mk = new P(null, "div.color-input", "div.color-input", -879914246), nk = new P(null, "filt", "filt", 169229082), ok = new P(null, "h1", "h1", -1896887462), pk = new P(null, "li.brand", "li.brand", -1938290661), qk = new P(null, "y2", "y2", -718691301), rk = new P(null, "on-change", "on-change", -732046149), sk = new P(null, "color-choose", "color-choose", -1385957061), tk = new P(null, "main", "main", -2117802661), uk = new P(null, "timestamp", 
"timestamp", 579478971), vk = new P(null, "border", "border", 1444987323), wk = new P(null, "h3", "h3", 2067611163), xk = new P(null, "on-key-down", "on-key-down", -1374733765), yk = new P(null, "body", "body", -2049205669), zk = new P(null, "site-dir", "site-dir", 545174331), Ak = new P(null, "cell", "cell", 764245084), Bk = new P(null, "div.demo-source.clearfix", "div.demo-source.clearfix", 620677308), Ck = new P(null, "base-color", "base-color", -1117474436), Dk = new P(null, "shared-state", "shared-state", 
-190369316), Ek = new P(null, "render-simple", "render-simple", 1183983100), Fk = new P(null, "hello-component", "hello-component", -14597636), Gk = new P(null, "page-conf", "page-conf", -1599869220), Jg = new P(null, "keywordize-keys", "keywordize-keys", 1310784252), Z = new P(null, "p", "p", 151049309), Hk = new P(null, "header#header", "header#header", 1650878621), Ik = new P(null, "ul#filters", "ul#filters", -899831395), Jk = new P(null, "div.color-slider", "div.color-slider", -843416163), Kk = 
new P(null, "x2", "x2", -1362513475), Lk = new P(null, "bmi-component", "bmi-component", -916423875), Mk = new P(null, "div.clearfix", "div.clearfix", 1775605822), Nk = new P(null, "test-results", "test-results", 575566942), Ok = new P(null, "href", "href", -793805698), Pk = new P(null, "comp", "comp", 1191953630), Qk = new P(null, "img", "img", 1442687358), Rk = new P(null, "builtin", "builtin", -1707593346), Sk = new P(null, "a", "a", -2123407586), Tk = new P(null, "dangerouslySetInnerHTML", "dangerouslySetInnerHTML", 
-554971138), Uk = new P(null, "height", "height", 1025178622), Vk = new P(null, "slider", "slider", -472668865), Wk = new P(null, "left", "left", -399115937), Xk = new P(null, "html", "html", -998796897), Yk = new P(null, "text", "text", -1790561697), Zk = new P(null, "span", "span", 1394872991), $k = new P(null, "atom-input", "atom-input", 1370868543);
function al(a, b, c) {
  if ("string" === typeof b) {
    return a.replace(new RegExp(String(b).replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, "\\$1").replace(/\x08/g, "\\x08"), "g"), c);
  }
  if (q(b.hasOwnProperty("source"))) {
    return a.replace(new RegExp(b.source, "g"), c);
  }
  throw "Invalid match arg: " + v.b(b);
}
var bl = function() {
  function a(a, b) {
    return O.a(v, pe(a, b));
  }
  function b(a) {
    return O.a(v, a);
  }
  var c = null, c = function(c, e) {
    switch(arguments.length) {
      case 1:
        return b.call(this, c);
      case 2:
        return a.call(this, c, e);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.b = b;
  c.a = a;
  return c;
}();
function cl(a) {
  return a.toUpperCase();
}
function dl(a, b) {
  if (0 >= b || b >= 2 + J(a)) {
    return Vc.a(Pe(I("", he.a(v, y(a)))), "");
  }
  if (q(Ac.a ? Ac.a(1, b) : Ac.call(null, 1, b))) {
    return new T(null, 1, 5, U, [a], null);
  }
  if (q(Ac.a ? Ac.a(2, b) : Ac.call(null, 2, b))) {
    return new T(null, 2, 5, U, ["", a], null);
  }
  var c = b - 2;
  return Vc.a(Pe(I("", Se.c(Pe(he.a(v, y(a))), 0, c))), Ad.a(a, c));
}
var el = function() {
  function a(a, b, c) {
    if (Ac.a("" + v.b(b), "/(?:)/")) {
      b = dl(a, c);
    } else {
      if (1 > c) {
        b = Pe(("" + v.b(a)).split(b));
      } else {
        a: {
          for (var h = c, m = Uc;;) {
            if (Ac.a(h, 1)) {
              b = Vc.a(m, a);
              break a;
            }
            var n = lg(b, a);
            if (q(n)) {
              var p = n, n = a.indexOf(p), p = a.substring(n + J(p)), h = h - 1, m = Vc.a(m, a.substring(0, n));
              a = p;
            } else {
              b = Vc.a(m, a);
              break a;
            }
          }
          b = void 0;
        }
      }
    }
    if (Ac.a(0, c)) {
      a: {
        for (c = b;;) {
          if (Ac.a("", null == c ? null : Db(c))) {
            c = null == c ? null : Eb(c);
          } else {
            break a;
          }
        }
        c = void 0;
      }
    } else {
      c = b;
    }
    return c;
  }
  function b(a, b) {
    return c.c(a, b, 0);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
var fl, gl = "undefined" !== typeof window && null != window.document;
function hl(a) {
  a = L.c(a, 1, null);
  return fd(a) ? a : null;
}
function il(a) {
  a = a.props.argv;
  var b = L.c(a, 1, null), b = null == b || fd(b) ? 2 : 1;
  return J(a) > b ? Se.a(a, b) : null;
}
function jl(a, b) {
  return a.cljsReactClass = b;
}
function kl(a) {
  return function(b) {
    return function(c) {
      var d = M.a(G.b ? G.b(b) : G.call(null, b), c);
      if (null != d) {
        return d;
      }
      d = a.b ? a.b(c) : a.call(null, c);
      S.j(b, N, c, d);
      return d;
    };
  }(ge.b ? ge.b(df) : ge.call(null, df));
}
var ll = new dg(null, new l(null, 2, ["aria", null, "data", null], null), null);
function ml(a) {
  return 2 > J(a) ? cl(a) : "" + v.b(cl(Ad.c(a, 0, 1))) + v.b(Ad.a(a, 1));
}
function nl(a) {
  if ("string" === typeof a) {
    return a;
  }
  a = Ed(a);
  var b = el.a(a, /-/), c = L.c(b, 0, null), b = zd(b);
  return q(ll.b ? ll.b(c) : ll.call(null, c)) ? a : O.c(v, c, he.a(ml, b));
}
function ol(a) {
  return null == a ? null : 9 === a.nodeType ? a.documentElement : a.firstChild;
}
function pl(a) {
  a = ol(a);
  return null == a ? null : a.getAttribute("data-reactid");
}
var ql = ge.b ? ge.b(df) : ge.call(null, df);
function rl(a, b, c) {
  return React.render(a.o ? a.o() : a.call(null), b, function() {
    var d = pl(b);
    null != d && S.j(ql, N, d, function() {
      return function() {
        var c;
        try {
          c = React.render(a.o ? a.o() : a.call(null), b);
        } catch (d) {
          if (d instanceof Object) {
            try {
              React.unmountComponentAtNode(b);
            } catch (h) {
              if (h instanceof Object) {
                "undefined" !== typeof console && console.log(h);
              } else {
                throw h;
              }
            }
            c = ol(b);
            q(c) && (c.removeAttribute("data-reactid"), c.innerHTML = "");
          }
          throw d;
        }
        return c;
      };
    }(d));
    return null == c ? null : c.o ? c.o() : c.call(null);
  });
}
;var sl, tl = ge.b ? ge.b(0) : ge.call(null, 0);
function ul(a, b) {
  b.$b = null;
  var c = sl;
  try {
    return sl = b, a.o ? a.o() : a.call(null);
  } finally {
    sl = c;
  }
}
function vl(a) {
  var b = a.$b;
  a.$b = null;
  return b;
}
function wl(a) {
  var b = sl;
  if (null != b) {
    var c = b.$b;
    b.$b = Vc.a(null == c ? fg : c, a);
  }
}
function xl(a, b, c, d) {
  this.state = a;
  this.k = b;
  this.R = d;
  this.i = 2153938944;
  this.p = 114690;
}
g = xl.prototype;
g.w = function() {
  return ea(this);
};
g.Yb = function(a, b, c) {
  return sd(function(a) {
    return function(e, f, h) {
      h.j ? h.j(f, a, b, c) : h.call(null, f, a, b, c);
      return null;
    };
  }(this), null, this.R);
};
g.Xb = function(a, b, c) {
  return this.R = N.c(this.R, b, c);
};
g.Zb = function(a, b) {
  return this.R = Yc.a(this.R, b);
};
g.v = function(a, b, c) {
  Wb(b, "#\x3cAtom: ");
  vg(this.state, b, c);
  return Wb(b, "\x3e");
};
g.D = function() {
  return this.k;
};
g.zc = function(a, b) {
  var c = kc, d;
  d = this.state;
  d = b.b ? b.b(d) : b.call(null, d);
  return c(this, d);
};
g.Ac = function(a, b, c) {
  a = kc;
  var d = this.state;
  b = b.a ? b.a(d, c) : b.call(null, d, c);
  return a(this, b);
};
g.Bc = function(a, b, c, d) {
  a = kc;
  var e = this.state;
  b = b.c ? b.c(e, c, d) : b.call(null, e, c, d);
  return a(this, b);
};
g.Cc = function(a, b, c, d, e) {
  return kc(this, O.t(b, this.state, c, d, e));
};
g.yc = function(a, b) {
  var c = this.state;
  this.state = b;
  null != this.R && Zb(this, c, b);
  return b;
};
g.sb = function() {
  wl(this);
  return this.state;
};
g.u = function(a, b) {
  return this === b;
};
var yl = function() {
  function a(a) {
    return new xl(a, null, 0, null);
  }
  var b = null, c = function() {
    function a(c, d) {
      var m = null;
      1 < arguments.length && (m = E(Array.prototype.slice.call(arguments, 1), 0));
      return b.call(this, c, m);
    }
    function b(a, c) {
      var d = md(c) ? O.a(ee, c) : c;
      M.a(d, fe);
      d = M.a(d, Ra);
      return new xl(a, d, 0, null);
    }
    a.m = 1;
    a.h = function(a) {
      var c = A(a);
      a = C(a);
      return b(c, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e) {
    switch(arguments.length) {
      case 1:
        return a.call(this, b);
      default:
        return c.e(b, E(arguments, 1));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 1;
  b.h = c.h;
  b.b = a;
  b.e = c.e;
  return b;
}();
function zl(a) {
  if (a ? a.gd : a) {
    return a.gd();
  }
  var b;
  b = zl[k(null == a ? null : a)];
  if (!b && (b = zl._, !b)) {
    throw u("IDisposable.dispose!", a);
  }
  return b.call(null, a);
}
function Al(a) {
  if (a ? a.hd : a) {
    return a.hd();
  }
  var b;
  b = Al[k(null == a ? null : a)];
  if (!b && (b = Al._, !b)) {
    throw u("IRunnable.run", a);
  }
  return b.call(null, a);
}
function Bl(a, b) {
  if (a ? a.Kc : a) {
    return a.Kc(0, b);
  }
  var c;
  c = Bl[k(null == a ? null : a)];
  if (!c && (c = Bl._, !c)) {
    throw u("IComputedImpl.-update-watching", a);
  }
  return c.call(null, a, b);
}
function Cl(a, b, c, d) {
  if (a ? a.fd : a) {
    return a.fd(0, 0, c, d);
  }
  var e;
  e = Cl[k(null == a ? null : a)];
  if (!e && (e = Cl._, !e)) {
    throw u("IComputedImpl.-handle-change", a);
  }
  return e.call(null, a, b, c, d);
}
function Dl(a, b, c, d) {
  return sd(function(b, f, h) {
    h.j ? h.j(f, a, c, d) : h.call(null, f, a, c, d);
    return null;
  }, null, b);
}
function El(a, b, c, d, e, f, h, m, n) {
  this.Hd = a;
  this.state = b;
  this.Ab = c;
  this.qb = d;
  this.ab = e;
  this.R = f;
  this.qc = h;
  this.ic = m;
  this.hc = n;
  this.i = 2153807872;
  this.p = 114690;
}
g = El.prototype;
g.fd = function(a, b, c, d) {
  var e = this;
  return q(function() {
    var a = e.qb;
    return q(a) ? Ua(e.Ab) && c !== d : a;
  }()) ? (e.Ab = !0, function() {
    var a = e.qc;
    return q(a) ? a : Al;
  }().call(null, this)) : null;
};
g.Kc = function(a, b) {
  for (var c = y(b), d = null, e = 0, f = 0;;) {
    if (f < e) {
      var h = d.C(null, f);
      pd(this.ab, h) || $b(h, this, Cl);
      f += 1;
    } else {
      if (c = y(c)) {
        d = c, hd(d) ? (c = hc(d), f = ic(d), d = c, e = J(c), c = f) : (c = A(d), pd(this.ab, c) || $b(c, this, Cl), c = D(d), d = null, e = 0), f = 0;
      } else {
        break;
      }
    }
  }
  c = y(this.ab);
  d = null;
  for (f = e = 0;;) {
    if (f < e) {
      h = d.C(null, f), pd(b, h) || ac(h, this), f += 1;
    } else {
      if (c = y(c)) {
        d = c, hd(d) ? (c = hc(d), f = ic(d), d = c, e = J(c), c = f) : (c = A(d), pd(b, c) || ac(c, this), c = D(d), d = null, e = 0), f = 0;
      } else {
        break;
      }
    }
  }
  return this.ab = b;
};
g.v = function(a, b, c) {
  Wb(b, "#\x3cReaction " + v.b(xc(this)) + ": ");
  vg(this.state, b, c);
  return Wb(b, "\x3e");
};
g.w = function() {
  return ea(this);
};
g.u = function(a, b) {
  return this === b;
};
g.gd = function() {
  for (var a = y(this.ab), b = null, c = 0, d = 0;;) {
    if (d < c) {
      var e = b.C(null, d);
      ac(e, this);
      d += 1;
    } else {
      if (a = y(a)) {
        b = a, hd(b) ? (a = hc(b), d = ic(b), b = a, c = J(a), a = d) : (a = A(b), ac(a, this), a = D(b), b = null, c = 0), d = 0;
      } else {
        break;
      }
    }
  }
  this.ab = fg;
  this.state = null;
  this.Ab = !0;
  q(this.qb) && (q(!1) && S.a(tl, wd), this.qb = !1);
  return q(this.hc) ? this.hc.o ? this.hc.o() : this.hc.call(null) : null;
};
g.yc = function(a, b) {
  var c = this.state;
  this.state = b;
  Zb(this, c, b);
  return b;
};
g.zc = function(a, b) {
  var c = kc, d;
  d = this.state;
  d = b.b ? b.b(d) : b.call(null, d);
  return c(this, d);
};
g.Ac = function(a, b, c) {
  a = kc;
  var d = this.state;
  b = b.a ? b.a(d, c) : b.call(null, d, c);
  return a(this, b);
};
g.Bc = function(a, b, c, d) {
  a = kc;
  var e = this.state;
  b = b.c ? b.c(e, c, d) : b.call(null, e, c, d);
  return a(this, b);
};
g.Cc = function(a, b, c, d, e) {
  return kc(this, O.t(b, this.state, c, d, e));
};
g.hd = function() {
  var a = this.state, b = ul(this.Hd, this), c = vl(this);
  Yd.a(c, this.ab) && Bl(this, c);
  q(this.qb) || (q(!1) && S.a(tl, Jc), this.qb = !0);
  this.Ab = !1;
  this.state = b;
  Dl(this, this.R, a, this.state);
  return b;
};
g.Yb = function(a, b, c) {
  q(this.ic) && (this.ic.a ? this.ic.a(b, c) : this.ic.call(null, b, c));
  return Dl(this, this.R, b, c);
};
g.Xb = function(a, b, c) {
  return this.R = N.c(this.R, b, c);
};
g.Zb = function(a, b) {
  this.R = Yc.a(this.R, b);
  return cd(this.R) ? zl(this) : null;
};
g.sb = function() {
  var a = this.qc;
  Ua(q(a) ? a : sl) && (a = new T(null, 2, 5, U, [this.qc, sl], null), "undefined" !== typeof console && console.log("" + v.b("dbg reagent.ratom:249: [auto-run *ratom-context*]: " + v.b(xg.e(E([a], 0))))));
  wl(this);
  return q(this.Ab) ? Al(this) : this.state;
};
var Fl = function() {
  function a(a, d) {
    var e = null;
    1 < arguments.length && (e = E(Array.prototype.slice.call(arguments, 1), 0));
    return b.call(this, a, e);
  }
  function b(a, b) {
    var e = md(b) ? O.a(ee, b) : b, f = M.a(e, Gh), h = M.a(e, Uj), m = M.a(e, $g), e = M.a(e, Cj), e = Ac.a(e, !0) ? Al : e, n = null != f, h = new El(a, null, !n, n, null, df, e, m, h);
    null != f && (q(!1) && S.a(tl, Jc), h.Kc(0, f));
    return h;
  }
  a.m = 1;
  a.h = function(a) {
    var d = A(a);
    a = C(a);
    return b(d, a);
  };
  a.e = b;
  return a;
}();
if ("undefined" === typeof Gl) {
  var Gl = 0
}
function Hl(a) {
  return setTimeout(a, 16);
}
var Il = Ua(gl) ? Hl : function() {
  var a = window, b = a.requestAnimationFrame;
  if (q(b)) {
    return b;
  }
  b = a.webkitRequestAnimationFrame;
  if (q(b)) {
    return b;
  }
  b = a.mozRequestAnimationFrame;
  if (q(b)) {
    return b;
  }
  a = a.msRequestAnimationFrame;
  return q(a) ? a : Hl;
}();
function Jl(a, b) {
  return b.cljsMountOrder - a.cljsMountOrder;
}
function Kl() {
  var a = Ll;
  if (q(a.Lc)) {
    return null;
  }
  a.Lc = !0;
  a = function(a) {
    return function() {
      return Ml(a);
    };
  }(a);
  return Il.b ? Il.b(a) : Il.call(null, a);
}
function Ml(a) {
  var b = a.Jc, c = a.pc;
  a.Jc = [];
  a.pc = [];
  a.Lc = !1;
  a: {
    b.sort(Jl);
    a = b.length;
    for (var d = 0;;) {
      if (d < a) {
        var e = b[d];
        q(e.cljsIsDirty) && e.forceUpdate();
        d += 1;
      } else {
        break a;
      }
    }
  }
  a: {
    b = c.length;
    for (a = 0;;) {
      if (a < b) {
        c[a].call(null), a += 1;
      } else {
        c = null;
        break a;
      }
    }
    c = void 0;
  }
  return c;
}
var Ll = new function() {
  this.Jc = [];
  this.Lc = !1;
  this.pc = [];
};
function Nl(a) {
  Ll.pc.push(a);
  Kl();
}
function Ol(a, b) {
  a.cljsIsDirty = !1;
  var c = a.cljsRatom;
  if (null == c) {
    var d = ul(b, a), e = vl(a);
    null != e && (a.cljsRatom = Fl.e(b, E([Cj, function() {
      return function() {
        a.cljsIsDirty = !0;
        Ll.Jc.push(a);
        return Kl();
      };
    }(d, e, c), Gh, e], 0)));
    return d;
  }
  return Al(c);
}
;var Pl, Ql;
function Rl(a) {
  var b = a.cljsState;
  return null != b ? b : a.cljsState = yl.b(null);
}
var Sl = null, Ul = function Tl(b) {
  var c = Pl;
  try {
    Pl = b;
    var d = b.cljsRender, e = b.props, f = null == b.componentFunction ? d.b ? d.b(b) : d.call(null, b) : function() {
      var b = e.argv;
      switch(J(b)) {
        case 1:
          return d.o ? d.o() : d.call(null);
        case 2:
          return b = L.a(b, 1), d.b ? d.b(b) : d.call(null, b);
        case 3:
          var c = L.a(b, 1), b = L.a(b, 2);
          return d.a ? d.a(c, b) : d.call(null, c, b);
        case 4:
          var c = L.a(b, 1), f = L.a(b, 2), b = L.a(b, 3);
          return d.c ? d.c(c, f, b) : d.call(null, c, f, b);
        case 5:
          var c = L.a(b, 1), f = L.a(b, 2), p = L.a(b, 3), b = L.a(b, 4);
          return d.j ? d.j(c, f, p, b) : d.call(null, c, f, p, b);
        default:
          return O.a(d, Se.a(b, 1));
      }
    }();
    return gd(f) ? Sl.b ? Sl.b(f) : Sl.call(null, f) : od(f) ? (b.cljsRender = f, Tl(b)) : f;
  } finally {
    Pl = c;
  }
}, Vl = new l(null, 1, [aj, function() {
  return Ua(Ql) ? Ol(this, function(a) {
    return function() {
      return Ul(a);
    };
  }(this)) : Ul(this);
}], null);
function Wl(a, b) {
  var c = a instanceof P ? a.da : null;
  switch(c) {
    case "componentWillUnmount":
      return function() {
        return function() {
          var a = this.cljsRatom;
          null == a || zl(a);
          this.cljsIsDirty = !1;
          return null == b ? null : b.b ? b.b(this) : b.call(null, this);
        };
      }(c);
    case "componentDidMount":
      return function() {
        return function() {
          this.cljsMountOrder = Gl += 1;
          return null == b ? null : b.b ? b.b(this) : b.call(null, this);
        };
      }(c);
    case "componentDidUpdate":
      return function() {
        return function(a) {
          a = a.argv;
          return b.a ? b.a(this, a) : b.call(null, this, a);
        };
      }(c);
    case "componentWillUpdate":
      return function() {
        return function(a) {
          a = a.argv;
          return b.a ? b.a(this, a) : b.call(null, this, a);
        };
      }(c);
    case "shouldComponentUpdate":
      return function() {
        return function(a) {
          var c = fl;
          if (q(c)) {
            return c;
          }
          c = this.props.argv;
          a = a.argv;
          return null == b ? !Ac.a(c, a) : b.c ? b.c(this, c, a) : b.call(null, this, c, a);
        };
      }(c);
    case "componentWillReceiveProps":
      return function() {
        return function(a) {
          a = a.argv;
          return b.a ? b.a(this, a) : b.call(null, this, a);
        };
      }(c);
    case "getInitialState":
      return function() {
        return function() {
          var a = b.b ? b.b(this) : b.call(null, this);
          return S.c(Rl(this), cg, a);
        };
      }(c);
    case "getDefaultProps":
      return null;
    default:
      return null;
  }
}
function Xl(a) {
  return od(a) ? function() {
    function b(a) {
      var b = null;
      0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
      return c.call(this, b);
    }
    function c(b) {
      return O.c(a, this, b);
    }
    b.m = 0;
    b.h = function(a) {
      a = y(a);
      return c(a);
    };
    b.e = c;
    return b;
  }() : a;
}
var $l = new dg(null, new l(null, 3, [Lh, null, aj, null, Zj, null], null), null);
function am(a, b) {
  if (q($l.b ? $l.b(a) : $l.call(null, a))) {
    return od(b) && (b.__reactDontBind = !0), b;
  }
  var c = Wl(a, b);
  return q(c) ? c : Xl(b);
}
var bm = new l(null, 3, [Oi, null, Vj, null, yi, null], null), cm = kl(nl);
function dm(a) {
  return sd(function(a, c, d) {
    return N.c(a, Fd.b(cm.b ? cm.b(c) : cm.call(null, c)), d);
  }, df, a);
}
function em(a) {
  return cg.e(E([bm, a], 0));
}
function fm(a, b) {
  return N.e(a, Lh, b, E([aj, aj.b(Vl)], 0));
}
function gm(a) {
  var b = function() {
    var b = Zj.b(a);
    return q(b) ? b : aj.b(a);
  }(), c = function() {
    var c = Ih.b(a);
    if (q(c)) {
      return c;
    }
    c = b.displayName;
    return q(c) ? c : b.name;
  }(), d = cd(c) ? "" + v.b(Bg.b("reagent")) : c, e = fm(N.c(a, Ih, d), b);
  return sd(function() {
    return function(a, b, c) {
      return N.c(a, b, am(b, c));
    };
  }(b, null, c, d, e), df, e);
}
function hm(a) {
  return sd(function(a, c, d) {
    a[Ed(c)] = d;
    return a;
  }, {}, a);
}
function im(a) {
  var b = hm(gm(em(dm(a))));
  a = React.createClass(b);
  b = function(a, b) {
    return function() {
      function a(b) {
        var d = null;
        0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
        return c.call(this, d);
      }
      function c(a) {
        a = O.c(Qe, b, a);
        return Sl.b ? Sl.b(a) : Sl.call(null, a);
      }
      a.m = 0;
      a.h = function(a) {
        a = y(a);
        return c(a);
      };
      a.e = c;
      return a;
    }();
  }(b, a);
  jl(b, a);
  jl(a, a);
  return b;
}
;var jm = /([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?/, km = new l(null, 3, [vj, "className", $i, "htmlFor", Hi, "charSet"], null);
function lm(a) {
  return "string" === typeof a ? a : "number" === typeof a ? a : a instanceof P ? Ed(a) : a instanceof Cc ? "" + v.b(a) : dd(a) ? Fg(a) : od(a) ? function() {
    function b(a) {
      var b = null;
      0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
      return c.call(this, b);
    }
    function c(b) {
      return O.a(a, b);
    }
    b.m = 0;
    b.h = function(a) {
      a = y(a);
      return c(a);
    };
    b.e = c;
    return b;
  }() : a;
}
var mm = kl(function(a) {
  var b = km.b ? km.b(a) : km.call(null, a);
  return q(b) ? b : nl(a);
});
kl(nl);
function nm(a) {
  return "string" === typeof a ? a : "number" === typeof a ? a : fd(a) ? sd(function(a, c, d) {
    a[mm.b ? mm.b(c) : mm.call(null, c)] = lm(d);
    return a;
  }, {}, a) : lm(a);
}
function om(a, b) {
  var c = L.c(b, 0, null), d = L.c(b, 1, null), e = a.id;
  a.id = null != e ? e : c;
  null != d && (c = a.className, a.className = null != c ? "" + v.b(d) + " " + v.b(c) : d);
}
function pm(a, b) {
  if (cd(a) && null == b) {
    return null;
  }
  if (Va(a) === Object) {
    return a;
  }
  var c = sd(function(a, b, c) {
    b = mm.b ? mm.b(b) : mm.call(null, b);
    "key" !== b && (a[b] = nm(c));
    return a;
  }, {}, a);
  null != b && om(c, b);
  return c;
}
function qm(a) {
  var b = a.cljsInputValue;
  if (null == b) {
    return null;
  }
  a.cljsInputDirty = !1;
  a = a.getDOMNode();
  return Yd.a(b, a.value) ? a.value = b : null;
}
function rm(a, b, c) {
  b = b.b ? b.b(c) : b.call(null, c);
  q(a.cljsInputDirty) || (a.cljsInputDirty = !0, Nl(function() {
    return function() {
      return qm(a);
    };
  }(b)));
  return b;
}
function sm(a, b) {
  if (q(function() {
    var a = b.hasOwnProperty("onChange");
    return q(a) ? b.hasOwnProperty("value") : a;
  }())) {
    var c = b.value, d = null == c ? "" : c, e = b.onChange;
    a.cljsInputValue = d;
    delete b.value;
    b.defaultValue = d;
    b.onChange = function(b, c, d, e) {
      return function(b) {
        return rm(a, e, b);
      };
    }(b, c, d, e);
    return b;
  }
  return a.cljsInputValue = null;
}
function tm(a) {
  var b = React.DOM;
  return Ac.a(a, "input") || Ac.a(a, "textarea") || a === b.input || a === b.textarea;
}
function um(a) {
  a.componentDidUpdate = function() {
    return function() {
      return qm(this);
    };
  }(a);
  a.componentWillUnmount = function() {
    return function() {
      return this.cljsInputValue = null;
    };
  }(a);
}
function vm(a, b, c) {
  var d = tm(a), e = d ? sm : null;
  c = {displayName:q(c) ? c : "ComponentWrapper", shouldComponentUpdate:function() {
    return function(a) {
      var b = fl;
      return q(b) ? b : !Ac.a(this.props.argv, a.argv);
    };
  }(d, e), render:function(c, d) {
    return function() {
      var c = this.props.argv, e = L.c(c, 1, null), f = null == e || fd(e), e = pm(f ? e : null, b);
      null != d && (d.a ? d.a(this, e) : d.call(null, this, e));
      f = f ? 2 : 1;
      return wm.j ? wm.j(c, a, e, f) : wm.call(null, c, a, e, f);
    };
  }(d, e)};
  d && um(c);
  return React.createClass(c);
}
var xm = kl(function(a) {
  var b, c;
  c = Ed(a);
  if ("string" === typeof c) {
    b = jm.exec(c), c = Ac.a(A(b), c) ? 1 === J(b) ? A(b) : Pe(b) : null;
  } else {
    throw new TypeError("re-matches must match against a string.");
  }
  var d = D(c);
  c = L.c(d, 0, null);
  b = L.c(d, 1, null);
  d = L.c(d, 2, null);
  d = q(d) ? al(d, /\./, " ") : null;
  b = new T(null, 2, 5, U, [c, q(q(b) ? b : d) ? new T(null, 2, 5, U, [b, d], null) : null], null);
  c = L.c(b, 0, null);
  b = L.c(b, 1, null);
  return vm(c, b, "" + v.b(a));
});
function ym(a) {
  return fd(a) ? M.a(a, th) : null;
}
function zm(a) {
  if ("string" !== typeof a) {
    if (gd(a)) {
      var b;
      b = L.a(a, 0);
      if (b instanceof P || b instanceof Cc || "string" === typeof b) {
        b = xm.b ? xm.b(b) : xm.call(null, b);
      } else {
        var c = b.cljsReactClass;
        null != c ? b = c : q(React.isValidElement(b)) ? b = jl(b, vm(b, null, null)) : (c = bd(b), c = N.c(c, lk, b), c = im(c).cljsReactClass, jl(b, c), b = c);
      }
      var c = {argv:a}, d = ym(bd(a));
      a = null == d ? ym(L.c(a, 1, null)) : d;
      null != a && (c.key = a);
      a = React.createElement(b, c);
    } else {
      a = md(a) ? Am.b ? Am.b(a) : Am.call(null, a) : a;
    }
  }
  return a;
}
Sl = zm;
function Am(a) {
  a = cb.b(a);
  for (var b = a.length, c = 0;;) {
    if (c < b) {
      a[c] = zm(a[c]), c += 1;
    } else {
      break;
    }
  }
  return a;
}
function wm(a, b, c, d) {
  return J(a) === d + 1 ? React.createElement(b, c, zm(L.a(a, d))) : React.createElement.apply(null, sd(function(a, b, c) {
    b >= d && a.push(zm(c));
    return a;
  }, [b, c], a));
}
;var Bm = function() {
  function a(a, b, c) {
    return rl(function() {
      var b = Zc(a) ? a.o ? a.o() : a.call(null) : a;
      return zm(b);
    }, b, c);
  }
  function b(a, b) {
    return c.c(a, b, null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function Cm(a) {
  var b = Ql;
  try {
    return Ql = !0, React.renderToString(zm(a));
  } finally {
    Ql = b;
  }
}
function Dm(a) {
  var b = Ql;
  try {
    return Ql = !0, React.renderToStaticMarkup(zm(a));
  } finally {
    Ql = b;
  }
}
ma("reagent.core.force_update_all", function() {
  var a = fl;
  try {
    fl = !0;
    for (var b = y(bg(G.b ? G.b(ql) : G.call(null, ql))), c = null, d = 0, e = 0;;) {
      if (e < d) {
        var f = c.C(null, e);
        f.o ? f.o() : f.call(null);
        e += 1;
      } else {
        var h = y(b);
        if (h) {
          var m = h;
          if (hd(m)) {
            var n = hc(m), p = ic(m), m = n, s = J(n), b = p, c = m, d = s
          } else {
            var t = A(m);
            t.o ? t.o() : t.call(null);
            b = D(m);
            c = null;
            d = 0;
          }
          e = 0;
        } else {
          break;
        }
      }
    }
  } finally {
    fl = a;
  }
  return "Updated";
});
var Em = function() {
  function a(a) {
    return yl.b(a);
  }
  var b = null, c = function() {
    function a(c, d) {
      var m = null;
      1 < arguments.length && (m = E(Array.prototype.slice.call(arguments, 1), 0));
      return b.call(this, c, m);
    }
    function b(a, c) {
      return O.c(yl, a, c);
    }
    a.m = 1;
    a.h = function(a) {
      var c = A(a);
      a = C(a);
      return b(c, a);
    };
    a.e = b;
    return a;
  }(), b = function(b, e) {
    switch(arguments.length) {
      case 1:
        return a.call(this, b);
      default:
        return c.e(b, E(arguments, 1));
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  b.m = 1;
  b.h = c.h;
  b.b = a;
  b.e = c.e;
  return b;
}();
function Fm(a) {
  return Il.b ? Il.b(a) : Il.call(null, a);
}
;var Gm = Em.b(Yf()), Hm = Em.b(0);
function Im(a) {
  var b = S.a(Hm, Jc);
  return S.j(Gm, N, b, new l(null, 3, [sj, b, Mi, a, kh, !1], null));
}
function Jm(a, b, c) {
  return se.a(null == a ? null : jb(a), b.a ? b.a(c, a) : b.call(null, c, a));
}
function Km(a) {
  return S.j(Gm, Jm, he, function(b) {
    return ve(b, new T(null, 2, 5, U, [1, kh], null), a);
  });
}
function Lm() {
  return S.j(Gm, Jm, re, function(a) {
    return te.a(a, new T(null, 2, 5, U, [1, kh], null));
  });
}
Im("Rename Cloact to Reagent");
Im("Add undo demo");
Im("Make all rendering async");
Im("Allow any arguments to component functions");
Km(!0);
function Mm(a) {
  var b = md(a) ? O.a(ee, a) : a, c = M.a(b, ij), d = M.a(b, ci), e = M.a(b, Mi), f = Em.b(e), h = function(a, b, c, d) {
    return function() {
      Q.a ? Q.a(a, "") : Q.call(null, a, "");
      return q(d) ? d.o ? d.o() : d.call(null) : null;
    };
  }(f, a, b, c, d, e);
  return function(a, b, c, d, e, f, h, z) {
    return function(F) {
      return new T(null, 2, 5, U, [jk, cg.e(E([F, new l(null, 5, [ri, "text", Yh, G.b ? G.b(a) : G.call(null, a), li, c, rk, function(a) {
        return function(b) {
          b = b.target.value;
          return Q.a ? Q.a(a, b) : Q.call(null, a, b);
        };
      }(a, b, c, d, e, f, h, z), xk, function(a, b, c) {
        return function(a) {
          switch(a.which) {
            case 13:
              return c();
            case 27:
              return b();
            default:
              return null;
          }
        };
      }(a, b, c, d, e, f, h, z)], null)], 0))], null);
    };
  }(f, h, function(a, b, c, d, e, f) {
    return function() {
      var c;
      c = "" + v.b(G.b ? G.b(a) : G.call(null, a));
      c = pa(c);
      cd(c) || (f.b ? f.b(c) : f.call(null, c));
      return b();
    };
  }(f, h, a, b, c, d, e), a, b, c, d, e);
}
var Nm = ad(Mm, new l(null, 1, [ei, function(a) {
  return a.getDOMNode().focus();
}], null));
function Om(a) {
  var b = md(a) ? O.a(ee, a) : a, c = M.a(b, kh), d = M.a(b, oj), e = M.a(b, nk);
  a = function(a, b, c, d, e) {
    return function(s) {
      return new l(null, 2, [vj, Ac.a(s, G.b ? G.b(e) : G.call(null, e)) ? "selected" : null, Ii, function(a, b, c, d, e) {
        return function() {
          return Q.a ? Q.a(e, s) : Q.call(null, e, s);
        };
      }(a, b, c, d, e)], null);
    };
  }(a, b, c, d, e);
  a: {
    switch(d) {
      case 1:
        b = "item";
        break a;
      default:
        b = "items";
    }
  }
  return new T(null, 4, 5, U, [Ri, new T(null, 5, 5, U, [kj, new T(null, 2, 5, U, [Rh, d], null), " ", b, " left"], null), new T(null, 4, 5, U, [Ik, new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [Sk, a(Di), "All"], null)], null), new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [Sk, a(oj), "Active"], null)], null), new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [Sk, a(kh), "Completed"], null)], null)], null), 0 < c ? new T(null, 4, 5, U, [gi, new l(null, 1, [Ii, Lm], null), "Clear completed ", c], 
  null) : null], null);
}
function Pm() {
  return function(a) {
    return function(b) {
      var c = md(b) ? O.a(ee, b) : b, d = M.a(c, Mi), e = M.a(c, kh), f = M.a(c, sj);
      return new T(null, 4, 5, U, [Vh, new l(null, 1, [vj, "" + v.b(q(e) ? "completed " : null) + v.b(q(G.b ? G.b(a) : G.call(null, a)) ? "editing" : null)], null), new T(null, 4, 5, U, [Pi, new T(null, 2, 5, U, [ji, new l(null, 3, [ri, "checkbox", Dj, e, rk, function(a, b, c, d, e) {
        return function() {
          return S.j(Gm, we, new T(null, 2, 5, U, [e, kh], null), Ua);
        };
      }(b, c, d, e, f, a)], null)], null), new T(null, 3, 5, U, [rj, new l(null, 1, [pj, function(a, b, c, d, e, f) {
        return function() {
          return Q.a ? Q.a(f, !0) : Q.call(null, f, !0);
        };
      }(b, c, d, e, f, a)], null), d], null), new T(null, 2, 5, U, [Qj, new l(null, 1, [Ii, function(a, b, c, d, e) {
        return function() {
          return S.c(Gm, Yc, e);
        };
      }(b, c, d, e, f, a)], null)], null)], null), q(G.b ? G.b(a) : G.call(null, a)) ? new T(null, 2, 5, U, [Nm, new l(null, 4, [vj, "edit", Mi, d, ci, function(a, b, c, d, e) {
        return function(a) {
          return S.j(Gm, ve, new T(null, 2, 5, U, [e, Mi], null), a);
        };
      }(b, c, d, e, f, a), ij, function(a, b, c, d, e, f) {
        return function() {
          return Q.a ? Q.a(f, !1) : Q.call(null, f, !1);
        };
      }(b, c, d, e, f, a)], null)], null) : null], null);
    };
  }(Em.b(!1));
}
function Qm() {
  return function(a) {
    return function() {
      var b = bg(G.b ? G.b(Gm) : G.call(null, Gm)), c = J(qe.a(kh, b)), d = J(b) - c;
      return new T(null, 3, 5, U, [Ri, new T(null, 3, 5, U, [ak, new T(null, 3, 5, U, [Hk, new T(null, 2, 5, U, [ok, "todos"], null), new T(null, 2, 5, U, [Mm, new l(null, 3, [sj, "new-todo", yh, "What needs to be done?", ci, Im], null)], null)], null), 0 < J(b) ? new T(null, 3, 5, U, [Ri, new T(null, 4, 5, U, [hh, new T(null, 2, 5, U, [mh, new l(null, 3, [ri, "checkbox", Dj, 0 === d, rk, function(a, b, c) {
        return function() {
          return Km(0 < c);
        };
      }(b, c, d, a)], null)], null), new T(null, 3, 5, U, [rj, new l(null, 1, [$i, "toggle-all"], null), "Mark all as complete"], null), new T(null, 2, 5, U, [xh, function() {
        return function(a, b, c, d) {
          return function p(s) {
            return new Gd(null, function() {
              return function() {
                for (;;) {
                  var a = y(s);
                  if (a) {
                    if (hd(a)) {
                      var b = hc(a), c = J(b), d = Kd(c);
                      a: {
                        for (var e = 0;;) {
                          if (e < c) {
                            var f = x.a(b, e), f = ad(new T(null, 2, 5, U, [Pm, f], null), new l(null, 1, [th, sj.b(f)], null));
                            d.add(f);
                            e += 1;
                          } else {
                            b = !0;
                            break a;
                          }
                        }
                        b = void 0;
                      }
                      return b ? Nd(d.N(), p(ic(a))) : Nd(d.N(), null);
                    }
                    d = A(a);
                    return I(ad(new T(null, 2, 5, U, [Pm, d], null), new l(null, 1, [th, sj.b(d)], null)), p(C(a)));
                  }
                  return null;
                }
              };
            }(a, b, c, d), null, null);
          };
        }(b, c, d, a)(qe.a(function() {
          switch((G.b ? G.b(a) : G.call(null, a)) instanceof P ? (G.b ? G.b(a) : G.call(null, a)).da : null) {
            case "all":
              return td;
            case "done":
              return kh;
            case "active":
              return ae(kh);
            default:
              throw Error("No matching clause: " + v.b(G.b ? G.b(a) : G.call(null, a)));;
          }
        }(), b));
      }()], null)], null), new T(null, 2, 5, U, [ik, new T(null, 2, 5, U, [Om, new l(null, 3, [oj, d, kh, c, nk, a], null)], null)], null)], null) : null], null), new T(null, 2, 5, U, [Fj, new T(null, 2, 5, U, [Z, "Double-click to edit a todo"], null)], null)], null);
    };
  }(Em.b(Di));
}
ma("todomvc.run", function() {
  var a = new T(null, 1, 5, U, [Qm], null), b = document.body;
  return Bm.a ? Bm.a(a, b) : Bm.call(null, a, b);
});
var Rm = new dg(null, new l(null, 63, ["partial", null, "map", null, "int", null, "range", null, "defn", null, "\x3d", null, "list", null, "%", null, "bit-test", null, "if-not", null, "dissoc", null, "not", null, "if-let", null, "update-in", null, "str", null, "second", null, "get-in", null, "fn", null, "for", null, "rest", null, "count", null, "empty?", null, "swap!", null, "sorted-map", null, "nil?", null, "false", null, "cond", null, "if", null, "let", null, "drop", null, "vals", null, "inc", 
null, "when-let", null, "atom", null, "vector", null, "or", null, "identity", null, "case", null, "remove", null, "true", null, "filter", null, "empty", null, "complement", null, "when", null, "reset!", null, "-\x3e", null, "mod", null, "pos?", null, "conj", null, "-\x3e\x3e", null, "add-watch", null, "zero?", null, "quot", null, "def", null, "dotimes", null, "assoc-in", null, "assoc", null, "into", null, "when-not", null, "dec", null, "merge", null, "ns", null, "first", null], null), null), Sm = 
new l(null, 5, [Ej, new l(null, 1, [Qi, new l(null, 2, [ph, "gray", Tg, "italic"], null)], null), lh, new l(null, 1, [Qi, new l(null, 1, [ph, "green"], null)], null), bj, new l(null, 1, [Qi, new l(null, 1, [ph, "blue"], null)], null), Rk, new l(null, 1, [Qi, new l(null, 2, [Hh, "bold", ph, "#687868"], null)], null), Yj, new l(null, 1, [Qi, new l(null, 2, [ph, "#55c", Hh, "bold"], null)], null)], null), Tm = new T(null, 3, 5, U, [new l(null, 1, [Qi, new l(null, 1, [ph, "#272"], null)], null), new l(null, 
1, [Qi, new l(null, 1, [ph, "#940"], null)], null), new l(null, 1, [Qi, new l(null, 1, [ph, "#44a"], null)], null)], null);
function Um(a) {
  var b = "" + v.b(" \\t\\n") + v.b("[({") + v.b(")\\]}"), c = "[" + v.b("[({") + "]", d = "[" + v.b(")\\]}") + "]", e = "[^" + v.b(b) + "]+", f = "\\^" + v.b(e), h = "[" + v.b(" \\t\\n") + "]+|\\^[^" + v.b(b) + "]+|.", m = og("(" + v.b(bl.a(")|(", new T(null, 7, 5, U, [";.*", '"[^"]*"', c, d, f, e, h], null))) + ")");
  return function(a, b, c, d, e, f, h, m, K, R, X, da, ta) {
    return function H(Yl) {
      return new Gd(null, function(a, b, c, d, e, f, h, m, n, p, s, t, w) {
        return function() {
          for (;;) {
            var a = y(Yl);
            if (a) {
              if (hd(a)) {
                var b = hc(a), c = J(b), d = Kd(c);
                return function() {
                  for (var a = 0;;) {
                    if (a < c) {
                      var e = x.a(b, a), f = L.c(e, 0, null), h = L.c(e, 1, null), m = L.c(e, 2, null), n = L.c(e, 3, null), p = L.c(e, 4, null), s = L.c(e, 5, null), t = L.c(e, 6, null), z = L.c(e, 7, null), e = d;
                      q(h) ? f = new T(null, 2, 5, U, [Ej, f], null) : q(m) ? f = new T(null, 2, 5, U, [lh, f], null) : q(n) ? f = new T(null, 2, 5, U, [Vg, f], null) : q(p) ? f = new T(null, 2, 5, U, [Bi, f], null) : q(s) ? f = new T(null, 2, 5, U, [Nh, f], null) : q(t) ? q(lg(w, f)) ? f = new T(null, 2, 5, U, [bj, f], null) : (h = f, h = Rm.b ? Rm.b(h) : Rm.call(null, h), f = q(h) ? new T(null, 2, 5, U, [Rk, f], null) : new T(null, 2, 5, U, [wh, f], null)) : f = q(z) ? new T(null, 2, 5, U, [Nh, 
                      f], null) : null;
                      e.add(f);
                      a += 1;
                    } else {
                      return!0;
                    }
                  }
                }() ? Nd(d.N(), H(ic(a))) : Nd(d.N(), null);
              }
              var e = A(a), f = L.c(e, 0, null), h = L.c(e, 1, null), m = L.c(e, 2, null), n = L.c(e, 3, null), p = L.c(e, 4, null), s = L.c(e, 5, null), t = L.c(e, 6, null), e = L.c(e, 7, null);
              return I(q(h) ? new T(null, 2, 5, U, [Ej, f], null) : q(m) ? new T(null, 2, 5, U, [lh, f], null) : q(n) ? new T(null, 2, 5, U, [Vg, f], null) : q(p) ? new T(null, 2, 5, U, [Bi, f], null) : q(s) ? new T(null, 2, 5, U, [Nh, f], null) : q(t) ? q(lg(w, f)) ? new T(null, 2, 5, U, [bj, f], null) : q(function() {
                var a = f;
                return Rm.b ? Rm.b(a) : Rm.call(null, a);
              }()) ? new T(null, 2, 5, U, [Rk, f], null) : new T(null, 2, 5, U, [wh, f], null) : q(e) ? new T(null, 2, 5, U, [Nh, f], null) : null, H(C(a)));
            }
            return null;
          }
        };
      }(a, b, c, d, e, f, h, m, K, R, X, da, ta), null, null);
    };
  }(" \\t\\n", "[({", ")\\]}", b, ";.*", '"[^"]*"', c, d, e, f, h, m, /^:/)(ng(m, a));
}
;var Vm = Em.b(new Date), Wm = Em.b("#f34");
function Xm() {
  setTimeout(function() {
    var a = new Date;
    return Q.a ? Q.a(Vm, a) : Q.call(null, Vm, a);
  }, 100);
}
function Ym(a) {
  return new T(null, 2, 5, U, [ok, a], null);
}
function Zm() {
  Xm();
  var a = A(el.a((G.b ? G.b(Vm) : G.call(null, Vm)).toTimeString(), " "));
  return new T(null, 3, 5, U, [Og, new l(null, 1, [Qi, new l(null, 1, [ph, G.b ? G.b(Wm) : G.call(null, Wm)], null)], null), a], null);
}
function $m() {
  return new T(null, 3, 5, U, [mk, "Time color: ", new T(null, 2, 5, U, [jk, new l(null, 3, [ri, "text", Yh, G.b ? G.b(Wm) : G.call(null, Wm), rk, function(a) {
    a = a.target.value;
    return Q.a ? Q.a(Wm, a) : Q.call(null, Wm, a);
  }], null)], null)], null);
}
function an() {
  return new T(null, 4, 5, U, [Ri, new T(null, 2, 5, U, [Ym, "Hello world, it is now"], null), new T(null, 1, 5, U, [Zm], null), new T(null, 1, 5, U, [$m], null)], null);
}
ma("simpleexample.run", function() {
  function a() {
    return new T(null, 1, 5, U, [an], null);
  }
  var b = document.body;
  return Bm.a ? Bm.a(a, b) : Bm.call(null, a, b);
});
var bn;
a: {
  var cn = aa.navigator;
  if (cn) {
    var dn = cn.userAgent;
    if (dn) {
      bn = dn;
      break a;
    }
  }
  bn = "";
}
function en(a) {
  return-1 != bn.indexOf(a);
}
;var fn = en("Opera") || en("OPR"), gn = en("Trident") || en("MSIE"), hn = en("Gecko") && -1 == bn.toLowerCase().indexOf("webkit") && !(en("Trident") || en("MSIE")), jn = -1 != bn.toLowerCase().indexOf("webkit");
function kn() {
  var a = aa.document;
  return a ? a.documentMode : void 0;
}
var ln = function() {
  var a = "", b;
  if (fn && aa.opera) {
    return a = aa.opera.version, ca(a) ? a() : a;
  }
  hn ? b = /rv\:([^\);]+)(\)|;)/ : gn ? b = /\b(?:MSIE|rv)[: ]([^\);]+)(\)|;)/ : jn && (b = /WebKit\/(\S+)/);
  b && (a = (a = b.exec(bn)) ? a[1] : "");
  return gn && (b = kn(), b > parseFloat(a)) ? String(b) : a;
}(), mn = {};
function nn(a) {
  var b;
  if (!(b = mn[a])) {
    b = 0;
    for (var c = pa(String(ln)).split("."), d = pa(String(a)).split("."), e = Math.max(c.length, d.length), f = 0;0 == b && f < e;f++) {
      var h = c[f] || "", m = d[f] || "", n = RegExp("(\\d*)(\\D*)", "g"), p = RegExp("(\\d*)(\\D*)", "g");
      do {
        var s = n.exec(h) || ["", "", ""], t = p.exec(m) || ["", "", ""];
        if (0 == s[0].length && 0 == t[0].length) {
          break;
        }
        b = Ba(0 == s[1].length ? 0 : parseInt(s[1], 10), 0 == t[1].length ? 0 : parseInt(t[1], 10)) || Ba(0 == s[2].length, 0 == t[2].length) || Ba(s[2], t[2]);
      } while (0 == b);
    }
    b = mn[a] = 0 <= b;
  }
  return b;
}
var on = aa.document, pn = on && gn ? kn() || ("CSS1Compat" == on.compatMode ? parseInt(ln, 10) : 5) : void 0;
function qn(a) {
  a.prototype.then = a.prototype.then;
  a.prototype.$goog_Thenable = !0;
}
function rn(a) {
  if (!a) {
    return!1;
  }
  try {
    return!!a.$goog_Thenable;
  } catch (b) {
    return!1;
  }
}
;function sn(a) {
  aa.setTimeout(function() {
    throw a;
  }, 0);
}
var tn;
function un() {
  var a = aa.MessageChannel;
  "undefined" === typeof a && "undefined" !== typeof window && window.postMessage && window.addEventListener && (a = function() {
    var a = document.createElement("iframe");
    a.style.display = "none";
    a.src = "";
    document.documentElement.appendChild(a);
    var b = a.contentWindow, a = b.document;
    a.open();
    a.write("");
    a.close();
    var c = "callImmediate" + Math.random(), d = "file:" == b.location.protocol ? "*" : b.location.protocol + "//" + b.location.host, a = ja(function(a) {
      if (a.origin == d || a.data == c) {
        this.port1.onmessage();
      }
    }, this);
    b.addEventListener("message", a, !1);
    this.port1 = {};
    this.port2 = {postMessage:function() {
      b.postMessage(c, d);
    }};
  });
  if ("undefined" !== typeof a) {
    var b = new a, c = {}, d = c;
    b.port1.onmessage = function() {
      c = c.next;
      var a = c.Tc;
      c.Tc = null;
      a();
    };
    return function(a) {
      d.next = {Tc:a};
      d = d.next;
      b.port2.postMessage(0);
    };
  }
  return "undefined" !== typeof document && "onreadystatechange" in document.createElement("script") ? function(a) {
    var b = document.createElement("script");
    b.onreadystatechange = function() {
      b.onreadystatechange = null;
      b.parentNode.removeChild(b);
      b = null;
      a();
      a = null;
    };
    document.documentElement.appendChild(b);
  } : function(a) {
    aa.setTimeout(a, 0);
  };
}
;function vn(a, b) {
  wn || xn();
  yn || (wn(), yn = !0);
  zn.push(new An(a, b));
}
var wn;
function xn() {
  if (aa.Promise && aa.Promise.resolve) {
    var a = aa.Promise.resolve();
    wn = function() {
      a.then(Bn);
    };
  } else {
    wn = function() {
      var a = Bn;
      ca(aa.setImmediate) ? aa.setImmediate(a) : (tn || (tn = un()), tn(a));
    };
  }
}
var yn = !1, zn = [];
function Bn() {
  for (;zn.length;) {
    var a = zn;
    zn = [];
    for (var b = 0;b < a.length;b++) {
      var c = a[b];
      try {
        c.Na.call(c.scope);
      } catch (d) {
        sn(d);
      }
    }
  }
  yn = !1;
}
function An(a, b) {
  this.Na = a;
  this.scope = b;
}
;function Cn(a, b) {
  this.ia = Dn;
  this.qa = void 0;
  this.fa = this.na = null;
  this.bc = this.Gc = !1;
  try {
    var c = this;
    a.call(b, function(a) {
      En(c, Fn, a);
    }, function(a) {
      En(c, Gn, a);
    });
  } catch (d) {
    En(this, Gn, d);
  }
}
var Dn = 0, Fn = 2, Gn = 3;
Cn.prototype.then = function(a, b, c) {
  return Hn(this, ca(a) ? a : null, ca(b) ? b : null, c);
};
qn(Cn);
Cn.prototype.cancel = function(a) {
  this.ia == Dn && vn(function() {
    var b = new In(a);
    Jn(this, b);
  }, this);
};
function Jn(a, b) {
  if (a.ia == Dn) {
    if (a.na) {
      var c = a.na;
      if (c.fa) {
        for (var d = 0, e = -1, f = 0, h;h = c.fa[f];f++) {
          if (h = h.Qb) {
            if (d++, h == a && (e = f), 0 <= e && 1 < d) {
              break;
            }
          }
        }
        0 <= e && (c.ia == Dn && 1 == d ? Jn(c, b) : (d = c.fa.splice(e, 1)[0], Kn(c, d, Gn, b)));
      }
    } else {
      En(a, Gn, b);
    }
  }
}
function Ln(a, b) {
  a.fa && a.fa.length || a.ia != Fn && a.ia != Gn || Mn(a);
  a.fa || (a.fa = []);
  a.fa.push(b);
}
function Hn(a, b, c, d) {
  var e = {Qb:null, dd:null, ed:null};
  e.Qb = new Cn(function(a, h) {
    e.dd = b ? function(c) {
      try {
        var e = b.call(d, c);
        a(e);
      } catch (p) {
        h(p);
      }
    } : a;
    e.ed = c ? function(b) {
      try {
        var e = c.call(d, b);
        void 0 === e && b instanceof In ? h(b) : a(e);
      } catch (p) {
        h(p);
      }
    } : h;
  });
  e.Qb.na = a;
  Ln(a, e);
  return e.Qb;
}
Cn.prototype.ld = function(a) {
  this.ia = Dn;
  En(this, Fn, a);
};
Cn.prototype.md = function(a) {
  this.ia = Dn;
  En(this, Gn, a);
};
function En(a, b, c) {
  if (a.ia == Dn) {
    if (a == c) {
      b = Gn, c = new TypeError("Promise cannot resolve to itself");
    } else {
      if (rn(c)) {
        a.ia = 1;
        c.then(a.ld, a.md, a);
        return;
      }
      var d = typeof c;
      if ("object" == d && null != c || "function" == d) {
        try {
          var e = c.then;
          if (ca(e)) {
            Nn(a, c, e);
            return;
          }
        } catch (f) {
          b = Gn, c = f;
        }
      }
    }
    a.qa = c;
    a.ia = b;
    Mn(a);
    b != Gn || c instanceof In || On(a, c);
  }
}
function Nn(a, b, c) {
  function d(b) {
    f || (f = !0, a.md(b));
  }
  function e(b) {
    f || (f = !0, a.ld(b));
  }
  a.ia = 1;
  var f = !1;
  try {
    c.call(b, e, d);
  } catch (h) {
    d(h);
  }
}
function Mn(a) {
  a.Gc || (a.Gc = !0, vn(a.Gd, a));
}
Cn.prototype.Gd = function() {
  for (;this.fa && this.fa.length;) {
    var a = this.fa;
    this.fa = [];
    for (var b = 0;b < a.length;b++) {
      Kn(this, a[b], this.ia, this.qa);
    }
  }
  this.Gc = !1;
};
function Kn(a, b, c, d) {
  if (c == Fn) {
    b.dd(d);
  } else {
    for (;a && a.bc;a = a.na) {
      a.bc = !1;
    }
    b.ed(d);
  }
}
function On(a, b) {
  a.bc = !0;
  vn(function() {
    a.bc && Pn.call(null, b);
  });
}
var Pn = sn;
function In(a) {
  Ha.call(this, a);
}
na(In, Ha);
In.prototype.name = "cancel";
/*
 Portions of this code are from MochiKit, received by
 The Closure Authors under the MIT license. All other code is Copyright
 2005-2009 The Closure Authors. All Rights Reserved.
*/
function Qn(a, b) {
  this.mc = [];
  this.cd = a;
  this.$c = b || null;
  this.gb = this.Wa = !1;
  this.qa = void 0;
  this.Nc = this.rd = this.rc = !1;
  this.oc = 0;
  this.na = null;
  this.tc = 0;
}
Qn.prototype.cancel = function(a) {
  if (this.Wa) {
    this.qa instanceof Qn && this.qa.cancel();
  } else {
    if (this.na) {
      var b = this.na;
      delete this.na;
      a ? b.cancel(a) : (b.tc--, 0 >= b.tc && b.cancel());
    }
    this.cd ? this.cd.call(this.$c, this) : this.Nc = !0;
    this.Wa || (a = new Rn, this.Pb(), this.Wa = !0, this.qa = a, this.gb = !0, Sn(this));
  }
};
Qn.prototype.Zc = function(a, b) {
  this.rc = !1;
  this.Wa = !0;
  this.qa = b;
  this.gb = !a;
  Sn(this);
};
Qn.prototype.Pb = function() {
  if (this.Wa) {
    if (!this.Nc) {
      throw new Tn;
    }
    this.Nc = !1;
  }
};
function Un(a, b, c) {
  a.mc.push([b, c, void 0]);
  a.Wa && Sn(a);
}
Qn.prototype.then = function(a, b, c) {
  var d, e, f = new Cn(function(a, b) {
    d = a;
    e = b;
  });
  Un(this, d, function(a) {
    a instanceof Rn ? f.cancel() : e(a);
  });
  return f.then(a, b, c);
};
qn(Qn);
function Vn(a) {
  return La(a.mc, function(a) {
    return ca(a[1]);
  });
}
function Sn(a) {
  if (a.oc && a.Wa && Vn(a)) {
    var b = a.oc, c = Wn[b];
    c && (aa.clearTimeout(c.dc), delete Wn[b]);
    a.oc = 0;
  }
  a.na && (a.na.tc--, delete a.na);
  for (var b = a.qa, d = c = !1;a.mc.length && !a.rc;) {
    var e = a.mc.shift(), f = e[0], h = e[1], e = e[2];
    if (f = a.gb ? h : f) {
      try {
        var m = f.call(e || a.$c, b);
        void 0 !== m && (a.gb = a.gb && (m == b || m instanceof Error), a.qa = b = m);
        rn(b) && (d = !0, a.rc = !0);
      } catch (n) {
        b = n, a.gb = !0, Vn(a) || (c = !0);
      }
    }
  }
  a.qa = b;
  d && (m = ja(a.Zc, a, !0), d = ja(a.Zc, a, !1), b instanceof Qn ? (Un(b, m, d), b.rd = !0) : b.then(m, d));
  c && (b = new Xn(b), Wn[b.dc] = b, a.oc = b.dc);
}
function Tn() {
  Ha.call(this);
}
na(Tn, Ha);
Tn.prototype.message = "Deferred has already fired";
Tn.prototype.name = "AlreadyCalledError";
function Rn() {
  Ha.call(this);
}
na(Rn, Ha);
Rn.prototype.message = "Deferred was canceled";
Rn.prototype.name = "CanceledError";
function Xn(a) {
  this.dc = aa.setTimeout(ja(this.Pd, this), 0);
  this.Fd = a;
}
Xn.prototype.Pd = function() {
  delete Wn[this.dc];
  throw this.Fd;
};
var Wn = {};
!hn && !gn || gn && gn && 9 <= pn || hn && nn("1.9.1");
gn && nn("9");
function Yn(a) {
  var b = document;
  return ba(a) ? b.getElementById(a) : a;
}
function Zn(a) {
  return a.contentDocument || a.contentWindow.document;
}
;function $n() {
  0 != ao && (bo[ea(this)] = this);
}
var ao = 0, bo = {};
$n.prototype.ad = !1;
$n.prototype.ac = function() {
  if (!this.ad && (this.ad = !0, this.ha(), 0 != ao)) {
    var a = ea(this);
    delete bo[a];
  }
};
$n.prototype.ha = function() {
  if (this.Gb) {
    for (;this.Gb.length;) {
      this.Gb.shift()();
    }
  }
};
function co(a) {
  a && "function" == typeof a.ac && a.ac();
}
;var eo = !gn || gn && 9 <= pn, fo = gn && !nn("9");
!jn || nn("528");
hn && nn("1.9b") || gn && nn("8") || fn && nn("9.5") || jn && nn("528");
hn && !nn("8") || gn && nn("9");
function go(a, b) {
  this.type = a;
  this.currentTarget = this.target = b;
  this.defaultPrevented = this.mb = !1;
  this.jd = !0;
}
go.prototype.ha = function() {
};
go.prototype.ac = function() {
};
go.prototype.preventDefault = function() {
  this.defaultPrevented = !0;
  this.jd = !1;
};
function ho(a) {
  ho[" "](a);
  return a;
}
ho[" "] = function() {
};
function io(a, b) {
  go.call(this, a ? a.type : "");
  this.relatedTarget = this.currentTarget = this.target = null;
  this.charCode = this.keyCode = this.button = this.screenY = this.screenX = this.clientY = this.clientX = this.offsetY = this.offsetX = 0;
  this.metaKey = this.shiftKey = this.altKey = this.ctrlKey = !1;
  this.Fc = this.state = null;
  if (a) {
    var c = this.type = a.type;
    this.target = a.target || a.srcElement;
    this.currentTarget = b;
    var d = a.relatedTarget;
    if (d) {
      if (hn) {
        var e;
        a: {
          try {
            ho(d.nodeName);
            e = !0;
            break a;
          } catch (f) {
          }
          e = !1;
        }
        e || (d = null);
      }
    } else {
      "mouseover" == c ? d = a.fromElement : "mouseout" == c && (d = a.toElement);
    }
    this.relatedTarget = d;
    this.offsetX = jn || void 0 !== a.offsetX ? a.offsetX : a.layerX;
    this.offsetY = jn || void 0 !== a.offsetY ? a.offsetY : a.layerY;
    this.clientX = void 0 !== a.clientX ? a.clientX : a.pageX;
    this.clientY = void 0 !== a.clientY ? a.clientY : a.pageY;
    this.screenX = a.screenX || 0;
    this.screenY = a.screenY || 0;
    this.button = a.button;
    this.keyCode = a.keyCode || 0;
    this.charCode = a.charCode || ("keypress" == c ? a.keyCode : 0);
    this.ctrlKey = a.ctrlKey;
    this.altKey = a.altKey;
    this.shiftKey = a.shiftKey;
    this.metaKey = a.metaKey;
    this.state = a.state;
    this.Fc = a;
    a.defaultPrevented && this.preventDefault();
  }
}
na(io, go);
io.prototype.preventDefault = function() {
  io.Jb.preventDefault.call(this);
  var a = this.Fc;
  if (a.preventDefault) {
    a.preventDefault();
  } else {
    if (a.returnValue = !1, fo) {
      try {
        if (a.ctrlKey || 112 <= a.keyCode && 123 >= a.keyCode) {
          a.keyCode = -1;
        }
      } catch (b) {
      }
    }
  }
};
io.prototype.ha = function() {
};
var jo = "closure_listenable_" + (1E6 * Math.random() | 0), ko = 0;
function lo(a, b, c, d, e) {
  this.Za = a;
  this.kc = null;
  this.src = b;
  this.type = c;
  this.Ob = !!d;
  this.cc = e;
  this.key = ++ko;
  this.nb = this.Nb = !1;
}
function mo(a) {
  a.nb = !0;
  a.Za = null;
  a.kc = null;
  a.src = null;
  a.cc = null;
}
;function no(a) {
  this.src = a;
  this.ba = {};
  this.Kb = 0;
}
no.prototype.add = function(a, b, c, d, e) {
  var f = a.toString();
  a = this.ba[f];
  a || (a = this.ba[f] = [], this.Kb++);
  var h = oo(a, b, d, e);
  -1 < h ? (b = a[h], c || (b.Nb = !1)) : (b = new lo(b, this.src, f, !!d, e), b.Nb = c, a.push(b));
  return b;
};
no.prototype.remove = function(a, b, c, d) {
  a = a.toString();
  if (!(a in this.ba)) {
    return!1;
  }
  var e = this.ba[a];
  b = oo(e, b, c, d);
  return-1 < b ? (mo(e[b]), Ia.splice.call(e, b, 1), 0 == e.length && (delete this.ba[a], this.Kb--), !0) : !1;
};
function po(a, b) {
  var c = b.type;
  if (!(c in a.ba)) {
    return!1;
  }
  var d = a.ba[c], e = Ja(d, b), f;
  (f = 0 <= e) && Ia.splice.call(d, e, 1);
  f && (mo(b), 0 == a.ba[c].length && (delete a.ba[c], a.Kb--));
  return f;
}
no.prototype.lc = function(a) {
  a = a && a.toString();
  var b = 0, c;
  for (c in this.ba) {
    if (!a || c == a) {
      for (var d = this.ba[c], e = 0;e < d.length;e++) {
        ++b, mo(d[e]);
      }
      delete this.ba[c];
      this.Kb--;
    }
  }
  return b;
};
no.prototype.Bb = function(a, b, c, d) {
  a = this.ba[a.toString()];
  var e = -1;
  a && (e = oo(a, b, c, d));
  return-1 < e ? a[e] : null;
};
function oo(a, b, c, d) {
  for (var e = 0;e < a.length;++e) {
    var f = a[e];
    if (!f.nb && f.Za == b && f.Ob == !!c && f.cc == d) {
      return e;
    }
  }
  return-1;
}
;var qo = "closure_lm_" + (1E6 * Math.random() | 0), ro = {}, so = 0;
function to(a, b, c, d, e) {
  if ("array" == k(b)) {
    for (var f = 0;f < b.length;f++) {
      to(a, b[f], c, d, e);
    }
    return null;
  }
  c = uo(c);
  if (a && a[jo]) {
    a = a.Ya(b, c, d, e);
  } else {
    if (!b) {
      throw Error("Invalid event type");
    }
    var f = !!d, h = vo(a);
    h || (a[qo] = h = new no(a));
    c = h.add(b, c, !1, d, e);
    c.kc || (d = wo(), c.kc = d, d.src = a, d.Za = c, a.addEventListener ? a.addEventListener(b.toString(), d, f) : a.attachEvent(xo(b.toString()), d), so++);
    a = c;
  }
  return a;
}
function wo() {
  var a = yo, b = eo ? function(c) {
    return a.call(b.src, b.Za, c);
  } : function(c) {
    c = a.call(b.src, b.Za, c);
    if (!c) {
      return c;
    }
  };
  return b;
}
function zo(a, b, c, d, e) {
  if ("array" == k(b)) {
    for (var f = 0;f < b.length;f++) {
      zo(a, b[f], c, d, e);
    }
    return null;
  }
  c = uo(c);
  if (a && a[jo]) {
    return a.Oc(b, c, d, e);
  }
  if (!a) {
    return!1;
  }
  if (a = vo(a)) {
    if (b = a.Bb(b, c, !!d, e)) {
      return Ao(b);
    }
  }
  return!1;
}
function Ao(a) {
  if ("number" == typeof a || !a || a.nb) {
    return!1;
  }
  var b = a.src;
  if (b && b[jo]) {
    return po(b.Ma, a);
  }
  var c = a.type, d = a.kc;
  b.removeEventListener ? b.removeEventListener(c, d, a.Ob) : b.detachEvent && b.detachEvent(xo(c), d);
  so--;
  (c = vo(b)) ? (po(c, a), 0 == c.Kb && (c.src = null, b[qo] = null)) : mo(a);
  return!0;
}
function xo(a) {
  return a in ro ? ro[a] : ro[a] = "on" + a;
}
function Bo(a, b, c, d) {
  var e = 1;
  if (a = vo(a)) {
    if (b = a.ba[b.toString()]) {
      for (b = b.concat(), a = 0;a < b.length;a++) {
        var f = b[a];
        f && f.Ob == c && !f.nb && (e &= !1 !== Co(f, d));
      }
    }
  }
  return Boolean(e);
}
function Co(a, b) {
  var c = a.Za, d = a.cc || a.src;
  a.Nb && Ao(a);
  return c.call(d, b);
}
function yo(a, b) {
  if (a.nb) {
    return!0;
  }
  if (!eo) {
    var c;
    if (!(c = b)) {
      a: {
        c = ["window", "event"];
        for (var d = aa, e;e = c.shift();) {
          if (null != d[e]) {
            d = d[e];
          } else {
            c = null;
            break a;
          }
        }
        c = d;
      }
    }
    e = c;
    c = new io(e, this);
    d = !0;
    if (!(0 > e.keyCode || void 0 != e.returnValue)) {
      a: {
        var f = !1;
        if (0 == e.keyCode) {
          try {
            e.keyCode = -1;
            break a;
          } catch (h) {
            f = !0;
          }
        }
        if (f || void 0 == e.returnValue) {
          e.returnValue = !0;
        }
      }
      e = [];
      for (f = c.currentTarget;f;f = f.parentNode) {
        e.push(f);
      }
      for (var f = a.type, m = e.length - 1;!c.mb && 0 <= m;m--) {
        c.currentTarget = e[m], d &= Bo(e[m], f, !0, c);
      }
      for (m = 0;!c.mb && m < e.length;m++) {
        c.currentTarget = e[m], d &= Bo(e[m], f, !1, c);
      }
    }
    return d;
  }
  return Co(a, new io(b, this));
}
function vo(a) {
  a = a[qo];
  return a instanceof no ? a : null;
}
var Do = "__closure_events_fn_" + (1E9 * Math.random() >>> 0);
function uo(a) {
  if (ca(a)) {
    return a;
  }
  a[Do] || (a[Do] = function(b) {
    return a.handleEvent(b);
  });
  return a[Do];
}
;function Eo() {
  $n.call(this);
  this.Ma = new no(this);
  this.qd = this;
  this.Ic = null;
}
na(Eo, $n);
Eo.prototype[jo] = !0;
g = Eo.prototype;
g.addEventListener = function(a, b, c, d) {
  to(this, a, b, c, d);
};
g.removeEventListener = function(a, b, c, d) {
  zo(this, a, b, c, d);
};
g.dispatchEvent = function(a) {
  var b, c = this.Ic;
  if (c) {
    for (b = [];c;c = c.Ic) {
      b.push(c);
    }
  }
  var c = this.qd, d = a.type || a;
  if (ba(a)) {
    a = new go(a, c);
  } else {
    if (a instanceof go) {
      a.target = a.target || c;
    } else {
      var e = a;
      a = new go(d, c);
      Ea(a, e);
    }
  }
  var e = !0, f;
  if (b) {
    for (var h = b.length - 1;!a.mb && 0 <= h;h--) {
      f = a.currentTarget = b[h], e = Fo(f, d, !0, a) && e;
    }
  }
  a.mb || (f = a.currentTarget = c, e = Fo(f, d, !0, a) && e, a.mb || (e = Fo(f, d, !1, a) && e));
  if (b) {
    for (h = 0;!a.mb && h < b.length;h++) {
      f = a.currentTarget = b[h], e = Fo(f, d, !1, a) && e;
    }
  }
  return e;
};
g.ha = function() {
  Eo.Jb.ha.call(this);
  this.Ma && this.Ma.lc(void 0);
  this.Ic = null;
};
g.Ya = function(a, b, c, d) {
  return this.Ma.add(String(a), b, !1, c, d);
};
g.Oc = function(a, b, c, d) {
  return this.Ma.remove(String(a), b, c, d);
};
function Fo(a, b, c, d) {
  b = a.Ma.ba[String(b)];
  if (!b) {
    return!0;
  }
  b = b.concat();
  for (var e = !0, f = 0;f < b.length;++f) {
    var h = b[f];
    if (h && !h.nb && h.Ob == c) {
      var m = h.Za, n = h.cc || h.src;
      h.Nb && po(a.Ma, h);
      e = !1 !== m.call(n, d) && e;
    }
  }
  return e && !1 != d.jd;
}
g.Bb = function(a, b, c, d) {
  return this.Ma.Bb(String(a), b, c, d);
};
function Go(a, b) {
  Eo.call(this);
  this.Fb = a || 1;
  this.pb = b || aa;
  this.sc = ja(this.Qd, this);
  this.Hc = la();
}
na(Go, Eo);
g = Go.prototype;
g.enabled = !1;
g.Q = null;
g.setInterval = function(a) {
  this.Fb = a;
  this.Q && this.enabled ? (this.stop(), this.start()) : this.Q && this.stop();
};
g.Qd = function() {
  if (this.enabled) {
    var a = la() - this.Hc;
    0 < a && a < .8 * this.Fb ? this.Q = this.pb.setTimeout(this.sc, this.Fb - a) : (this.Q && (this.pb.clearTimeout(this.Q), this.Q = null), this.dispatchEvent(Ho), this.enabled && (this.Q = this.pb.setTimeout(this.sc, this.Fb), this.Hc = la()));
  }
};
g.start = function() {
  this.enabled = !0;
  this.Q || (this.Q = this.pb.setTimeout(this.sc, this.Fb), this.Hc = la());
};
g.stop = function() {
  this.enabled = !1;
  this.Q && (this.pb.clearTimeout(this.Q), this.Q = null);
};
g.ha = function() {
  Go.Jb.ha.call(this);
  this.stop();
  delete this.pb;
};
var Ho = "tick";
function Io(a) {
  $n.call(this);
  this.bd = a;
  this.gc = {};
}
na(Io, $n);
var Jo = [];
g = Io.prototype;
g.Ya = function(a, b, c, d) {
  "array" != k(b) && (b && (Jo[0] = b.toString()), b = Jo);
  for (var e = 0;e < b.length;e++) {
    var f = to(a, b[e], c || this.handleEvent, d || !1, this.bd || this);
    if (!f) {
      break;
    }
    this.gc[f.key] = f;
  }
  return this;
};
g.Oc = function(a, b, c, d, e) {
  if ("array" == k(b)) {
    for (var f = 0;f < b.length;f++) {
      this.Oc(a, b[f], c, d, e);
    }
  } else {
    c = c || this.handleEvent, e = e || this.bd || this, c = uo(c), d = !!d, b = a && a[jo] ? a.Bb(b, c, d, e) : a ? (a = vo(a)) ? a.Bb(b, c, d, e) : null : null, b && (Ao(b), delete this.gc[b.key]);
  }
  return this;
};
g.lc = function() {
  Ca(this.gc, Ao);
  this.gc = {};
};
g.ha = function() {
  Io.Jb.ha.call(this);
  this.lc();
};
g.handleEvent = function() {
  throw Error("EventHandler.handleEvent not implemented");
};
function Ko(a) {
  go.call(this, "navigate");
  this.Rd = a;
}
na(Ko, go);
function Lo() {
  return!(en("iPad") || en("Android") && !en("Mobile") || en("Silk")) && (en("iPod") || en("iPhone") || en("Android") || en("IEMobile"));
}
;function Mo(a, b) {
  for (var c = [a], d = b.length - 1;0 <= d;--d) {
    c.push(typeof b[d], b[d]);
  }
  return c.join("\x0B");
}
;function No(a, b, c, d) {
  Eo.call(this);
  if (a && !b) {
    throw Error("Can't use invisible history without providing a blank page.");
  }
  var e;
  c ? e = c : (e = "history_state" + Oo, document.write(oa(Po, e, e)), e = Yn(e));
  this.Cb = e;
  c = c ? (c = 9 == c.nodeType ? c : c.ownerDocument || c.document) ? c.parentWindow || c.defaultView : window : window;
  this.M = c;
  this.ec = b;
  gn && !b && (this.ec = "https" == window.location.protocol ? "https:///" : 'javascript:""');
  this.Q = new Go(Qo);
  b = ka(co, this.Q);
  this.Gb || (this.Gb = []);
  this.Gb.push(b);
  this.$a = !a;
  this.Va = new Io(this);
  if (a || Ro) {
    d ? a = d : (a = "history_iframe" + Oo, d = this.ec ? 'src\x3d"' + qa(this.ec) + '"' : "", document.write(oa(So, a, d)), a = Yn(a)), this.fc = a, this.nd = !0;
  }
  Ro && (this.Va.Ya(this.M, "load", this.Kd), this.kd = this.Ec = !1);
  this.$a ? To(this, this.oa(), !0) : Uo(this, this.Cb.value);
  Oo++;
}
na(No, Eo);
No.prototype.Ga = !1;
No.prototype.lb = !1;
No.prototype.ib = null;
var Vo = function(a, b) {
  var c = b || Mo;
  return function() {
    var b = this || aa, b = b.closure_memoize_cache_ || (b.closure_memoize_cache_ = {}), e = c(ea(a), arguments);
    return b.hasOwnProperty(e) ? b[e] : b[e] = a.apply(this, arguments);
  };
}(function() {
  return gn ? 8 <= document.documentMode : "onhashchange" in aa;
}), Ro = gn && !(gn && 8 <= pn);
g = No.prototype;
g.kb = null;
g.ha = function() {
  No.Jb.ha.call(this);
  this.Va.ac();
  this.ob(!1);
};
g.ob = function(a) {
  if (a != this.Ga) {
    if (Ro && !this.Ec) {
      this.kd = a;
    } else {
      if (a) {
        if (fn ? this.Va.Ya(this.M.document, Wo, this.Nd) : hn && this.Va.Ya(this.M, "pageshow", this.Md), Vo() && this.$a) {
          this.Va.Ya(this.M, "hashchange", this.Ld), this.Ga = !0, this.dispatchEvent(new Ko(this.oa()));
        } else {
          if (!gn || Lo() || this.Ec) {
            this.Va.Ya(this.Q, Ho, ja(this.Pb, this, !0)), this.Ga = !0, Ro || (this.ib = this.oa(), this.dispatchEvent(new Ko(this.oa()))), this.Q.start();
          }
        }
      } else {
        this.Ga = !1, this.Va.lc(), this.Q.stop();
      }
    }
  }
};
g.Kd = function() {
  this.Ec = !0;
  this.Cb.value && Uo(this, this.Cb.value, !0);
  this.ob(this.kd);
};
g.Md = function(a) {
  a.Fc.persisted && (this.ob(!1), this.ob(!0));
};
g.Ld = function() {
  var a = Xo(this.M);
  a != this.ib && Yo(this, a);
};
g.oa = function() {
  return null != this.kb ? this.kb : this.$a ? Xo(this.M) : Zo(this) || "";
};
g.Mc = function(a, b) {
  this.oa() != a && (this.$a ? (To(this, a, !1), Vo() || gn && !Lo() && Uo(this, a, !1, b), this.Ga && this.Pb(!1)) : (Uo(this, a, !1), this.kb = this.ib = this.Cb.value = a, this.dispatchEvent(new Ko(a))));
};
function Xo(a) {
  a = a.location.href;
  var b = a.indexOf("#");
  return 0 > b ? "" : a.substring(b + 1);
}
function To(a, b, c) {
  a = a.M.location;
  var d = a.href.split("#")[0], e = -1 != a.href.indexOf("#");
  if (Ro || e || b) {
    d += "#" + b;
  }
  d != a.href && (c ? a.replace(d) : a.href = d);
}
function Uo(a, b, c, d) {
  if (a.nd || b != Zo(a)) {
    if (a.nd = !1, b = encodeURIComponent(String(b)), gn) {
      var e = Zn(a.fc);
      e.open("text/html", c ? "replace" : void 0);
      e.write(oa($o, qa(d || a.M.document.title), b));
      e.close();
    } else {
      if (b = a.ec + "#" + b, a = a.fc.contentWindow) {
        c ? a.location.replace(b) : a.location.href = b;
      }
    }
  }
}
function Zo(a) {
  if (gn) {
    return a = Zn(a.fc), a.body ? decodeURIComponent(a.body.innerHTML.replace(/\+/g, " ")) : null;
  }
  var b = a.fc.contentWindow;
  if (b) {
    var c;
    try {
      c = decodeURIComponent(Xo(b).replace(/\+/g, " "));
    } catch (d) {
      return a.lb || (!0 != a.lb && a.Q.setInterval(ap), a.lb = !0), null;
    }
    a.lb && (!1 != a.lb && a.Q.setInterval(Qo), a.lb = !1);
    return c || null;
  }
  return null;
}
g.Pb = function() {
  if (this.$a) {
    var a = Xo(this.M);
    a != this.ib && Yo(this, a);
  }
  if (!this.$a || Ro) {
    if (a = Zo(this) || "", null == this.kb || a == this.kb) {
      this.kb = null, a != this.ib && Yo(this, a);
    }
  }
};
function Yo(a, b) {
  a.ib = a.Cb.value = b;
  a.$a ? (Ro && Uo(a, b), To(a, b)) : Uo(a, b);
  a.dispatchEvent(new Ko(a.oa()));
}
g.Nd = function() {
  this.Q.stop();
  this.Q.start();
};
var Wo = ["mousedown", "keydown", "mousemove"], $o = "\x3ctitle\x3e%s\x3c/title\x3e\x3cbody\x3e%s\x3c/body\x3e", So = '\x3ciframe id\x3d"%s" style\x3d"display:none" %s\x3e\x3c/iframe\x3e', Po = '\x3cinput type\x3d"text" name\x3d"%s" id\x3d"%s" style\x3d"display:none"\x3e', Oo = 0, Qo = 150, ap = 1E4;
function bp(a, b) {
  Eo.call(this);
  this.M = a || window;
  this.nc = b || null;
  to(this.M, "popstate", this.Hb, !1, this);
  to(this.M, "hashchange", this.Hb, !1, this);
}
na(bp, Eo);
g = bp.prototype;
g.Ga = !1;
g.Lb = !0;
g.jc = "/";
g.ob = function(a) {
  a != this.Ga && (this.Ga = a) && this.dispatchEvent(new Ko(this.oa()));
};
g.oa = function() {
  if (this.Lb) {
    var a = this.M.location.href, b = a.indexOf("#");
    return 0 > b ? "" : a.substring(b + 1);
  }
  return this.nc ? this.nc.fe(this.jc, this.M.location) : this.M.location.pathname.substr(this.jc.length);
};
g.Mc = function(a, b) {
  a != this.oa() && (this.M.history.pushState(null, b || this.M.document.title || "", this.Lb ? "#" + a : this.nc ? this.nc.ee(a, this.jc, this.M.location) : this.jc + a + this.M.location.search), this.dispatchEvent(new Ko(a)));
};
g.ha = function() {
  zo(this.M, "popstate", this.Hb, !1, this);
  this.Lb && zo(this.M, "hashchange", this.Hb, !1, this);
};
g.Hb = function() {
  this.Ga && this.dispatchEvent(new Ko(this.oa()));
};
if ("undefined" === typeof $) {
  var $ = Em.b(Xc([Ng, ah, rh, Ah, qi, nj, Xj, yk, zk], [new T(null, 1, 5, U, ["site/public/css/main.css"], null), "js/main.js", !1, "js/out", new l(null, 1, ["index.html", function() {
    return new T(null, 2, 5, U, [Ri, "Empty"], null);
  }], null), df, "css/built.css", function() {
    return new T(null, 2, 5, U, [Ri, cp.o ? cp.o() : cp.call(null)], null);
  }, "outsite/public"]))
}
if ("undefined" === typeof dp) {
  var dp = Em.b("index.html")
}
if ("undefined" === typeof ep) {
  var ep = Em.b(new l(null, 1, [Si, !1], null))
}
var fp = function() {
  function a(a, b, c) {
    S.e($, we, new T(null, 1, 5, U, [qi], null), N, E([a, b], 0));
    return S.e($, we, new T(null, 1, 5, U, [nj], null), N, E([a, c], 0));
  }
  function b(a, b) {
    return c.c(a, b, null);
  }
  var c = null, c = function(c, e, f) {
    switch(arguments.length) {
      case 2:
        return b.call(this, c, e);
      case 3:
        return a.call(this, c, e, f);
    }
    throw Error("Invalid arity: " + arguments.length);
  };
  c.a = b;
  c.c = a;
  return c;
}();
function gp(a, b) {
  var c = Ok.b(a), d = qi.b(G.b ? G.b($) : G.call(null, $)).call(null, c);
  return new T(null, 3, 5, U, [Sk, N.e(a, Ok, hp.b ? hp.b(c) : hp.call(null, c), E([Ii, q(Si.b(G.b ? G.b(ep) : G.call(null, ep))) ? function(a, b) {
    return function(c) {
      c.preventDefault();
      Q.a ? Q.a(dp, a) : Q.call(null, dp, a);
      return Fm(function() {
        return function() {
          return document.body.scrollTop = 0;
        };
      }(a, b));
    };
  }(c, d) : td], 0)), b], null);
}
function cp() {
  return new T(null, 1, 5, U, [te.c(G.b ? G.b($) : G.call(null, $), new T(null, 2, 5, U, [qi, G.b ? G.b(dp) : G.call(null, dp)], null), te.a(G.b ? G.b($) : G.call(null, $), new T(null, 2, 5, U, [qi, "index.html"], null)))], null);
}
function ip() {
  return te.c(G.b ? G.b($) : G.call(null, $), new T(null, 2, 5, U, [nj, G.b ? G.b(dp) : G.call(null, dp)], null), function() {
    var a = te.a(G.b ? G.b($) : G.call(null, $), new T(null, 2, 5, U, [nj, "index.html"], null));
    return q(a) ? a : "";
  }());
}
$b(dp, Zh, function() {
  return q(gl) ? document.title = ip() : null;
});
function jp() {
  if (q(gl)) {
    var a = window.location.protocol, b = rh.b(G.b ? G.b($) : G.call(null, $));
    return q(b) ? (b = window, b = !(!b.history || !b.history.pushState), q(b) ? (new dg(null, new l(null, 2, ["https:", null, "http:", null], null), null)).call(null, a) : b) : b;
  }
  return null;
}
function kp() {
  if (q(jp())) {
    var a = new bp;
    !1 != a.Lb && (zo(a.M, "hashchange", a.Hb, !1, a), a.Lb = !1);
    return a;
  }
  a = new No;
  a.Mc(G.b ? G.b(dp) : G.call(null, dp));
  return a;
}
var lp = null;
function mp() {
  return q(jp()) ? uj.b(G.b ? G.b($) : G.call(null, $)) : null;
}
function np() {
  if (null == lp) {
    lp = kp();
    S.j(ep, N, Si, null != lp);
    var a = lp;
    q(a) && (to(a, "navigate", function() {
      return function(a) {
        a = a.Rd;
        var c = mp();
        a = q(q(c) ? 0 === a.indexOf(c) : c) ? Ad.a(a, J(c)) : a;
        Q.a ? Q.a(dp, a) : Q.call(null, dp, a);
        return Ml(Ll);
      };
    }(a, "navigate", a, a)), yg(dp, Oh, function(a) {
      return function(c, d, e, f) {
        return Ac.a(e, f) ? null : a.Mc("" + v.b(mp()) + v.b(f));
      };
    }(a, a)), a.ob(!0));
  }
}
function op(a) {
  var b = window.location.pathname, c = /.[^\/]*/;
  a: {
    switch(a) {
      case "":
        a = ".";
        break a;
    }
  }
  a = J(ng(c, a));
  b = O.a(v, ke.a(a, ng(c, b)));
  return al("" + v.b(b) + "/", /^\//, "");
}
function hp(a) {
  var b = J(ng(/\//, G.b ? G.b(dp) : G.call(null, dp)));
  return "" + v.b(O.a(v, le.a(b, "../"))) + v.b(a);
}
function pp() {
  var a = yk.b(G.b ? G.b($) : G.call(null, $));
  return new T(null, 1, 5, U, [a], null);
}
function qp(a, b) {
  return new T(null, 2, 5, U, [a, new l(null, 1, [Tk, new l(null, 1, [gk, b], null)], null)], null);
}
function rp(a) {
  var b = md(a) ? O.a(ee, a) : a;
  M.a(b, Zi);
  M.a(b, hj);
  var c = M.a(b, Gk);
  a = M.a(b, uk);
  var d = M.a(b, yk), b = M.a(b, Mi), e = G.b ? G.b($) : G.call(null, $), f = hp("" + v.b(Ah.b(e)) + "/goog/base.js"), h = "" + v.b(hp(ah.b(e))) + v.b(a), m = hp(Xj.b(e)), e = hj.b(e);
  return Dm(new T(null, 3, 5, U, [Xk, new T(null, 5, 5, U, [Ui, new T(null, 2, 5, U, [Ra, new l(null, 1, [Hi, "utf-8"], null)], null), new T(null, 2, 5, U, [Ra, new l(null, 2, [Th, new Cc(null, "viewport", "viewport", 2083874242, null), Jh, "width\x3ddevice-width, initial-scale\x3d1.0"], null)], null), new T(null, 2, 5, U, [mj, new l(null, 2, [Ok, "" + v.b(m) + v.b(a), Pg, new Cc(null, "stylesheet", "stylesheet", -152080899, null)], null)], null), new T(null, 2, 5, U, [Mi, b], null)], null), new T(null, 
  6, 5, U, [yk, qp(Ri, d), qp(Dh, "var pageConfig \x3d " + v.b(function() {
    var a = Fg(c);
    return JSON.stringify(a);
  }())), q(e) ? new T(null, 2, 5, U, [Dh, new l(null, 2, [si, f, ri, "text/javascript"], null)], null) : null, new T(null, 2, 5, U, [Dh, new l(null, 2, [si, h, ri, "text/javascript"], null)], null), q(e) ? qp(Dh, "goog.require('devsetup');") : null], null)], null));
}
function sp(a, b) {
  Q.a ? Q.a(dp, a) : Q.call(null, dp, a);
  var c;
  c = pp();
  c = Cm.b ? Cm.b(c) : Cm.call(null, c);
  return "\x3c!doctype html\x3e" + v.b(rp(new l(null, 4, [Mi, ip(), yk, c, Gk, new l(null, 2, [rh, !0, bh, a], null), uk, b], null)));
}
function tp(a) {
  for (var b = require("fs"), c = require("path"), d = function() {
    var b = c.dirname(a), b = c.normalize(b);
    return el.a(b, /\//);
  }(), d = ig.a(function() {
    return function(a, b) {
      return "" + v.b(a) + "/" + v.b(b);
    };
  }(b, c, d), d), d = y(d), e = null, f = 0, h = 0;;) {
    if (h < f) {
      var m = e.C(null, h);
      q(b.existsSync(m)) || b.mkdirSync(m);
      h += 1;
    } else {
      if (d = y(d)) {
        e = d, hd(e) ? (d = hc(e), f = ic(e), e = d, m = J(d), d = f, f = m) : (m = A(e), q(b.existsSync(m)) || b.mkdirSync(m), d = D(e), e = null, f = 0), h = 0;
      } else {
        break;
      }
    }
  }
}
function up(a, b) {
  var c = require("fs");
  tp(a);
  c.writeFileSync(a, b);
}
function vp(a) {
  return require("fs").readFileSync(a);
}
var wp = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    var b = require("path");
    return O.a(b.join, a);
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
ma("sitetools.genpages", function(a) {
  "undefined" !== typeof console && console.log("Generating site");
  S.c($, cg, Kg.e(a, E([Jg, !0], 0)));
  a = zk.b(G.b ? G.b($) : G.call(null, $));
  for (var b = Em.b(fg), c = "?" + v.b(Date.now()), d = function(a, b, c) {
    return function() {
      return A(qe.a(function(a, b) {
        return function(a) {
          return null == (G.b ? G.b(b) : G.call(null, b)).call(null, a);
        };
      }(a, b, c), $f(qi.b(G.b ? G.b($) : G.call(null, $)))));
    };
  }(a, b, c), e = d();;) {
    if (q(e)) {
      S.c(b, Vc, e), up(wp.e(E([a, e], 0)), sp(e, c)), e = d();
    } else {
      break;
    }
  }
  up(wp.e(E([a, Xj.b(G.b ? G.b($) : G.call(null, $))], 0)), bl.a("\\n", he.a(vp, Ng.b(G.b ? G.b($) : G.call(null, $)))));
  return "undefined" !== typeof console ? console.log("Wrote site") : null;
});
function xp(a) {
  S.c($, cg, a);
  if (q(gl)) {
    var b = "undefined" !== typeof pageConfig ? Kg.e(pageConfig, E([Jg, !0], 0)) : null;
    a = bh.b(b);
    S.c($, cg, b);
    q(a) && (b = (b = Ua(uj.b(G.b ? G.b($) : G.call(null, $)))) ? jp() : b, q(b) && S.j($, N, uj, op(a)), Q.a ? Q.a(dp, a) : Q.call(null, dp, a));
    np();
    document.title = ip();
    a = pp();
    b = document.body;
    return Bm.a ? Bm.a(a, b) : Bm.call(null, a, b);
  }
  return null;
}
;var yp = function(a) {
  return function(b) {
    return function() {
      function c(a) {
        var b = null;
        0 < arguments.length && (b = E(Array.prototype.slice.call(arguments, 0), 0));
        return d.call(this, b);
      }
      function d(c) {
        var d = M.c(G.b ? G.b(b) : G.call(null, b), c, ld);
        d === ld && (d = O.a(a, c), S.j(b, N, c, d));
        return d;
      }
      c.m = 0;
      c.h = function(a) {
        a = y(a);
        return d(a);
      };
      c.e = d;
      return c;
    }();
  }(ge.b ? ge.b(df) : ge.call(null, df));
}(function(a) {
  var b = /^def|^ns\b/, c = J(Tm), d = function(a, b) {
    return function(a) {
      return L.a(Tm, (a % b + b) % b);
    };
  }(b, c), c = Um(a), e = null, f = 0;
  for (a = Uc;;) {
    var h = A(c), m = L.c(h, 0, null), h = L.c(h, 1, null), n = function() {
      switch(m instanceof P ? m.da : null) {
        case "close":
          return f - 1;
        case "open":
          return f + 1;
        default:
          return f;
      }
    }(), p = function() {
      switch(m instanceof P ? m.da : null) {
        case "close":
          return d(n);
        case "open":
          return d(f);
        case "iden":
          var a;
          a = e;
          a = q(a) ? lg(b, e) : a;
          return q(a) ? Yj.b(Sm) : null;
        default:
          return a = m, Sm.b ? Sm.b(a) : Sm.call(null, a);
      }
    }(), c = C(c);
    if (cd(c)) {
      return O.c(Qe, sh, a);
    }
    var s = Ac.a(m, Nh) ? e : h, t = n;
    a = Vc.a(a, new T(null, 3, 5, U, [Zk, p, h], null));
    e = s;
    f = t;
  }
});
function zp(a) {
  var b = /[^ \t]+/;
  return se.a(df, function() {
    return function(a) {
      return function e(b) {
        return new Gd(null, function(a) {
          return function() {
            for (;;) {
              var c = y(b);
              if (c) {
                if (hd(c)) {
                  var n = hc(c), p = J(n), s = Kd(p);
                  a: {
                    for (var t = 0;;) {
                      if (t < p) {
                        var w = x.a(n, t), w = new T(null, 2, 5, U, [Fd.b(Tc(ng(a, w))), w], null);
                        s.add(w);
                        t += 1;
                      } else {
                        n = !0;
                        break a;
                      }
                    }
                    n = void 0;
                  }
                  return n ? Nd(s.N(), e(ic(c))) : Nd(s.N(), null);
                }
                s = A(c);
                return I(new T(null, 2, 5, U, [Fd.b(Tc(ng(a, s))), s], null), e(C(c)));
              }
              return null;
            }
          };
        }(a), null, null);
      };
    }(b)(a);
  }());
}
function Ap(a) {
  return N.e(zp(el.a(a, /\n(?=[(])/)), Ph, "(ns example\n  (:require [reagent.core :as reagent :refer [atom]]))\n", E([Nj, "(ns example\n  (:require [reagent.core :as r :refer [atom]]))\n"], 0));
}
function Bp(a, b) {
  var c = U, d;
  d = bl.a("\n", he.a(a, b));
  d = yp.b ? yp.b(d) : yp.call(null, d);
  return new T(null, 2, 5, c, [sh, d], null);
}
function Cp() {
  return function(a) {
    return function(b) {
      var c = md(b) ? O.a(ee, b) : b, d = M.a(c, Oj), e = M.a(c, dk), f = M.a(c, si), h = M.a(c, Pk);
      return new T(null, 3, 5, U, [Ri, q(h) ? new T(null, 4, 5, U, [uh, new T(null, 3, 5, U, [Eh, new l(null, 1, [Ii, function(a, b, c, d, e, f, h) {
        return function(a) {
          a.preventDefault();
          S.a(h, Ua);
          return!1;
        };
      }(b, c, d, e, f, h, a)], null), q(G.b ? G.b(a) : G.call(null, a)) ? "hide" : "show"], null), q(d) ? null : new T(null, 2, 5, U, [qh, "Example "], null), q(G.b ? G.b(a) : G.call(null, a)) ? Ua(e) ? new T(null, 2, 5, U, [Lg, new T(null, 1, 5, U, [h], null)], null) : new T(null, 1, 5, U, [h], null) : null], null) : null, q(G.b ? G.b(a) : G.call(null, a)) ? q(f) ? new T(null, 3, 5, U, [Bk, q(d) ? null : new T(null, 2, 5, U, [qh, "Source"], null), f], null) : new T(null, 1, 5, U, [Mk], null) : null], 
      null);
    };
  }(Em.b(!0));
}
;function Dp() {
  return new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Z, "I am a component!"], null), new T(null, 5, 5, U, [Ki, "I have ", new T(null, 2, 5, U, [Rh, "bold"], null), new T(null, 3, 5, U, [Zk, new l(null, 1, [Qi, new l(null, 1, [ph, "red"], null)], null), " and red "], null), "text."], null)], null);
}
function Ep() {
  return new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Z, "I include simple-component."], null), new T(null, 1, 5, U, [Dp], null)], null);
}
function Fp(a) {
  return new T(null, 4, 5, U, [Z, "Hello, ", a, "!"], null);
}
function Gp() {
  return new T(null, 2, 5, U, [Fp, "world"], null);
}
function Hp(a) {
  return new T(null, 2, 5, U, [oh, function() {
    return function c(a) {
      return new Gd(null, function() {
        for (;;) {
          var e = y(a);
          if (e) {
            if (hd(e)) {
              var f = hc(e), h = J(f), m = Kd(h);
              a: {
                for (var n = 0;;) {
                  if (n < h) {
                    var p = x.a(f, n), p = ad(new T(null, 3, 5, U, [Vh, "Item ", p], null), new l(null, 1, [th, p], null));
                    m.add(p);
                    n += 1;
                  } else {
                    f = !0;
                    break a;
                  }
                }
                f = void 0;
              }
              return f ? Nd(m.N(), c(ic(e))) : Nd(m.N(), null);
            }
            m = A(e);
            return I(ad(new T(null, 3, 5, U, [Vh, "Item ", m], null), new l(null, 1, [th, m], null)), c(C(e)));
          }
          return null;
        }
      }, null, null);
    }(a);
  }()], null);
}
function Ip() {
  return new T(null, 3, 5, U, [Ri, "Here is a list:", new T(null, 2, 5, U, [Hp, hg.b(3)], null)], null);
}
var Jp = Em.b(0);
function Kp() {
  return new T(null, 7, 5, U, [Ri, "The atom ", new T(null, 2, 5, U, [Y, "click-count"], null), " has value: ", G.b ? G.b(Jp) : G.call(null, Jp), ". ", new T(null, 2, 5, U, [jk, new l(null, 3, [ri, "button", Yh, "Click me!", Ii, function() {
    return S.a(Jp, Jc);
  }], null)], null)], null);
}
function Lp(a) {
  return new T(null, 2, 5, U, [jk, new l(null, 3, [ri, "text", Yh, G.b ? G.b(a) : G.call(null, a), rk, function(b) {
    b = b.target.value;
    return Q.a ? Q.a(a, b) : Q.call(null, a, b);
  }], null)], null);
}
function Mp() {
  return function(a) {
    return function() {
      return new T(null, 3, 5, U, [Ri, new T(null, 3, 5, U, [Z, "The value is now: ", G.b ? G.b(a) : G.call(null, a)], null), new T(null, 3, 5, U, [Z, "Change it here: ", new T(null, 2, 5, U, [Lp, a], null)], null)], null);
    };
  }(Em.b("foo"));
}
function Np() {
  return function(a) {
    return function() {
      setTimeout(function(a) {
        return function() {
          return S.a(a, Jc);
        };
      }(a), 1E3);
      return new T(null, 3, 5, U, [Ri, "Seconds Elapsed: ", G.b ? G.b(a) : G.call(null, a)], null);
    };
  }(Em.b(0));
}
var Op = Em.b(new l(null, 2, [Uk, 180, gj, 80], null));
function Pp(a, b, c, d) {
  var e;
  a: {
    switch(a instanceof P ? a.da : null) {
      case "bmi":
        e = gj;
        break a;
      default:
        e = zj;
    }
  }
  return new T(null, 2, 5, U, [jk, new l(null, 6, [ri, "range", Yh, b, Yg, c, lj, d, Qi, new l(null, 1, [ki, "100%"], null), rk, function(b) {
    return function(c) {
      return S.e(Op, N, a, c.target.value, E([b, null], 0));
    };
  }(e)], null)], null);
}
function Qp() {
  var a;
  a = G.b ? G.b(Op) : G.call(null, Op);
  a = md(a) ? O.a(ee, a) : a;
  var b = M.a(a, zj), c = M.a(a, gj), d = M.a(a, Uk) / 100;
  a = null == b ? N.c(a, zj, c / (d * d)) : N.c(a, gj, b * d * d);
  c = md(a) ? O.a(ee, a) : a;
  a = M.a(c, zj);
  var b = M.a(c, Uk), c = M.a(c, gj), e = 18.5 > a ? new T(null, 2, 5, U, ["orange", "underweight"], null) : 25 > a ? new T(null, 2, 5, U, ["inherit", "normal"], null) : 30 > a ? new T(null, 2, 5, U, ["orange", "overweight"], null) : new T(null, 2, 5, U, ["red", "obese"], null), d = L.c(e, 0, null), e = L.c(e, 1, null);
  return new T(null, 5, 5, U, [Ri, new T(null, 2, 5, U, [wk, "BMI calculator"], null), new T(null, 5, 5, U, [Ri, "Height: ", b | 0, "cm", new T(null, 5, 5, U, [Pp, Uk, b, 100, 220], null)], null), new T(null, 5, 5, U, [Ri, "Weight: ", c | 0, "kg", new T(null, 5, 5, U, [Pp, gj, c, 30, 150], null)], null), new T(null, 6, 5, U, [Ri, "BMI: ", a | 0, " ", new T(null, 3, 5, U, [Zk, new l(null, 1, [Qi, new l(null, 1, [ph, d], null)], null), e], null), new T(null, 5, 5, U, [Pp, zj, a, 10, 50], null)], null)], 
  null);
}
if ("undefined" === typeof Rp) {
  var Rp = Ap('\n(ns reagentdemo.intro\n  (:require [reagent.core :as reagent :refer [atom]]\n            [reagent.interop :refer-macros [.\' .!]]\n            [reagent.debug :refer-macros [dbg println]]\n            [clojure.string :as string]\n            [reagentdemo.syntax :refer-macros [get-source]]\n            [sitetools :refer [link]]\n            [reagentdemo.common :as common :refer [demo-component]]\n            [simpleexample :as simple]\n            [todomvc :as todo]))\n\n(defn simple-component []\n  [:div\n   [:p "I am a component!"]\n   [:p.someclass\n    "I have " [:strong "bold"]\n    [:span {:style {:color "red"}} " and red "] "text."]])\n\n(defn simple-parent []\n  [:div\n   [:p "I include simple-component."]\n   [simple-component]])\n\n(defn hello-component [name]\n  [:p "Hello, " name "!"])\n\n(defn say-hello []\n  [hello-component "world"])\n\n(defn lister [items]\n  [:ul\n   (for [item items]\n     ^{:key item} [:li "Item " item])])\n\n(defn lister-user []\n  [:div\n   "Here is a list:"\n   [lister (range 3)]])\n\n(def click-count (atom 0))\n\n(defn counting-component []\n  [:div\n   "The atom " [:code "click-count"] " has value: "\n   @click-count ". "\n   [:input {:type "button" :value "Click me!"\n            :on-click #(swap! click-count inc)}]])\n\n(defn atom-input [value]\n  [:input {:type "text"\n           :value @value\n           :on-change #(reset! value (-\x3e % .-target .-value))}])\n\n(defn shared-state []\n  (let [val (atom "foo")]\n    (fn []\n      [:div\n       [:p "The value is now: " @val]\n       [:p "Change it here: " [atom-input val]]])))\n\n(defn timer-component []\n  (let [seconds-elapsed (atom 0)]\n    (fn []\n      (js/setTimeout #(swap! seconds-elapsed inc) 1000)\n      [:div\n       "Seconds Elapsed: " @seconds-elapsed])))\n\n(defn render-simple []\n  (reagent/render-component [simple-component]\n                            (.-body js/document)))\n\n(def bmi-data (atom {:height 180 :weight 80}))\n\n(defn calc-bmi []\n  (let [{:keys [height weight bmi] :as data} @bmi-data\n        h (/ height 100)]\n    (if (nil? bmi)\n      (assoc data :bmi (/ weight (* h h)))\n      (assoc data :weight (* bmi h h)))))\n\n(defn slider [param value min max]\n  (let [reset (case param :bmi :weight :bmi)]\n    [:input {:type "range" :value value :min min :max max\n             :style {:width "100%"}\n             :on-change #(swap! bmi-data assoc\n                                param (-\x3e % .-target .-value)\n                                reset nil)}]))\n\n(defn bmi-component []\n  (let [{:keys [weight height bmi]} (calc-bmi)\n        [color diagnose] (cond\n                          (\x3c bmi 18.5) ["orange" "underweight"]\n                          (\x3c bmi 25) ["inherit" "normal"]\n                          (\x3c bmi 30) ["orange" "overweight"]\n                          :else ["red" "obese"])]\n    [:div\n     [:h3 "BMI calculator"]\n     [:div\n      "Height: " (int height) "cm"\n      [slider :height height 100 220]]\n     [:div\n      "Weight: " (int weight) "kg"\n      [slider :weight weight 30 150]]\n     [:div\n      "BMI: " (int bmi) " "\n      [:span {:style {:color color}} diagnose]\n      [slider :bmi bmi 10 50]]]))\n\n(defonce funmap (-\x3e "reagentdemo/intro.cljs" get-source common/fun-map))\n(defonce src-for (partial common/src-for funmap))\n\n(defn intro []\n  (let [github {:href "https://github.com/reagent-project/reagent"}\n        clojurescript {:href "https://github.com/clojure/clojurescript"}\n        react {:href "http://facebook.github.io/react/"}\n        hiccup {:href "https://github.com/weavejester/hiccup"}\n        dynamic-children {:href "http://facebook.github.io/react/docs/multiple-components.html#dynamic-children"}]\n    [:div.demo-text\n\n     [:h2 "Introduction to Reagent"]\n\n     [:p [:a github "Reagent"] " provides a minimalistic interface\n     between " [:a clojurescript "ClojureScript"] " and " [:a\n     react "React"] ". It allows you to define efficient React\n     components using nothing but plain ClojureScript functions and\n     data, that describe your UI using a " [:a hiccup "Hiccup"] "-like\n     syntax."]\n\n     [:p "The goal of Reagent is to make it possible to define\n     arbitrarily complex UIs using just a couple of basic concepts,\n     and to be fast enough by default that you rarely have to care\n     about performance."]\n\n     [:p "A very basic Reagent component may look something like this: "]\n     [demo-component {:comp simple-component\n                      :src (src-for [:simple-component])}]\n\n     [:p "You can build new components using other components as\n     building blocks. Like this:"]\n     [demo-component {:comp simple-parent\n                      :src (src-for [:simple-parent])}]\n\n     [:p "Data is passed to child components using plain old Clojure\n     data types. Like this:"]\n\n     [demo-component {:comp say-hello\n                      :src (src-for [:hello-component :say-hello])}]\n\n     [:p [:strong "Note: "]\n      "In the example above, " [:code "hello-component"] " might just\n      as well have been called as a normal Clojure function instead of\n      as a Reagent component, i.e with parenthesis instead of square\n      brackets. The only difference would have been performance, since\n      \u201dreal\u201d Reagent components are only re-rendered when their data\n      have changed. More advanced components though (see below) must\n      be called with square brackets."]\n\n     [:p "Here is another example that shows items in a "\n     [:code "seq"] ":" ]\n\n     [demo-component {:comp lister-user\n                      :src (src-for [:lister :lister-user])}]\n\n     [:p [:strong "Note: "]\n     "The " [:code "^{:key item}"] " part above isn\u2019t really necessary\n     in this simple example, but attaching a unique key to every item\n     in a dynamically generated list of components is good practice,\n     and helps React to improve performance for large lists. The key\n     can be given either (as in this example) as meta-data, or as a "\n     [:code ":key"] " item in the first argument to a component (if it\n     is a map). See React\u2019s " [:a dynamic-children "documentation"] "\n     for more info."]]))\n\n(defn managing-state []\n  [:div.demo-text\n   [:h2 "Managing state in Reagent"]\n\n   [:p "The easiest way to manage state in Reagent is to use Reagent\u2019s\n   own version of " [:code "atom"] ". It works exactly like the one in\n   clojure.core, except that it keeps track of every time it is\n   deref\u2019ed. Any component that uses an " [:code "atom"]" is automagically\n   re-rendered when its value changes."]\n\n   [:p "Let\u2019s demonstrate that with a simple example:"]\n   [demo-component {:comp counting-component\n                    :src (src-for [:ns :click-count :counting-component])}]\n\n   [:p "Sometimes you may want to maintain state locally in a\n   component. That is easy to do with an " [:code "atom"] " as well."]\n\n   [:p "Here is an example of that, where we call "\n    [:code "setTimeout"] " every time the component is rendered to\n   update a counter:"]\n\n   [demo-component {:comp timer-component\n                    :src (src-for [:timer-component])}]\n\n   [:p "The previous example also uses another feature of Reagent: a\n   component function can return another function, that is used to do\n   the actual rendering. This function is called with the same\n   arguments as the first one."]\n\n   [:p "This allows you to perform some setup of newly created\n   components without resorting to React\u2019s lifecycle events."]\n\n   [:p "By simply passing an "[:code "atom"]" around you can share\n   state management between components, like this:"]\n\n   [demo-component {:comp shared-state\n                    :src (src-for [:ns :atom-input :shared-state])}]\n\n   [:p [:strong "Note: "] "Component functions can be called with any\n   arguments \u2013 as long as they are immutable. You "[:em "could"]" use\n   mutable objects as well, but then you have to make sure that the\n   component is updated when your data changes. Reagent assumes by\n   default that two objects are equal if they are the same object."]])\n\n(defn essential-api []\n  [:div.demo-text\n   [:h2 "Essential API"]\n\n   [:p "Reagent supports most of React\u2019s API, but there is really only\n   one entry-point that is necessary for most applications: "\n    [:code "reagent.core/render-component"] "."]\n\n   [:p "It takes two arguments: a component, and a DOM node. For\n   example, splashing the very first example all over the page would\n   look like this:"]\n\n   [demo-component {:src (src-for [:ns :simple-component :render-simple])}]])\n\n(defn performance []\n  [:div.demo-text\n   [:h2 "Performance"]\n\n   [:p "React itself is very fast, and so is Reagent. In fact, Reagent\n   will be even faster than plain React a lot of the time, thanks to\n   optimizations made possible by ClojureScript."]\n\n   [:p "Mounted components are only re-rendered when their parameters\n   have changed. The change could come from a deref\u2019ed "\n   [:code "atom"] ", the arguments passed to the component or\n   component state."]\n\n   [:p "All of these are checked for changes with "\n   [:code "identical?"] " which is basically only a pointer\n   comparison, so the overhead is very low. Maps passed as arguments\n   to components are compared the same way: they are considered equal\n   if all their entries are identical. This also applies to built-in\n   React components like " [:code ":div"] ", " [:code ":p"] ", etc."]\n\n   [:p "All this means that you simply won\u2019t have to care about\n   performance most of the time. Just define your UI however you like\n   \u2013 it will be fast enough."]\n\n   [:p "There are a couple of situations that you might have to care\n   about, though. If you give Reagent a big " [:code "seq"] " of\n   components to render, you might have to supply all of them with a\n   unique " [:code ":key"] " attribute to speed up rendering (see\n   above). Also note that anonymous functions are not, in general,\n   equal to each other even if they represent the same code and\n   closure."]\n\n   [:p "But again, in general you should just trust that React and\n   Reagent will be fast enough. This very page is composed of a single\n   Reagent component with thousands of child components (every single\n   parenthesis etc in the code examples is a separate component), and\n   yet the page can be updated many times every second without taxing\n   the browser the slightest."]\n\n   [:p "Incidentally, this page also uses another React trick: the\n   entire page is pre-rendered using Node, and "\n   [:code "reagent/render-component-to-string"] ". When it is loaded\n   into the browser, React automatically attaches event-handlers to\n   the already present DOM tree."]])\n\n(defn bmi-demo []\n  [:div.demo-text\n   [:h2 "Putting it all together"]\n\n   [:p "Here is a slightly less contrived example: a simple BMI\n   calculator."]\n\n   [:p "Data is kept in a single " [:code "reagent.core/atom"] ": a map\n   with height, weight and BMI as keys."]\n\n   [demo-component {:comp bmi-component\n                    :src (src-for [:ns :bmi-data :calc-bmi :slider\n                                   :bmi-component])}]])\n\n(defn complete-simple-demo []\n  [:div.demo-text\n   [:h2 "Complete demo"]\n\n   [:p "Reagent comes with a couple of complete examples, with\n   Leiningen project files and everything. Here\u2019s one of them in\n   action:"]\n\n   [demo-component {:comp simple/simple-example\n                    :complete true\n                    :src (-\x3e "simpleexample.cljs"\n                             get-source\n                             common/syntaxify)}]])\n\n(defn todomvc-demo []\n  [:div.demo-text\n   [:h2 "Todomvc"]\n\n   [:p "The obligatory todo list looks roughly like this in\n   Reagent (cheating a little bit by skipping routing and\n   persistence):"]\n\n   [demo-component {:comp todo/todo-app\n                    :complete true\n                    :src (-\x3e "todomvc.cljs"\n                             get-source\n                             common/syntaxify)}]])\n\n(defn main []\n  (let [show-all (atom false)\n        head "Reagent: Minimalistic React for ClojureScript"]\n    (js/setTimeout #(reset! show-all true) 500)\n    (fn []\n      [:div.reagent-demo\n       [:h1 head]\n       [intro]\n       [managing-state]\n       [essential-api]\n       [bmi-demo]\n       [performance]\n       ;; Show heavy examples on load, to make html file smaller\n       (when @show-all [complete-simple-demo])\n       (when @show-all [todomvc-demo])])))\n')
}
if ("undefined" === typeof Sp) {
  var Sp = be.a(Bp, Rp)
}
function Tp() {
  var a = new l(null, 1, [Ok, "http://facebook.github.io/react/docs/multiple-components.html#dynamic-children"], null);
  return new T(null, 14, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Introduction to Reagent"], null), new T(null, 9, 5, U, [Z, new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "https://github.com/reagent-project/reagent"], null), "Reagent"], null), " provides a minimalistic interface\n     between ", new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "https://github.com/clojure/clojurescript"], null), "ClojureScript"], null), " and ", new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "http://facebook.github.io/react/"], 
  null), "React"], null), ". It allows you to define efficient React\n     components using nothing but plain ClojureScript functions and\n     data, that describe your UI using a ", new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "https://github.com/weavejester/hiccup"], null), "Hiccup"], null), "-like\n     syntax."], null), new T(null, 2, 5, U, [Z, "The goal of Reagent is to make it possible to define\n     arbitrarily complex UIs using just a couple of basic concepts,\n     and to be fast enough by default that you rarely have to care\n     about performance."], 
  null), new T(null, 2, 5, U, [Z, "A very basic Reagent component may look something like this: "], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Dp, si, function() {
    var a = new T(null, 1, 5, U, [ej], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 2, 5, U, [Z, "You can build new components using other components as\n     building blocks. Like this:"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Ep, si, function() {
    var a = new T(null, 1, 5, U, [Tj], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 2, 5, U, [Z, "Data is passed to child components using plain old Clojure\n     data types. Like this:"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Gp, si, function() {
    var a = new T(null, 2, 5, U, [Fk, Pj], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 5, 5, U, [Z, new T(null, 2, 5, U, [Rh, "Note: "], null), "In the example above, ", new T(null, 2, 5, U, [Y, "hello-component"], null), " might just\n      as well have been called as a normal Clojure function instead of\n      as a Reagent component, i.e with parenthesis instead of square\n      brackets. The only difference would have been performance, since\n      \u201dreal\u201d Reagent components are only re-rendered when their data\n      have changed. More advanced components though (see below) must\n      be called with square brackets."], 
  null), new T(null, 4, 5, U, [Z, "Here is another example that shows items in a ", new T(null, 2, 5, U, [Y, "seq"], null), ":"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Ip, si, function() {
    var a = new T(null, 2, 5, U, [Gi, ai], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 9, 5, U, [Z, new T(null, 2, 5, U, [Rh, "Note: "], null), "The ", new T(null, 2, 5, U, [Y, "^{:key item}"], null), " part above isn\u2019t really necessary\n     in this simple example, but attaching a unique key to every item\n     in a dynamically generated list of components is good practice,\n     and helps React to improve performance for large lists. The key\n     can be given either (as in this example) as meta-data, or as a ", new T(null, 2, 5, U, [Y, ":key"], 
  null), " item in the first argument to a component (if it\n     is a map). See React\u2019s ", new T(null, 3, 5, U, [Sk, a, "documentation"], null), "\n     for more info."], null)], null);
}
function Up() {
  return new T(null, 13, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Managing state in Reagent"], null), new T(null, 6, 5, U, [Z, "The easiest way to manage state in Reagent is to use Reagent\u2019s\n   own version of ", new T(null, 2, 5, U, [Y, "atom"], null), ". It works exactly like the one in\n   clojure.core, except that it keeps track of every time it is\n   deref\u2019ed. Any component that uses an ", new T(null, 2, 5, U, [Y, "atom"], null), " is automagically\n   re-rendered when its value changes."], 
  null), new T(null, 2, 5, U, [Z, "Let\u2019s demonstrate that with a simple example:"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Kp, si, function() {
    var a = new T(null, 3, 5, U, [Ph, Ug, Xi], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 4, 5, U, [Z, "Sometimes you may want to maintain state locally in a\n   component. That is easy to do with an ", new T(null, 2, 5, U, [Y, "atom"], null), " as well."], null), new T(null, 4, 5, U, [Z, "Here is an example of that, where we call ", new T(null, 2, 5, U, [Y, "setTimeout"], null), " every time the component is rendered to\n   update a counter:"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Np, si, function() {
    var a = new T(null, 1, 5, U, [Lj], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 2, 5, U, [Z, "The previous example also uses another feature of Reagent: a\n   component function can return another function, that is used to do\n   the actual rendering. This function is called with the same\n   arguments as the first one."], null), new T(null, 2, 5, U, [Z, "This allows you to perform some setup of newly created\n   components without resorting to React\u2019s lifecycle events."], null), new T(null, 4, 5, U, [Z, "By simply passing an ", new T(null, 
  2, 5, U, [Y, "atom"], null), " around you can share\n   state management between components, like this:"], null), new T(null, 2, 5, U, [Cp, new l(null, 2, [Pk, Mp, si, function() {
    var a = new T(null, 3, 5, U, [Ph, $k, Dk], null);
    return Sp.b ? Sp.b(a) : Sp.call(null, a);
  }()], null)], null), new T(null, 5, 5, U, [Z, new T(null, 2, 5, U, [Rh, "Note: "], null), "Component functions can be called with any\n   arguments \u2013 as long as they are immutable. You ", new T(null, 2, 5, U, [ni, "could"], null), " use\n   mutable objects as well, but then you have to make sure that the\n   component is updated when your data changes. Reagent assumes by\n   default that two objects are equal if they are the same object."], null)], null);
}
function Vp() {
  var a = U, b = new T(null, 2, 5, U, [Wj, "Essential API"], null), c = new T(null, 4, 5, U, [Z, "Reagent supports most of React\u2019s API, but there is really only\n   one entry-point that is necessary for most applications: ", new T(null, 2, 5, U, [Y, "reagent.core/render-component"], null), "."], null), d = new T(null, 2, 5, U, [Z, "It takes two arguments: a component, and a DOM node. For\n   example, splashing the very first example all over the page would\n   look like this:"], null), e = U, 
  f;
  f = new T(null, 3, 5, U, [Ph, ej, Ek], null);
  f = Sp.b ? Sp.b(f) : Sp.call(null, f);
  return new T(null, 5, 5, a, [Hj, b, c, d, new T(null, 2, 5, e, [Cp, new l(null, 1, [si, f], null)], null)], null);
}
function Wp() {
  return new T(null, 9, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Performance"], null), new T(null, 2, 5, U, [Z, "React itself is very fast, and so is Reagent. In fact, Reagent\n   will be even faster than plain React a lot of the time, thanks to\n   optimizations made possible by ClojureScript."], null), new T(null, 4, 5, U, [Z, "Mounted components are only re-rendered when their parameters\n   have changed. The change could come from a deref\u2019ed ", new T(null, 2, 5, U, [Y, "atom"], null), ", the arguments passed to the component or\n   component state."], 
  null), new T(null, 8, 5, U, [Z, "All of these are checked for changes with ", new T(null, 2, 5, U, [Y, "identical?"], null), " which is basically only a pointer\n   comparison, so the overhead is very low. Maps passed as arguments\n   to components are compared the same way: they are considered equal\n   if all their entries are identical. This also applies to built-in\n   React components like ", new T(null, 2, 5, U, [Y, ":div"], null), ", ", new T(null, 2, 5, U, [Y, ":p"], null), ", etc."], null), 
  new T(null, 2, 5, U, [Z, "All this means that you simply won\u2019t have to care about\n   performance most of the time. Just define your UI however you like\n   \u2013 it will be fast enough."], null), new T(null, 6, 5, U, [Z, "There are a couple of situations that you might have to care\n   about, though. If you give Reagent a big ", new T(null, 2, 5, U, [Y, "seq"], null), " of\n   components to render, you might have to supply all of them with a\n   unique ", new T(null, 2, 5, U, [Y, ":key"], 
  null), " attribute to speed up rendering (see\n   above). Also note that anonymous functions are not, in general,\n   equal to each other even if they represent the same code and\n   closure."], null), new T(null, 2, 5, U, [Z, "But again, in general you should just trust that React and\n   Reagent will be fast enough. This very page is composed of a single\n   Reagent component with thousands of child components (every single\n   parenthesis etc in the code examples is a separate component), and\n   yet the page can be updated many times every second without taxing\n   the browser the slightest."], 
  null), new T(null, 4, 5, U, [Z, "Incidentally, this page also uses another React trick: the\n   entire page is pre-rendered using Node, and ", new T(null, 2, 5, U, [Y, "reagent/render-component-to-string"], null), ". When it is loaded\n   into the browser, React automatically attaches event-handlers to\n   the already present DOM tree."], null)], null);
}
function Xp() {
  var a = U, b = new T(null, 2, 5, U, [Wj, "Putting it all together"], null), c = new T(null, 2, 5, U, [Z, "Here is a slightly less contrived example: a simple BMI\n   calculator."], null), d = new T(null, 4, 5, U, [Z, "Data is kept in a single ", new T(null, 2, 5, U, [Y, "reagent.core/atom"], null), ": a map\n   with height, weight and BMI as keys."], null), e = U, f;
  f = new T(null, 5, 5, U, [Ph, oi, gh, Vk, Lk], null);
  f = Sp.b ? Sp.b(f) : Sp.call(null, f);
  return new T(null, 5, 5, a, [Hj, b, c, d, new T(null, 2, 5, e, [Cp, new l(null, 2, [Pk, Qp, si, f], null)], null)], null);
}
function Yp() {
  return new T(null, 4, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Complete demo"], null), new T(null, 2, 5, U, [Z, "Reagent comes with a couple of complete examples, with\n   Leiningen project files and everything. Here\u2019s one of them in\n   action:"], null), new T(null, 2, 5, U, [Cp, new l(null, 3, [Pk, an, dk, !0, si, yp.b ? yp.b('\n(ns simpleexample\n  (:require [reagent.core :as reagent :refer [atom]]))\n\n(def timer (atom (js/Date.)))\n(def time-color (atom "#f34"))\n\n(defn update-time [time]\n  ;; Update the time every 1/10 second to be accurate...\n  (js/setTimeout #(reset! time (js/Date.)) 100))\n\n(defn greeting [message]\n  [:h1 message])\n\n(defn clock []\n  (update-time timer)\n  (let [time-str (-\x3e @timer .toTimeString (clojure.string/split " ") first)]\n    [:div.example-clock\n     {:style {:color @time-color}}\n     time-str]))\n\n(defn color-input []\n  [:div.color-input\n   "Time color: "\n   [:input {:type "text"\n            :value @time-color\n            :on-change #(reset! time-color (-\x3e % .-target .-value))}]])\n\n(defn simple-example []\n  [:div\n   [greeting "Hello world, it is now"]\n   [clock]\n   [color-input]])\n\n(defn ^:export run []\n  (reagent/render-component (fn [] [simple-example])\n                            (.-body js/document)))\n') : 
  yp.call(null, '\n(ns simpleexample\n  (:require [reagent.core :as reagent :refer [atom]]))\n\n(def timer (atom (js/Date.)))\n(def time-color (atom "#f34"))\n\n(defn update-time [time]\n  ;; Update the time every 1/10 second to be accurate...\n  (js/setTimeout #(reset! time (js/Date.)) 100))\n\n(defn greeting [message]\n  [:h1 message])\n\n(defn clock []\n  (update-time timer)\n  (let [time-str (-\x3e @timer .toTimeString (clojure.string/split " ") first)]\n    [:div.example-clock\n     {:style {:color @time-color}}\n     time-str]))\n\n(defn color-input []\n  [:div.color-input\n   "Time color: "\n   [:input {:type "text"\n            :value @time-color\n            :on-change #(reset! time-color (-\x3e % .-target .-value))}]])\n\n(defn simple-example []\n  [:div\n   [greeting "Hello world, it is now"]\n   [clock]\n   [color-input]])\n\n(defn ^:export run []\n  (reagent/render-component (fn [] [simple-example])\n                            (.-body js/document)))\n')], 
  null)], null)], null);
}
function Zp() {
  return new T(null, 4, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Todomvc"], null), new T(null, 2, 5, U, [Z, "The obligatory todo list looks roughly like this in\n   Reagent (cheating a little bit by skipping routing and\n   persistence):"], null), new T(null, 2, 5, U, [Cp, new l(null, 3, [Pk, Qm, dk, !0, si, yp.b ? yp.b('\n(ns todomvc\n  (:require [reagent.core :as reagent :refer [atom]]))\n\n(def todos (atom (sorted-map)))\n\n(def counter (atom 0))\n\n(defn add-todo [text]\n  (let [id (swap! counter inc)]\n    (swap! todos assoc id {:id id :title text :done false})))\n\n(defn toggle [id] (swap! todos update-in [id :done] not))\n(defn save [id title] (swap! todos assoc-in [id :title] title))\n(defn delete [id] (swap! todos dissoc id))\n\n(defn mmap [m f a] (-\x3e\x3e m (f a) (into (empty m))))\n(defn complete-all [v] (swap! todos mmap map #(assoc-in % [1 :done] v)))\n(defn clear-done [] (swap! todos mmap remove #(get-in % [1 :done])))\n\n(add-todo "Rename Cloact to Reagent")\n(add-todo "Add undo demo")\n(add-todo "Make all rendering async")\n(add-todo "Allow any arguments to component functions")\n(complete-all true)\n\n(defn todo-input [{:keys [title on-save on-stop]}]\n  (let [val (atom title)\n        stop #(do (reset! val "")\n                  (if on-stop (on-stop)))\n        save #(let [v (-\x3e @val str clojure.string/trim)]\n                (if-not (empty? v) (on-save v))\n                (stop))]\n    (fn [props]\n      [:input (merge props\n                     {:type "text" :value @val :on-blur save\n                      :on-change #(reset! val (-\x3e % .-target .-value))\n                      :on-key-down #(case (.-which %)\n                                      13 (save)\n                                      27 (stop)\n                                      nil)})])))\n\n(def todo-edit (with-meta todo-input\n                 {:component-did-mount #(.focus (reagent/dom-node %))}))\n\n(defn todo-stats [{:keys [filt active done]}]\n  (let [props-for (fn [name]\n                    {:class (if (\x3d name @filt) "selected")\n                     :on-click #(reset! filt name)})]\n    [:div\n     [:span#todo-count\n      [:strong active] " " (case active 1 "item" "items") " left"]\n     [:ul#filters\n      [:li [:a (props-for :all) "All"]]\n      [:li [:a (props-for :active) "Active"]]\n      [:li [:a (props-for :done) "Completed"]]]\n     (when (pos? done)\n       [:button#clear-completed {:on-click clear-done}\n        "Clear completed " done])]))\n\n(defn todo-item []\n  (let [editing (atom false)]\n    (fn [{:keys [id done title]}]\n      [:li {:class (str (if done "completed ")\n                        (if @editing "editing"))}\n       [:div.view\n        [:input.toggle {:type "checkbox" :checked done\n                        :on-change #(toggle id)}]\n        [:label {:on-double-click #(reset! editing true)} title]\n        [:button.destroy {:on-click #(delete id)}]]\n       (when @editing\n         [todo-edit {:class "edit" :title title\n                     :on-save #(save id %)\n                     :on-stop #(reset! editing false)}])])))\n\n(defn todo-app [props]\n  (let [filt (atom :all)]\n    (fn []\n      (let [items (vals @todos)\n            done (-\x3e\x3e items (filter :done) count)\n            active (- (count items) done)]\n        [:div\n         [:section#todoapp\n          [:header#header\n           [:h1 "todos"]\n           [todo-input {:id "new-todo"\n                        :placeholder "What needs to be done?"\n                        :on-save add-todo}]]\n          (when (-\x3e items count pos?)\n            [:div\n             [:section#main\n              [:input#toggle-all {:type "checkbox" :checked (zero? active)\n                                  :on-change #(complete-all (pos? active))}]\n              [:label {:for "toggle-all"} "Mark all as complete"]\n              [:ul#todo-list\n               (for [todo (filter (case @filt\n                                    :active (complement :done)\n                                    :done :done\n                                    :all identity) items)]\n                 ^{:key (:id todo)} [todo-item todo])]]\n             [:footer#footer\n              [todo-stats {:active active :done done :filt filt}]]])]\n         [:footer#info\n          [:p "Double-click to edit a todo"]]]))))\n\n(defn ^:export run []\n  (reagent/render-component [todo-app] (.-body js/document)))\n') : 
  yp.call(null, '\n(ns todomvc\n  (:require [reagent.core :as reagent :refer [atom]]))\n\n(def todos (atom (sorted-map)))\n\n(def counter (atom 0))\n\n(defn add-todo [text]\n  (let [id (swap! counter inc)]\n    (swap! todos assoc id {:id id :title text :done false})))\n\n(defn toggle [id] (swap! todos update-in [id :done] not))\n(defn save [id title] (swap! todos assoc-in [id :title] title))\n(defn delete [id] (swap! todos dissoc id))\n\n(defn mmap [m f a] (-\x3e\x3e m (f a) (into (empty m))))\n(defn complete-all [v] (swap! todos mmap map #(assoc-in % [1 :done] v)))\n(defn clear-done [] (swap! todos mmap remove #(get-in % [1 :done])))\n\n(add-todo "Rename Cloact to Reagent")\n(add-todo "Add undo demo")\n(add-todo "Make all rendering async")\n(add-todo "Allow any arguments to component functions")\n(complete-all true)\n\n(defn todo-input [{:keys [title on-save on-stop]}]\n  (let [val (atom title)\n        stop #(do (reset! val "")\n                  (if on-stop (on-stop)))\n        save #(let [v (-\x3e @val str clojure.string/trim)]\n                (if-not (empty? v) (on-save v))\n                (stop))]\n    (fn [props]\n      [:input (merge props\n                     {:type "text" :value @val :on-blur save\n                      :on-change #(reset! val (-\x3e % .-target .-value))\n                      :on-key-down #(case (.-which %)\n                                      13 (save)\n                                      27 (stop)\n                                      nil)})])))\n\n(def todo-edit (with-meta todo-input\n                 {:component-did-mount #(.focus (reagent/dom-node %))}))\n\n(defn todo-stats [{:keys [filt active done]}]\n  (let [props-for (fn [name]\n                    {:class (if (\x3d name @filt) "selected")\n                     :on-click #(reset! filt name)})]\n    [:div\n     [:span#todo-count\n      [:strong active] " " (case active 1 "item" "items") " left"]\n     [:ul#filters\n      [:li [:a (props-for :all) "All"]]\n      [:li [:a (props-for :active) "Active"]]\n      [:li [:a (props-for :done) "Completed"]]]\n     (when (pos? done)\n       [:button#clear-completed {:on-click clear-done}\n        "Clear completed " done])]))\n\n(defn todo-item []\n  (let [editing (atom false)]\n    (fn [{:keys [id done title]}]\n      [:li {:class (str (if done "completed ")\n                        (if @editing "editing"))}\n       [:div.view\n        [:input.toggle {:type "checkbox" :checked done\n                        :on-change #(toggle id)}]\n        [:label {:on-double-click #(reset! editing true)} title]\n        [:button.destroy {:on-click #(delete id)}]]\n       (when @editing\n         [todo-edit {:class "edit" :title title\n                     :on-save #(save id %)\n                     :on-stop #(reset! editing false)}])])))\n\n(defn todo-app [props]\n  (let [filt (atom :all)]\n    (fn []\n      (let [items (vals @todos)\n            done (-\x3e\x3e items (filter :done) count)\n            active (- (count items) done)]\n        [:div\n         [:section#todoapp\n          [:header#header\n           [:h1 "todos"]\n           [todo-input {:id "new-todo"\n                        :placeholder "What needs to be done?"\n                        :on-save add-todo}]]\n          (when (-\x3e items count pos?)\n            [:div\n             [:section#main\n              [:input#toggle-all {:type "checkbox" :checked (zero? active)\n                                  :on-change #(complete-all (pos? active))}]\n              [:label {:for "toggle-all"} "Mark all as complete"]\n              [:ul#todo-list\n               (for [todo (filter (case @filt\n                                    :active (complement :done)\n                                    :done :done\n                                    :all identity) items)]\n                 ^{:key (:id todo)} [todo-item todo])]]\n             [:footer#footer\n              [todo-stats {:active active :done done :filt filt}]]])]\n         [:footer#info\n          [:p "Double-click to edit a todo"]]]))))\n\n(defn ^:export run []\n  (reagent/render-component [todo-app] (.-body js/document)))\n')], 
  null)], null)], null);
}
function $p() {
  var a = Em.b(!1);
  setTimeout(function(a) {
    return function() {
      return Q.a ? Q.a(a, !0) : Q.call(null, a, !0);
    };
  }(a, "Reagent: Minimalistic React for ClojureScript"), 500);
  return function(a, c) {
    return function() {
      return new T(null, 9, 5, U, [ek, new T(null, 2, 5, U, [ok, c], null), new T(null, 1, 5, U, [Tp], null), new T(null, 1, 5, U, [Up], null), new T(null, 1, 5, U, [Vp], null), new T(null, 1, 5, U, [Xp], null), new T(null, 1, 5, U, [Wp], null), q(G.b ? G.b(a) : G.call(null, a)) ? new T(null, 1, 5, U, [Yp], null) : null, q(G.b ? G.b(a) : G.call(null, a)) ? new T(null, 1, 5, U, [Zp], null) : null], null);
    };
  }(a, "Reagent: Minimalistic React for ClojureScript");
}
;var aq = Ap('(ns reagentdemo.news.undodemo\n  (:require [reagent.core :as reagent :refer [atom]]\n            [reagent.interop :refer-macros [.\' .!]]\n            [reagent.debug :refer-macros [dbg println]]\n            [reagentdemo.syntax :refer-macros [get-source]]\n            [sitetools :as tools :refer [link]]\n            [reagentdemo.common :as common :refer [demo-component]]\n            [todomvc :as todomvc]))\n\n(def url "news/cloact-reagent-undo-demo.html")\n(def title "Cloact becomes Reagent: Undo is trivial")\n\n(def funmap (-\x3e ::this get-source common/fun-map))\n(def src-for (partial common/src-for funmap))\n\n(def state todomvc/todos)\n\n(def undo-list (atom nil))\n\n(defn undo []\n  (let [undos @undo-list]\n    (when-let [old (first undos)]\n      (reset! state old)\n      (reset! undo-list (rest undos)))))\n\n(defn undo-button []\n  (let [n (count @undo-list)]\n    [:input {:type "button" :on-click undo\n             :disabled (zero? n)\n             :value (str "Undo (" n ")")}]))\n\n(defn todomvc-with-undo []\n  (add-watch state ::undo-watcher\n             (fn [_ _ old-state _]\n               (swap! undo-list conj old-state)))\n  [:div\n   [undo-button]\n   [todomvc/todo-app]])\n\n(defn undo-demo []\n  [demo-component {:comp todomvc-with-undo\n                   :src (src-for [:state :undo-list :undo :save-state\n                                  :undo-button :todomvc-with-undo])}])\n\n(def undo-demo-cleanup\n  (with-meta undo-demo {:component-will-unmount\n                        (fn []\n                          (reset! undo-list nil)\n                          (remove-watch state ::undo-watcher))}))\n\n(defn main [{:keys [summary]}]\n  (let [head title]\n    [:div.reagent-demo\n     [:h1 [link {:href url} head]]\n     [:div.demo-text\n      [:h2 "(reset! cloact-name \\"Reagent\\")"]\n\n      [:p "It turns out that \u201dCloact\u201d was a really, really bad\n      name. It made some people think about birds\u2019 behinds, in\n      possibly unhealthy ways, which even Google suggested they\n      should."]\n\n      [:p "The new name is " [:strong "Reagent"] ", which hopefully\n      doesn\u2019t bring with it the same disgusting connotations."]\n\n      [:p "The API is otherwise unchanged, so a simple\n      search-and-replace should suffice."]\n\n      (if summary\n        [link {:href url\n               :class \'news-read-more} "Read more"]\n        [:div.demo-text\n\n         [:h2 "Undo the easy way"]\n\n         [:p "To celebrate the undoing of the apparently disgusting\n         name, here is an example of how easy it is to add undo\n         functionality to Reagent components."]\n\n         [:p "It simply saves the old state whenever it changes, and\n         restores it when the button is clicked."]\n\n         [:p "The really nice thing about ClojureScript is that not\n         only is this easy and safe to do, courtesy of immutable data\n         structures, it is also efficient. ClojureScript figures out\n         how to represent \u201dchanges\u201d to maps and vectors efficiently,\n         so that you won\u2019t have to."]\n\n         [undo-demo-cleanup]])]]))\n\n(tools/register-page url (fn [] [main]) title)\n'), 
bq = be.a(Bp, aq), cq = Em.b(null);
function dq() {
  var a = G.b ? G.b(cq) : G.call(null, cq), b = A(a);
  return q(b) ? (Q.a ? Q.a(Gm, b) : Q.call(null, Gm, b), a = C(a), Q.a ? Q.a(cq, a) : Q.call(null, cq, a)) : null;
}
function eq() {
  var a = J(G.b ? G.b(cq) : G.call(null, cq));
  return new T(null, 2, 5, U, [jk, new l(null, 4, [ri, "button", Ii, dq, zh, 0 === a, Yh, "Undo (" + v.b(a) + ")"], null)], null);
}
function fq() {
  yg(Gm, vh, function(a, b, c) {
    return S.c(cq, Vc, c);
  });
  return new T(null, 3, 5, U, [Ri, new T(null, 1, 5, U, [eq], null), new T(null, 1, 5, U, [Qm], null)], null);
}
var gq = ad(function() {
  var a = U, b;
  b = new T(null, 6, 5, U, [ti, Mj, cj, Jj, Ai, xj], null);
  b = bq.b ? bq.b(b) : bq.call(null, b);
  return new T(null, 2, 5, a, [Cp, new l(null, 2, [Pk, fq, si, b], null)], null);
}, new l(null, 1, [Ij, function() {
  Q.a ? Q.a(cq, null) : Q.call(null, cq, null);
  return ac(Gm, vh);
}], null));
function hq(a) {
  a = md(a) ? O.a(ee, a) : a;
  a = M.a(a, Ti);
  return new T(null, 3, 5, U, [ek, new T(null, 2, 5, U, [ok, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "news/cloact-reagent-undo-demo.html"], null), "Cloact becomes Reagent: Undo is trivial"], null)], null), new T(null, 6, 5, U, [Hj, new T(null, 2, 5, U, [Wj, '(reset! cloact-name "Reagent")'], null), new T(null, 2, 5, U, [Z, "It turns out that \u201dCloact\u201d was a really, really bad\n      name. It made some people think about birds\u2019 behinds, in\n      possibly unhealthy ways, which even Google suggested they\n      should."], 
  null), new T(null, 4, 5, U, [Z, "The new name is ", new T(null, 2, 5, U, [Rh, "Reagent"], null), ", which hopefully\n      doesn\u2019t bring with it the same disgusting connotations."], null), new T(null, 2, 5, U, [Z, "The API is otherwise unchanged, so a simple\n      search-and-replace should suffice."], null), q(a) ? new T(null, 3, 5, U, [gp, new l(null, 2, [Ok, "news/cloact-reagent-undo-demo.html", vj, new Cc(null, "news-read-more", "news-read-more", 150793143, null)], null), "Read more"], 
  null) : new T(null, 6, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "Undo the easy way"], null), new T(null, 2, 5, U, [Z, "To celebrate the undoing of the apparently disgusting\n         name, here is an example of how easy it is to add undo\n         functionality to Reagent components."], null), new T(null, 2, 5, U, [Z, "It simply saves the old state whenever it changes, and\n         restores it when the button is clicked."], null), new T(null, 2, 5, U, [Z, "The really nice thing about ClojureScript is that not\n         only is this easy and safe to do, courtesy of immutable data\n         structures, it is also efficient. ClojureScript figures out\n         how to represent \u201dchanges\u201d to maps and vectors efficiently,\n         so that you won\u2019t have to."], 
  null), new T(null, 1, 5, U, [gq], null)], null)], null)], null);
}
fp.c("news/cloact-reagent-undo-demo.html", function() {
  return new T(null, 1, 5, U, [hq], null);
}, "Cloact becomes Reagent: Undo is trivial");
function iq(a) {
  if (a ? a.Id : a) {
    return a.od;
  }
  var b;
  b = iq[k(null == a ? null : a)];
  if (!b && (b = iq._, !b)) {
    throw u("IPoint.x", a);
  }
  return b.call(null, a);
}
function jq(a) {
  if (a ? a.Jd : a) {
    return a.pd;
  }
  var b;
  b = jq[k(null == a ? null : a)];
  if (!b && (b = jq._, !b)) {
    throw u("IPoint.y", a);
  }
  return b.call(null, a);
}
function kq(a, b) {
  this.od = a;
  this.pd = b;
}
kq.prototype.Id = function() {
  return this.od;
};
kq.prototype.Jd = function() {
  return this.pd;
};
function lq(a, b) {
  var c = function() {
    var c = iq(b) - iq(a);
    return Math.pow(c, 2);
  }() + function() {
    var c = jq(b) - jq(a);
    return Math.pow(c, 2);
  }();
  return Math.sqrt(c);
}
;var mq = new l(null, 4, [fh, "black", fj, 2, Wh, "blue", dh, 5], null), nq = new l(null, 2, [fh, "black", fj, 2], null), oq = new l(null, 3, [Wh, "rgba(255,0,0,0.1)", fh, "black", fj, 2], null);
function pq(a) {
  return function(b) {
    var c = b.clientX;
    b = b.clientY;
    return a.a ? a.a(c, b) : a.call(null, c, b);
  };
}
function qq(a, b) {
  return function() {
    zo(window, "mousemove", a);
    var c = window, d = G.b ? G.b(b) : G.call(null, b);
    return zo(c, "mouseup", d);
  };
}
function rq(a, b) {
  var c = md(a) ? O.a(ee, a) : a, d = M.a(c, wi);
  return new T(null, 2, 5, U, [hi, cg.e(E([mq, new l(null, 3, [Fi, function(a, b, c) {
    return function() {
      var a = pq(c), b = ge.b ? ge.b(null) : ge.call(null, null), d = qq(a, b);
      Q.a ? Q.a(b, d) : Q.call(null, b, d);
      to(window, "mousemove", a);
      return to(window, "mouseup", d);
    };
  }(a, c, d), qj, iq(b), Aj, jq(b)], null)], 0))], null);
}
function sq(a, b) {
  return new T(null, 2, 5, U, [dj, cg.e(E([nq, new l(null, 4, [hk, iq(a), ii, jq(a), Kk, iq(b), qk, jq(b)], null)], 0))], null);
}
function tq(a, b, c) {
  return new T(null, 4, 5, U, [Vi, new T(null, 3, 5, U, [sq, a, b], null), new T(null, 3, 5, U, [sq, b, c], null), new T(null, 3, 5, U, [sq, c, a], null)], null);
}
function uq(a, b) {
  return new T(null, 2, 5, U, [hi, cg.e(E([oq, new l(null, 3, [qj, iq(a), Aj, jq(a), dh, lq(a, b)], null)], 0))], null);
}
;var vq = Em.b(new l(null, 5, [Ci, new kq(100, 100), Xg, new kq(200, 200), eh, new kq(100, 200), Yi, new kq(250, 250), Z, new kq(250, 300)], null));
function wq(a, b) {
  return function(c, d) {
    var e = a.getDOMNode().getBoundingClientRect();
    return S.j(vq, N, b, new kq(c - e.left, d - e.top));
  };
}
function xq(a) {
  var b = G.b ? G.b(vq) : G.call(null, vq), c = md(b) ? O.a(ee, b) : b, b = M.a(c, Yi), d = M.a(c, Z), e = M.a(c, eh), f = M.a(c, Xg), c = M.a(c, Ci);
  return new T(null, 9, 5, U, [Vi, new T(null, 4, 5, U, [tq, c, f, e], null), new T(null, 3, 5, U, [uq, d, b], null), new T(null, 3, 5, U, [sq, d, b], null), new T(null, 3, 5, U, [rq, new l(null, 1, [wi, wq(a, Yi)], null), b], null), new T(null, 3, 5, U, [rq, new l(null, 1, [wi, wq(a, Z)], null), d], null), new T(null, 3, 5, U, [rq, new l(null, 1, [wi, wq(a, Ci)], null), c], null), new T(null, 3, 5, U, [rq, new l(null, 1, [wi, wq(a, Xg)], null), f], null), new T(null, 3, 5, U, [rq, new l(null, 1, 
  [wi, wq(a, eh)], null), e], null)], null);
}
function yq(a) {
  var b = md(a) ? O.a(ee, a) : a;
  a = M.a(b, Uk);
  b = M.a(b, ki);
  return new T(null, 4, 5, U, [Kj, new l(null, 3, [ki, q(b) ? b : 800, Uk, q(a) ? a : 600, Qi, new l(null, 1, [vk, "1px solid black"], null)], null), new T(null, 3, 5, U, [Yk, new l(null, 4, [Qi, new l(null, 2, [di, "none", Sh, "none"], null), fk, 20, Mg, 20, Bh, 20], null), "The points are draggable"], null), new T(null, 2, 5, U, [xq, Pl], null)], null);
}
ma("geometry.core.run", function() {
  var a = new T(null, 1, 5, U, [yq], null), b = document.getElementById("app");
  return Bm.a ? Bm.a(a, b) : Bm.call(null, a, b);
});
var zq = Ap('(ns reagentdemo.news.anyargs\n  (:require [reagent.core :as r :refer [atom]]\n            [reagent.interop :refer-macros [.\' .!]]\n            [reagent.debug :refer-macros [dbg println]]\n            [reagentdemo.syntax :refer-macros [get-source]]\n            [sitetools :as tools :refer [link]]\n            [reagentdemo.common :as common :refer [demo-component]]\n            [geometry.core :as geometry]))\n\n(def url "news/any-arguments.html")\n(def title "All arguments allowed")\n\n(def funmap (-\x3e ::this get-source common/fun-map))\n(def src-for (partial common/src-for funmap))\n\n(defn hello-component [name]\n  [:p "Hello, " name "!"])\n\n(defn say-hello []\n  [hello-component "world"])\n\n(defn geometry-example []\n  [geometry/main {:width "100%" :height 500}])\n\n(defn my-div []\n  (let [this (r/current-component)]\n    (into [:div.custom (r/props this)]\n          (r/children this))))\n\n(defn call-my-div []\n  [:div\n   [my-div "Some text."]\n   [my-div {:style {:font-weight \'bold}}\n    [:p "Some other text in bold."]]])\n\n(defn main [{:keys [summary]}]\n  (let [geometry {:href "https://github.com/reagent-project/reagent/tree/master/examples/geometry"}\n        jonase {:href "https://github.com/jonase"}]\n    [:div.reagent-demo\n     [:h1 [link {:href url} title]]\n     [:div.demo-text\n\n      [:h2 "If it looks like a function\u2026"]\n\n      [:p "Calling a component in Reagent looks a lot like a function\n      call. Now it also " [:em "works"] " like one."]\n\n      [:p "Before 0.4.0, component functions were always called with\n      three arguments: a map of attributes, a vector of \u201dchildren\u201d,\n      and the current React component."]\n\n      [:p "This was confusing, and an unnecessary limitation, so now\n      component functions get exactly the same arguments you pass to\n      them."]\n\n      (if summary\n        [link {:href url :class \'news-read-more} "Read more"]\n        [:div.demo-text\n         [:p "In other words, you can now do this:"]\n\n         [demo-component {:comp say-hello\n                          :src (src-for [:hello-component :say-hello])}]\n\n         [:p "In the above example, it wouldn\u2019t make any difference at\n          all if " [:code "hello-component"] " had been called as a\n          function, i.e with parentheses instead of brackets (except\n          for performance, since components are cached between renders\n          if the arguments to them don\u2019t change)."]\n\n         [:p "But there is one drawback: component function no longer\n          receives the \u201dcurrent component\u201d as a parameter. Instead\n          you\u2019ll have to use "\n          [:code "reagent.core/current-component"]\n          " in order to get that. Beware that "\n          [:code "current-component"] " is only valid in component\n          functions, and must be called outside of e.g event handlers\n          and " [:code "for"] " expressions, so it\u2019s safest to always\n          put the call at the top, as in " [:code "my-div"] " here:"]\n\n         [demo-component {:comp call-my-div\n                          :src (src-for [:nsr :my-div :call-my-div])}]\n\n         [:p [:em "Note: "] [:code "r/props"] " and "\n         [:code "r/children"] " correspond to React\u2019s "\n         [:code "this.props"] " and " [:code "this.props.children"] ",\n         respectively. They may be convenient to use when wrapping\n         native React components, since they follow the same\n         conventions when interpreting the arguments given."]\n\n         [:h2 "Other news in 0.4.0"]\n\n         [:ul\n\n          [:li "React has been updated to version 0.9.0."]\n\n          [:li "You can now use any object that satisfies "\n           [:code "ifn?"] " as a component function, and not just\n           plain functions. That includes functions defined with "\n           [:code "deftype"] ", " [:code "defrecord"] ", etc, as well\n           as collections like maps."]\n\n          [:li\n           [:code "reagent.core/set-state"] " and "\n           [:code "reagent.core/replace-state"] " are now implemented\n           using an " [:code "reagent.core/atom"] ", and are\n           consequently async."]\n\n          [:li "Keys associated with items in a seq (e.g \u201ddynamic\n           children\u201d in React parlance) can now be specified with\n           meta-data, as well as with a " [:code ":key"] " item in the\n           first parameter as before. In other words, these two forms\n           are now equivalent: " [:code "^{:key foo} [:li bar]"] "\n           and " [:code "[:li {:key foo} bar]"] "."]\n\n          [:li "Performance has been improved even further. For\n           example, there is now practically no overhead for\n           tracking derefs in components that don\u2019t use "\n           [:code "atom"] "s. Allocations and memory use have also\n           been reduced."]\n\n          [:li "Intro and examples have been tweaked a little to take\n          advantage of the new calling conventions."]]\n\n         [:h2 "New svg example"]\n\n         [:p "There is also a new, elegant and simple "\n          [:a geometry "example"] " of using svg with Reagent, written\n          by " [:a jonase "Jonas Enlund"] ". It also shows how you can\n          use Reagent\u2019s new calling convensions, and looks like\n          this:"]\n\n         [demo-component {:comp geometry-example}]])]]))\n\n(tools/register-page url (fn [] [main])\n                     (str "Reagent 0.4.0: " title))\n'), 
Aq = be.a(Bp, zq);
function Bq(a) {
  return new T(null, 4, 5, U, [Z, "Hello, ", a, "!"], null);
}
function Cq() {
  return new T(null, 2, 5, U, [Bq, "world"], null);
}
function Dq() {
  return new T(null, 2, 5, U, [yq, new l(null, 2, [ki, "100%", Uk, 500], null)], null);
}
function Eq() {
  var a = Pl;
  return se.a(new T(null, 2, 5, U, [Qg, hl(a.props.argv)], null), il(a));
}
function Fq() {
  return new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Eq, "Some text."], null), new T(null, 3, 5, U, [Eq, new l(null, 1, [Qi, new l(null, 1, [Hh, new Cc(null, "bold", "bold", 1523721992, null)], null)], null), new T(null, 2, 5, U, [Z, "Some other text in bold."], null)], null)], null);
}
function Gq(a) {
  a = md(a) ? O.a(ee, a) : a;
  a = M.a(a, Ti);
  var b = new l(null, 1, [Ok, "https://github.com/reagent-project/reagent/tree/master/examples/geometry"], null), c = new l(null, 1, [Ok, "https://github.com/jonase"], null);
  return new T(null, 3, 5, U, [ek, new T(null, 2, 5, U, [ok, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "news/any-arguments.html"], null), "All arguments allowed"], null)], null), new T(null, 6, 5, U, [Hj, new T(null, 2, 5, U, [Wj, "If it looks like a function\u2026"], null), new T(null, 4, 5, U, [Z, "Calling a component in Reagent looks a lot like a function\n      call. Now it also ", new T(null, 2, 5, U, [ni, "works"], null), " like one."], null), new T(null, 2, 5, U, [Z, "Before 0.4.0, component functions were always called with\n      three arguments: a map of attributes, a vector of \u201dchildren\u201d,\n      and the current React component."], 
  null), new T(null, 2, 5, U, [Z, "This was confusing, and an unnecessary limitation, so now\n      component functions get exactly the same arguments you pass to\n      them."], null), q(a) ? new T(null, 3, 5, U, [gp, new l(null, 2, [Ok, "news/any-arguments.html", vj, new Cc(null, "news-read-more", "news-read-more", 150793143, null)], null), "Read more"], null) : new T(null, 12, 5, U, [Hj, new T(null, 2, 5, U, [Z, "In other words, you can now do this:"], null), new T(null, 2, 5, U, [Cp, new l(null, 
  2, [Pk, Cq, si, function() {
    var a = new T(null, 2, 5, U, [Fk, Pj], null);
    return Aq.b ? Aq.b(a) : Aq.call(null, a);
  }()], null)], null), new T(null, 4, 5, U, [Z, "In the above example, it wouldn\u2019t make any difference at\n          all if ", new T(null, 2, 5, U, [Y, "hello-component"], null), " had been called as a\n          function, i.e with parentheses instead of brackets (except\n          for performance, since components are cached between renders\n          if the arguments to them don\u2019t change)."], null), new T(null, 10, 5, U, [Z, "But there is one drawback: component function no longer\n          receives the \u201dcurrent component\u201d as a parameter. Instead\n          you\u2019ll have to use ", 
  new T(null, 2, 5, U, [Y, "reagent.core/current-component"], null), " in order to get that. Beware that ", new T(null, 2, 5, U, [Y, "current-component"], null), " is only valid in component\n          functions, and must be called outside of e.g event handlers\n          and ", new T(null, 2, 5, U, [Y, "for"], null), " expressions, so it\u2019s safest to always\n          put the call at the top, as in ", new T(null, 2, 5, U, [Y, "my-div"], null), " here:"], null), new T(null, 2, 5, U, [Cp, new l(null, 
  2, [Pk, Fq, si, function() {
    var a = new T(null, 3, 5, U, [Nj, Gj, nh], null);
    return Aq.b ? Aq.b(a) : Aq.call(null, a);
  }()], null)], null), new T(null, 10, 5, U, [Z, new T(null, 2, 5, U, [ni, "Note: "], null), new T(null, 2, 5, U, [Y, "r/props"], null), " and ", new T(null, 2, 5, U, [Y, "r/children"], null), " correspond to React\u2019s ", new T(null, 2, 5, U, [Y, "this.props"], null), " and ", new T(null, 2, 5, U, [Y, "this.props.children"], null), ",\n         respectively. They may be convenient to use when wrapping\n         native React components, since they follow the same\n         conventions when interpreting the arguments given."], 
  null), new T(null, 2, 5, U, [Wj, "Other news in 0.4.0"], null), new T(null, 7, 5, U, [oh, new T(null, 2, 5, U, [Vh, "React has been updated to version 0.9.0."], null), new T(null, 8, 5, U, [Vh, "You can now use any object that satisfies ", new T(null, 2, 5, U, [Y, "ifn?"], null), " as a component function, and not just\n           plain functions. That includes functions defined with ", new T(null, 2, 5, U, [Y, "deftype"], null), ", ", new T(null, 2, 5, U, [Y, "defrecord"], null), ", etc, as well\n           as collections like maps."], 
  null), new T(null, 7, 5, U, [Vh, new T(null, 2, 5, U, [Y, "reagent.core/set-state"], null), " and ", new T(null, 2, 5, U, [Y, "reagent.core/replace-state"], null), " are now implemented\n           using an ", new T(null, 2, 5, U, [Y, "reagent.core/atom"], null), ", and are\n           consequently async."], null), new T(null, 8, 5, U, [Vh, "Keys associated with items in a seq (e.g \u201ddynamic\n           children\u201d in React parlance) can now be specified with\n           meta-data, as well as with a ", 
  new T(null, 2, 5, U, [Y, ":key"], null), " item in the\n           first parameter as before. In other words, these two forms\n           are now equivalent: ", new T(null, 2, 5, U, [Y, "^{:key foo} [:li bar]"], null), "\n           and ", new T(null, 2, 5, U, [Y, "[:li {:key foo} bar]"], null), "."], null), new T(null, 4, 5, U, [Vh, "Performance has been improved even further. For\n           example, there is now practically no overhead for\n           tracking derefs in components that don\u2019t use ", 
  new T(null, 2, 5, U, [Y, "atom"], null), "s. Allocations and memory use have also\n           been reduced."], null), new T(null, 2, 5, U, [Vh, "Intro and examples have been tweaked a little to take\n          advantage of the new calling conventions."], null)], null), new T(null, 2, 5, U, [Wj, "New svg example"], null), new T(null, 6, 5, U, [Z, "There is also a new, elegant and simple ", new T(null, 3, 5, U, [Sk, b, "example"], null), " of using svg with Reagent, written\n          by ", new T(null, 
  3, 5, U, [Sk, c, "Jonas Enlund"], null), ". It also shows how you can\n          use Reagent\u2019s new calling convensions, and looks like\n          this:"], null), new T(null, 2, 5, U, [Cp, new l(null, 1, [Pk, Dq], null)], null)], null)], null)], null);
}
fp.c("news/any-arguments.html", function() {
  return new T(null, 1, 5, U, [Gq], null);
}, "Reagent 0.4.0: " + v.b("All arguments allowed"));
var Hq = Ap('(ns reagentdemo.news.async\n  (:require [reagent.core :as reagent :refer [atom]]\n            [reagent.interop :refer-macros [.\' .!]]\n            [reagent.debug :refer-macros [dbg println]]\n            [reagentdemo.syntax :refer-macros [get-source]]\n            [sitetools :as tools :refer [link]]\n            [reagentdemo.common :as common :refer [demo-component]]))\n\n(def url "news/reagent-is-async.html")\n(def title "Faster by waiting")\n\n(def funmap (-\x3e "reagentdemo/news/async.cljs" get-source common/fun-map))\n(def src-for (partial common/src-for funmap))\n\n(defn timing-wrapper [f]\n  (let [start-time (atom nil)\n        render-time (atom nil)\n        now #(.now js/Date)\n        start #(reset! start-time (now))\n        stop #(reset! render-time (- (now) @start-time))\n        timed-f (with-meta f\n                  {:component-will-mount start\n                   :component-will-update start\n                   :component-did-mount stop\n                   :component-did-update stop})]\n    (fn []\n      [:div\n       [:p [:em "render time: " @render-time "ms"]]\n       [timed-f]])))\n\n(def base-color (atom {:red 130 :green 160 :blue 120}))\n(def ncolors (atom 20))\n(def random-colors (atom nil))\n\n(defn to-rgb [{:keys [red green blue]}]\n  (let [hex #(str (if (\x3c % 16) "0")\n                  (-\x3e % js/Math.round (.toString 16)))]\n    (str "#" (hex red) (hex green) (hex blue))))\n\n(defn tweak-color [{:keys [red green blue]}]\n  (let [rnd #(-\x3e (js/Math.random) (* 256))\n        tweak #(-\x3e % (+ (rnd)) (/ 2) js/Math.floor)]\n    {:red (tweak red) :green (tweak green) :blue (tweak blue)}))\n\n(defn reset-random-colors [color]\n  (reset! random-colors\n          (repeatedly #(-\x3e color tweak-color to-rgb))))\n\n(defn color-choose [color-part]\n  [:div.color-slider\n   (name color-part) " " (color-part @base-color)\n   [:input {:type "range" :min 0 :max 255\n            :value (color-part @base-color)\n            :on-change (fn [e]\n                         (swap! base-color assoc\n                                color-part (-\x3e e .-target .-value int))\n                         (reset-random-colors @base-color))}]])\n\n(defn ncolors-choose []\n  [:div.color-slider\n   "number of color divs " @ncolors\n   [:input {:type "range" :min 0 :max 500\n            :value @ncolors\n            :on-change #(reset! ncolors (-\x3e % .-target .-value))}]])\n\n(defn color-plate [color]\n  [:div.color-plate\n   {:style {:background-color color}}])\n\n(defn palette []\n  (let [color @base-color\n        n @ncolors]\n    [:div\n     [:div\n      [:p "base color: "]\n      [color-plate (to-rgb color)]]\n     [:div.color-samples\n      [:p n " random matching colors:"]\n      (map-indexed (fn [k v]\n                     ^{:key k} [color-plate v])\n                   (take n @random-colors))]]))\n\n(defn color-demo []\n  (reset-random-colors @base-color)\n  (fn []\n    [:div\n     [:h2 "Matching colors"]\n     [color-choose :red]\n     [color-choose :green]\n     [color-choose :blue]\n     [ncolors-choose]\n     [timing-wrapper palette]]))\n\n(defn main [{:keys [summary]}]\n  (let [om-article {:href "http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/"}]\n    [:div.reagent-demo\n     [:h1 [link {:href url} title]]\n     [:div.demo-text\n      [:h2 "Reagent gets async rendering"]\n\n      [:p "Reagent already separates state from components. Now they\n      are also separated in time."]\n\n      [:p "From version 0.3.0, changes in application state (as\n      represented by Reagent\u2019s " [:code "atom"] "s) are no longer\n      rendered immediately to the DOM. Instead, Reagent waits until\n      the browser is ready to repaint the window, and then all the\n      changes are rendered in one single go."]\n\n      (if summary\n        [link {:href url\n               :class \'news-read-more} "Read more"]\n        [:div.demo-text\n\n         [:p "This is good for all sorts of reasons:"]\n         [:ul\n\n          [:li "Reagent doesn\'t have to spend time doing renderings\n          that no one would ever see (because changes to application\n          state happened faster than the browser could repaint)."]\n\n          [:li "If two or more atoms are changed simultaneously, this\n          now leads to only one re-rendering, and not two."]\n\n          [:li "The new code does proper batching of renderings even\n          when changes to atoms are done outside of event\n          handlers (which is great for e.g core.async users)."]\n\n          [:li "Repaints can be synced by the browser with for example\n          CSS transitions, since Reagent uses requestAnimationFrame\n          to do the batching. That makes for example animations\n          smoother."]]\n\n         [:p "In short, Reagent renders less often, but at the right\n         times. For a much better description of why async rendering\n         is good, see David Nolen\u2019s " [:a om-article "excellent\n         explanation here."]]\n\n         [:h2 "The bad news"]\n\n         [:p "Lunches in general tend to be non-free, and this is no\n         exception\u2026 The downside to async rendering is that you can no\n         longer depend on changes to atoms being immediately available\n         in the DOM. (Actually, you couldn\u2019t before either, since\n         React.js itself does batching inside event handlers.)"]\n\n         [:p "This may make testing a bit more verbose: you now have\n         to call " [:code "reagent.core/flush"] " to force Reagent to\n         synchronize state with the DOM."]\n\n         [:h2 "An example"]\n\n         [:p "Here is an example to (hopefully) demonstrate the\n         virtues of async rendering. It consists of a simple color\n         chooser (three sliders to set the red, green and blue\n         components of a base color), and shows the base color + a\n         bunch of divs in random matching colors. As soon as the base\n         color is changed, a new set of random colors is shown."]\n\n         [:p "If you change one of the base color components, the base\n         color should change immediately, and smoothly (on my Macbook\n         Air, rendering takes around 2ms, with 20 colored divs\n         showing)."]\n\n         [:p "But perhaps more interesting is to see what happens when\n         the updates can\u2019t be made smoothly (because the browser\n         simply cannot re-render the colored divs quickly enough). On\n         my machine, this starts to happen if I change the number of\n         divs shown to above 150 or so."]\n\n         [:p "As you increase the number of divs, you\u2019ll notice that\n         the base color no longer changes quite so smoothly when you\n         move the color sliders."]\n\n         [:p "But the crucial point is that the sliders "\n          [:strong "still work"] ". Without async rendering, you could\n         quickly get into a situation where the browser hangs for a\n         while, doing updates corresponding to an old state. "]\n\n         [:p "With async rendering, the only thing that happens is\n         that the frame rate goes down."]\n\n         [:p "Btw, I find it quite impressive that React manages to\n         change 500 divs (12 full screens worth) in slightly more than\n         40ms. And even better: when I change the number of divs\n         shown, it only takes around 6ms to re-render the color\n         palette (because the individual divs don\u2019t have to be\n         re-rendered, divs are just added or removed from the DOM as\n         needed)."]\n\n         [demo-component\n          {:comp color-demo\n           :src (src-for\n                 [:ns :timing-wrapper :base-color :ncolors\n                  :random-colors :to-rgb :tweak-color\n                  :reset-random-colors :color-choose :ncolors-choose\n                  :palette :color-demo])}]])]]))\n\n(tools/register-page url (fn [] [main])\n                     (str "Reagent: " title))\n'), 
Iq = be.a(Bp, Hq);
function Jq(a) {
  var b = Em.b(null), c = Em.b(null), d = function() {
    return function() {
      return Date.now();
    };
  }(b, c), e = function(a, b, c) {
    return function() {
      var b = c();
      return Q.a ? Q.a(a, b) : Q.call(null, a, b);
    };
  }(b, c, d), f = function(a, b, c) {
    return function() {
      var d = c() - (G.b ? G.b(a) : G.call(null, a));
      return Q.a ? Q.a(b, d) : Q.call(null, b, d);
    };
  }(b, c, d, e);
  a = ad(a, new l(null, 4, [Rj, e, zi, e, ei, f, pi, f], null));
  return function(a, b, c, d, e, f) {
    return function() {
      return new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Z, new T(null, 4, 5, U, [ni, "render time: ", G.b ? G.b(b) : G.call(null, b), "ms"], null)], null), new T(null, 1, 5, U, [f], null)], null);
    };
  }(b, c, d, e, f, a);
}
var Kq = Em.b(new l(null, 3, [wj, 130, $h, 160, yj, 120], null)), Lq = Em.b(20), Mq = Em.b(null);
function Nq(a) {
  var b = md(a) ? O.a(ee, a) : a, c = M.a(b, yj), d = M.a(b, $h), e = M.a(b, wj);
  a = function() {
    return function(a) {
      return "" + v.b(16 > a ? "0" : null) + v.b(Math.round(a).toString(16));
    };
  }(a, b, c, d, e);
  return "#" + v.b(a(e)) + v.b(a(d)) + v.b(a(c));
}
function Oq(a) {
  var b = md(a) ? O.a(ee, a) : a, c = M.a(b, yj), d = M.a(b, $h), e = M.a(b, wj);
  a = function(a) {
    return function(b) {
      b = (b + a()) / 2;
      return Math.floor(b);
    };
  }(function() {
    return function() {
      return 256 * Math.random();
    };
  }(a, b, c, d, e), a, b, c, d, e);
  return new l(null, 3, [wj, a(e), $h, a(d), yj, a(c)], null);
}
function Pq() {
  var a = G.b ? G.b(Kq) : G.call(null, Kq), b = me.b(function() {
    return function() {
      return Nq(Oq(a));
    };
  }(Mq));
  return Q.a ? Q.a(Mq, b) : Q.call(null, Mq, b);
}
function Qq(a) {
  return new T(null, 5, 5, U, [Jk, Ed(a), " ", function() {
    var b = G.b ? G.b(Kq) : G.call(null, Kq);
    return a.b ? a.b(b) : a.call(null, b);
  }(), new T(null, 2, 5, U, [jk, new l(null, 5, [ri, "range", Yg, 0, lj, 255, Yh, function() {
    var b = G.b ? G.b(Kq) : G.call(null, Kq);
    return a.b ? a.b(b) : a.call(null, b);
  }(), rk, function(b) {
    S.j(Kq, N, a, b.target.value | 0);
    return Pq();
  }], null)], null)], null);
}
function Rq() {
  return new T(null, 4, 5, U, [Jk, "number of color divs ", G.b ? G.b(Lq) : G.call(null, Lq), new T(null, 2, 5, U, [jk, new l(null, 5, [ri, "range", Yg, 0, lj, 500, Yh, G.b ? G.b(Lq) : G.call(null, Lq), rk, function(a) {
    a = a.target.value;
    return Q.a ? Q.a(Lq, a) : Q.call(null, Lq, a);
  }], null)], null)], null);
}
function Sq(a) {
  return new T(null, 2, 5, U, [ih, new l(null, 1, [Qi, new l(null, 1, [fi, a], null)], null)], null);
}
function Tq() {
  var a = G.b ? G.b(Kq) : G.call(null, Kq), b = G.b ? G.b(Lq) : G.call(null, Lq);
  return new T(null, 3, 5, U, [Ri, new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Z, "base color: "], null), new T(null, 2, 5, U, [Sq, Nq(a)], null)], null), new T(null, 3, 5, U, [vi, new T(null, 3, 5, U, [Z, b, " random matching colors:"], null), ce(function() {
    return function(a, b) {
      return ad(new T(null, 2, 5, U, [Sq, b], null), new l(null, 1, [th, a], null));
    };
  }(a, b), ie.a(b, G.b ? G.b(Mq) : G.call(null, Mq)))], null)], null);
}
function Uq() {
  Pq();
  return function() {
    return new T(null, 7, 5, U, [Ri, new T(null, 2, 5, U, [Wj, "Matching colors"], null), new T(null, 2, 5, U, [Qq, wj], null), new T(null, 2, 5, U, [Qq, $h], null), new T(null, 2, 5, U, [Qq, yj], null), new T(null, 1, 5, U, [Rq], null), new T(null, 2, 5, U, [Jq, Tq], null)], null);
  };
}
function Vq(a) {
  a = md(a) ? O.a(ee, a) : a;
  var b = M.a(a, Ti), c = new l(null, 1, [Ok, "http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/"], null);
  a = U;
  var d = new T(null, 2, 5, U, [ok, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "news/reagent-is-async.html"], null), "Faster by waiting"], null)], null), e = U, f = new T(null, 2, 5, U, [Wj, "Reagent gets async rendering"], null), h = new T(null, 2, 5, U, [Z, "Reagent already separates state from components. Now they\n      are also separated in time."], null), m = new T(null, 4, 5, U, [Z, "From version 0.3.0, changes in application state (as\n      represented by Reagent\u2019s ", new T(null, 
  2, 5, U, [Y, "atom"], null), "s) are no longer\n      rendered immediately to the DOM. Instead, Reagent waits until\n      the browser is ready to repaint the window, and then all the\n      changes are rendered in one single go."], null);
  if (q(b)) {
    b = new T(null, 3, 5, U, [gp, new l(null, 2, [Ok, "news/reagent-is-async.html", vj, new Cc(null, "news-read-more", "news-read-more", 150793143, null)], null), "Read more"], null);
  } else {
    var b = U, n = new T(null, 2, 5, U, [Z, "This is good for all sorts of reasons:"], null), p = new T(null, 5, 5, U, [oh, new T(null, 2, 5, U, [Vh, "Reagent doesn't have to spend time doing renderings\n          that no one would ever see (because changes to application\n          state happened faster than the browser could repaint)."], null), new T(null, 2, 5, U, [Vh, "If two or more atoms are changed simultaneously, this\n          now leads to only one re-rendering, and not two."], null), new T(null, 
    2, 5, U, [Vh, "The new code does proper batching of renderings even\n          when changes to atoms are done outside of event\n          handlers (which is great for e.g core.async users)."], null), new T(null, 2, 5, U, [Vh, "Repaints can be synced by the browser with for example\n          CSS transitions, since Reagent uses requestAnimationFrame\n          to do the batching. That makes for example animations\n          smoother."], null)], null), c = new T(null, 3, 5, U, [Z, "In short, Reagent renders less often, but at the right\n         times. For a much better description of why async rendering\n         is good, see David Nolen\u2019s ", 
    new T(null, 3, 5, U, [Sk, c, "excellent\n         explanation here."], null)], null), s = new T(null, 2, 5, U, [Wj, "The bad news"], null), t = new T(null, 2, 5, U, [Z, "Lunches in general tend to be non-free, and this is no\n         exception\u2026 The downside to async rendering is that you can no\n         longer depend on changes to atoms being immediately available\n         in the DOM. (Actually, you couldn\u2019t before either, since\n         React.js itself does batching inside event handlers.)"], 
    null), w = new T(null, 4, 5, U, [Z, "This may make testing a bit more verbose: you now have\n         to call ", new T(null, 2, 5, U, [Y, "reagent.core/flush"], null), " to force Reagent to\n         synchronize state with the DOM."], null), B = new T(null, 2, 5, U, [Wj, "An example"], null), z = new T(null, 2, 5, U, [Z, "Here is an example to (hopefully) demonstrate the\n         virtues of async rendering. It consists of a simple color\n         chooser (three sliders to set the red, green and blue\n         components of a base color), and shows the base color + a\n         bunch of divs in random matching colors. As soon as the base\n         color is changed, a new set of random colors is shown."], 
    null), F = new T(null, 2, 5, U, [Z, "If you change one of the base color components, the base\n         color should change immediately, and smoothly (on my Macbook\n         Air, rendering takes around 2ms, with 20 colored divs\n         showing)."], null), K = new T(null, 2, 5, U, [Z, "But perhaps more interesting is to see what happens when\n         the updates can\u2019t be made smoothly (because the browser\n         simply cannot re-render the colored divs quickly enough). On\n         my machine, this starts to happen if I change the number of\n         divs shown to above 150 or so."], 
    null), R = new T(null, 2, 5, U, [Z, "As you increase the number of divs, you\u2019ll notice that\n         the base color no longer changes quite so smoothly when you\n         move the color sliders."], null), X = new T(null, 4, 5, U, [Z, "But the crucial point is that the sliders ", new T(null, 2, 5, U, [Rh, "still work"], null), ". Without async rendering, you could\n         quickly get into a situation where the browser hangs for a\n         while, doing updates corresponding to an old state. "], 
    null), da = new T(null, 2, 5, U, [Z, "With async rendering, the only thing that happens is\n         that the frame rate goes down."], null), ta = new T(null, 2, 5, U, [Z, "Btw, I find it quite impressive that React manages to\n         change 500 divs (12 full screens worth) in slightly more than\n         40ms. And even better: when I change the number of divs\n         shown, it only takes around 6ms to re-render the color\n         palette (because the individual divs don\u2019t have to be\n         re-rendered, divs are just added or removed from the DOM as\n         needed)."], 
    null), Xa = U, H;
    H = new T(null, 12, 5, U, [Ph, jh, Ck, Kh, Wg, Mh, bk, kk, sk, ui, Xh, jj], null);
    H = Iq.b ? Iq.b(H) : Iq.call(null, H);
    b = new T(null, 16, 5, b, [Hj, n, p, c, s, t, w, B, z, F, K, R, X, da, ta, new T(null, 2, 5, Xa, [Cp, new l(null, 2, [Pk, Uq, si, H], null)], null)], null);
  }
  return new T(null, 3, 5, a, [ek, d, new T(null, 5, 5, e, [Hj, f, h, m, b], null)], null);
}
fp.c("news/reagent-is-async.html", function() {
  return new T(null, 1, 5, U, [Vq], null);
}, "Reagent: " + v.b("Faster by waiting"));
function Wq(a, b) {
  return new T(null, 2, 5, U, [Wi, new l(null, 1, [vj, q(0 != (a & 1 << b)) ? "light" : "dark"], null)], null);
}
function Xq(a) {
  return new T(null, 6, 5, U, [xi, new T(null, 3, 5, U, [Wq, a, 3], null), new T(null, 3, 5, U, [Wq, a, 2], null), new T(null, 3, 5, U, [Wq, a, 1], null), new T(null, 3, 5, U, [Wq, a, 0], null), new T(null, 2, 5, U, [Wi, a], null)], null);
}
function Yq(a) {
  return new T(null, 3, 5, U, [tj, new T(null, 2, 5, U, [Xq, xd(a, 10)], null), new T(null, 2, 5, U, [Xq, (a % 10 + 10) % 10], null)], null);
}
var Zq = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    return se.a(new T(null, 1, 5, U, [ch], null), he.a(be.a(Qe, Wi), a));
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function $q(a, b, c) {
  return new T(null, 7, 5, U, [Ei, new l(null, 2, [Ii, c, vj, q(b) ? "wide" : null], null), new T(null, 5, 5, U, [Zq, 8, 4, 2, 1], null), new T(null, 2, 5, U, [Yq, a.getHours()], null), new T(null, 2, 5, U, [Yq, a.getMinutes()], null), new T(null, 2, 5, U, [Yq, a.getSeconds()], null), q(b) ? new T(null, 2, 5, U, [Yq, xd(a.getMilliseconds(), 10)], null) : null], null);
}
var ar = Em.b(new l(null, 2, [bi, new Date, Zg, !1], null));
function br() {
  return S.j(ar, N, bi, new Date);
}
function cr() {
  var a = G.b ? G.b(ar) : G.call(null, ar), b = md(a) ? O.a(ee, a) : a, c = M.a(b, Zg), d = M.a(b, bi);
  q(c) ? Fm(br) : setTimeout(br, 1E3);
  return new T(null, 4, 5, U, [$q, d, c, function() {
    return function() {
      return S.j(ar, we, new T(null, 1, 5, U, [Zg], null), Ua);
    };
  }(a, b, c, d)], null);
}
;var dr = Ap('(ns reagentdemo.news.binaryclock\n  (:require [reagent.core :as r :refer [atom]]))\n\n(defn cell [n bit]\n  [:div.clock-cell {:class (if (bit-test n bit)\n                             "light"\n                             "dark")}])\n\n(defn column [n]\n  [:div.clock-col\n   [cell n 3]\n   [cell n 2]\n   [cell n 1]\n   [cell n 0]\n   [:div.clock-cell n]])\n\n(defn column-pair [n]\n  [:div.clock-pair\n   [column (quot n 10)]\n   [column (mod n 10)]])\n\n(defn legend [\x26 items]\n  (into [:div.clock-col.clock-legend]\n        (map (partial vector :div.clock-cell)\n             items)))\n\n(defn clock [date show-100s toggle-100s]\n  [:div.clock-main {:on-click toggle-100s\n                    :class (when show-100s "wide")}\n   [legend 8 4 2 1]\n   [column-pair (.getHours date)]\n   [column-pair (.getMinutes date)]\n   [column-pair (.getSeconds date)]\n   (when show-100s\n     [column-pair (-\x3e (.getMilliseconds date)\n                      (quot 10))])])\n\n(def clock-state (atom {:time (js/Date.)\n                        :show-100s false}))\n\n(defn update-time []\n  (swap! clock-state assoc :time (js/Date.)))\n\n(defn main []\n  (let [{:keys [time show-100s]} @clock-state]\n    (if show-100s\n      (r/next-tick update-time)\n      (js/setTimeout update-time 1000))\n    [clock time show-100s\n     #(swap! clock-state update-in [:show-100s] not)]))\n'), 
er = be.a(Bp, dr), fr = function() {
  function a(a) {
    var d = null;
    0 < arguments.length && (d = E(Array.prototype.slice.call(arguments, 0), 0));
    return b.call(this, d);
  }
  function b(a) {
    var b = U;
    a = Pe(a);
    a = er.b ? er.b(a) : er.call(null, a);
    return new T(null, 2, 5, b, [Cp, new l(null, 2, [si, a, Oj, !0], null)], null);
  }
  a.m = 0;
  a.h = function(a) {
    a = y(a);
    return b(a);
  };
  a.e = b;
  return a;
}();
function gr(a) {
  a = md(a) ? O.a(ee, a) : a;
  a = M.a(a, Ti);
  var b = new l(null, 1, [Ok, "https://github.com/reagent-project/reagent/blob/master/demo/reagentdemo/news/binaryclock.cljs"], null);
  return new T(null, 3, 5, U, [ek, new T(null, 2, 5, U, [ok, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "news/binary-clock.html"], null), "A binary clock"], null)], null), new T(null, 5, 5, U, [Hj, q(a) ? null : new T(null, 3, 5, U, [Ri, new T(null, 2, 5, U, [Mk, new T(null, 1, 5, U, [cr], null)], null), new T(null, 2, 5, U, [Ri, new T(null, 2, 5, U, [Rh, "Click to toggle 1/100th seconds."], null)], null)], null), new T(null, 10, 5, U, [Z, "Fredrik Dyrkell wrote a very nice ", new T(null, 3, 5, 
  U, [Sk, new l(null, 1, [Ok, "http://www.lexicallyscoped.com/2014/01/23/clojurescript-react-om-binary-clock.html"], null), "binary\n      clock"], null), " using ", new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "https://github.com/swannodette/om"], null), "Om"], null), ". I thought I\u2019d replicate that\n      using Reagent for fun (another re-write, using ", new T(null, 3, 5, U, [Sk, new l(null, 1, [Ok, "http://hoplon.io"], null), "Hoplon"], null), ", can be seen ", new T(null, 3, 5, U, [Sk, 
  new l(null, 1, [Ok, "http://pmbauer.github.io/2014/01/27/hoplon-binary-clock/"], null), "here"], null), ")."], null), new T(null, 2, 5, U, [Z, "So, without further ado, here is a binary clock using Reagent."], null), q(a) ? new T(null, 3, 5, U, [gp, new l(null, 2, [Ok, "news/binary-clock.html", vj, new Cc(null, "news-read-mode", "news-read-mode", 80085219, null)], null), "Read more"], null) : new T(null, 23, 5, U, [Hj, new T(null, 2, 5, U, [fr, Nj], null), new T(null, 2, 5, U, [Z, "We start with the basics: The clock is built out of\n         cells, with a light colour if the bit the cell corresponds to\n         is set."], 
  null), new T(null, 2, 5, U, [fr, Ak], null), new T(null, 2, 5, U, [Z, "Cells are combined into columns of four bits, with a\n         decimal digit at the bottom."], null), new T(null, 2, 5, U, [fr, Ni], null), new T(null, 2, 5, U, [Z, "Columns are in turn combined into pairs:"], null), new T(null, 2, 5, U, [fr, Sg], null), new T(null, 2, 5, U, [Z, "We'll also need the legend on the left side:"], null), new T(null, 2, 5, U, [fr, mi], null), new T(null, 2, 5, U, [Z, "We combine these element into a component that shows the\n         legend, hours, minutes and seconds; and optionally 1/100\n         seconds. It also responds to clicks."], 
  null), new T(null, 2, 5, U, [fr, ck], null), new T(null, 2, 5, U, [Z, "We also need to keep track of the time, and of the\n         detail shown, in a Reagent atom. And a function to update the\n         time."], null), new T(null, 3, 5, U, [fr, Rg, Uh], null), new T(null, 8, 5, U, [Z, "And finally we use the ", new T(null, 2, 5, U, [Y, "clock"], null), " component.\n         The current time is scheduled to be updated, after a suitable\n         delay, every time the main component is rendered (", 
  new T(null, 2, 5, U, [Y, "reagent.core/next-tick"], null), " is just a front for ", new T(null, 2, 5, U, [Y, "requestAnimationFrame"], null), "):"], null), new T(null, 2, 5, U, [fr, tk], null), new T(null, 4, 5, U, [Z, "The entire source is also available ", new T(null, 3, 5, U, [Sk, b, "here"], null), "."], null), new T(null, 2, 5, U, [Wj, "How it all works"], null), new T(null, 2, 5, U, [Z, "Reading through the source, it may look like the entire\n         clock component is recreated from scratch whenever the time\n         changes. "], 
  null), new T(null, 4, 5, U, [Z, "That is an illusion: Reagent and React together\n         makes sure that only the parts of the DOM that actually need\n         to change are updated. For example, the ", new T(null, 2, 5, U, [Y, "column-pair"], null), " function corresponding to hours only\n         runs once every hour."], null), new T(null, 2, 5, U, [Z, "And that\u2019s what makes Reagent and React fast. Try\n         clicking on the clock to toggle the display of 1/100th\n         seconds. Most browsers should have no trouble at all keeping\n         up (even if they won\u2019t actually show every 1/100th second:\n         they are typically limited to roughly 60 fps)."], 
  null), new T(null, 2, 5, U, [Z, "But it is a very handy illusion. Almost the entire UI is\n         made up of pure functions, transforming immutable data into\n         other immutable data structures. That makes them easy to\n         reason about, and trivial to test. You don\u2019t have to care\n         about \u201dmodel objects\u201d, or about how to update the DOM\n         efficiently. "], null), new T(null, 2, 5, U, [Z, "Just pass arguments to component functions, return a UI\n         description that corresponds to those arguments, and leave it\n         to React to actually display that UI."], 
  null)], null)], null)], null);
}
fp.c("news/binary-clock.html", function() {
  return new T(null, 1, 5, U, [gr], null);
}, "Reagent: " + v.b("A binary clock"));
function hr() {
  return new T(null, 5, 5, U, [Ri, new T(null, 2, 5, U, [gr, new l(null, 1, [Ti, !0], null)], null), new T(null, 2, 5, U, [Gq, new l(null, 1, [Ti, !0], null)], null), new T(null, 2, 5, U, [Vq, new l(null, 1, [Ti, !0], null)], null), new T(null, 2, 5, U, [hq, new l(null, 1, [Ti, !0], null)], null)], null);
}
;eval('if (typeof React !\x3d \'undefined\' \x26\x26 typeof console !\x3d \'undefined\') { console.log(\'React is already defined\'); } else { (function (exports, module) { /**\n * React v0.12.1\n *\n * Copyright 2013-2014, Facebook, Inc.\n * All rights reserved.\n *\n * This source code is licensed under the BSD-style license found in the\n * LICENSE file in the root directory of this source tree. An additional grant\n * of patent rights can be found in the PATENTS file in the same directory.\n *\n */\n!function(e){if("object"\x3d\x3dtypeof exports\x26\x26"undefined"!\x3dtypeof module)module.exports\x3de();else if("function"\x3d\x3dtypeof define\x26\x26define.amd)define([],e);else{var t;"undefined"!\x3dtypeof window?t\x3dwindow:"undefined"!\x3dtypeof global?t\x3dglobal:"undefined"!\x3dtypeof self\x26\x26(t\x3dself),t.React\x3de()}}(function(){return function e(t,n,r){function o(i,s){if(!n[i]){if(!t[i]){var u\x3d"function"\x3d\x3dtypeof require\x26\x26require;if(!s\x26\x26u)return u(i,!0);if(a)return a(i,!0);var c\x3dnew Error("Cannot find module \'"+i+"\'");throw c.code\x3d"MODULE_NOT_FOUND",c}var l\x3dn[i]\x3d{exports:{}};t[i][0].call(l.exports,function(e){var n\x3dt[i][1][e];return o(n?n:e)},l,l.exports,e,t,n,r)}return n[i].exports}for(var a\x3d"function"\x3d\x3dtypeof require\x26\x26require,i\x3d0;i\x3cr.length;i++)o(r[i]);return o}({1:[function(e,t){"use strict";var n\x3de("./DOMPropertyOperations"),r\x3de("./EventPluginUtils"),o\x3de("./ReactChildren"),a\x3de("./ReactComponent"),i\x3de("./ReactCompositeComponent"),s\x3de("./ReactContext"),u\x3de("./ReactCurrentOwner"),c\x3de("./ReactElement"),l\x3d(e("./ReactElementValidator"),e("./ReactDOM")),p\x3de("./ReactDOMComponent"),d\x3de("./ReactDefaultInjection"),f\x3de("./ReactInstanceHandles"),h\x3de("./ReactLegacyElement"),m\x3de("./ReactMount"),v\x3de("./ReactMultiChild"),g\x3de("./ReactPerf"),y\x3de("./ReactPropTypes"),E\x3de("./ReactServerRendering"),C\x3de("./ReactTextComponent"),R\x3de("./Object.assign"),M\x3de("./deprecated"),b\x3de("./onlyChild");d.inject();var O\x3dc.createElement,D\x3dc.createFactory;O\x3dh.wrapCreateElement(O),D\x3dh.wrapCreateFactory(D);var x\x3dg.measure("React","render",m.render),P\x3d{Children:{map:o.map,forEach:o.forEach,count:o.count,only:b},DOM:l,PropTypes:y,initializeTouchEvents:function(e){r.useTouchEvents\x3de},createClass:i.createClass,createElement:O,createFactory:D,constructAndRenderComponent:m.constructAndRenderComponent,constructAndRenderComponentByID:m.constructAndRenderComponentByID,render:x,renderToString:E.renderToString,renderToStaticMarkup:E.renderToStaticMarkup,unmountComponentAtNode:m.unmountComponentAtNode,isValidClass:h.isValidClass,isValidElement:c.isValidElement,withContext:s.withContext,__spread:R,renderComponent:M("React","renderComponent","render",this,x),renderComponentToString:M("React","renderComponentToString","renderToString",this,E.renderToString),renderComponentToStaticMarkup:M("React","renderComponentToStaticMarkup","renderToStaticMarkup",this,E.renderToStaticMarkup),isValidComponent:M("React","isValidComponent","isValidElement",this,c.isValidElement)};"undefined"!\x3dtypeof __REACT_DEVTOOLS_GLOBAL_HOOK__\x26\x26"function"\x3d\x3dtypeof __REACT_DEVTOOLS_GLOBAL_HOOK__.inject\x26\x26__REACT_DEVTOOLS_GLOBAL_HOOK__.inject({Component:a,CurrentOwner:u,DOMComponent:p,DOMPropertyOperations:n,InstanceHandles:f,Mount:m,MultiChild:v,TextComponent:C});P.version\x3d"0.12.1",t.exports\x3dP},{"./DOMPropertyOperations":12,"./EventPluginUtils":20,"./Object.assign":27,"./ReactChildren":31,"./ReactComponent":32,"./ReactCompositeComponent":34,"./ReactContext":35,"./ReactCurrentOwner":36,"./ReactDOM":37,"./ReactDOMComponent":39,"./ReactDefaultInjection":49,"./ReactElement":50,"./ReactElementValidator":51,"./ReactInstanceHandles":58,"./ReactLegacyElement":59,"./ReactMount":61,"./ReactMultiChild":62,"./ReactPerf":66,"./ReactPropTypes":70,"./ReactServerRendering":74,"./ReactTextComponent":76,"./deprecated":104,"./onlyChild":135}],2:[function(e,t){"use strict";var n\x3de("./focusNode"),r\x3d{componentDidMount:function(){this.props.autoFocus\x26\x26n(this.getDOMNode())}};t.exports\x3dr},{"./focusNode":109}],3:[function(e,t){"use strict";function n(){var e\x3dwindow.opera;return"object"\x3d\x3dtypeof e\x26\x26"function"\x3d\x3dtypeof e.version\x26\x26parseInt(e.version(),10)\x3c\x3d12}function r(e){return(e.ctrlKey||e.altKey||e.metaKey)\x26\x26!(e.ctrlKey\x26\x26e.altKey)}var o\x3de("./EventConstants"),a\x3de("./EventPropagators"),i\x3de("./ExecutionEnvironment"),s\x3de("./SyntheticInputEvent"),u\x3de("./keyOf"),c\x3di.canUseDOM\x26\x26"TextEvent"in window\x26\x26!("documentMode"in document||n()),l\x3d32,p\x3dString.fromCharCode(l),d\x3do.topLevelTypes,f\x3d{beforeInput:{phasedRegistrationNames:{bubbled:u({onBeforeInput:null}),captured:u({onBeforeInputCapture:null})},dependencies:[d.topCompositionEnd,d.topKeyPress,d.topTextInput,d.topPaste]}},h\x3dnull,m\x3d!1,v\x3d{eventTypes:f,extractEvents:function(e,t,n,o){var i;if(c)switch(e){case d.topKeyPress:var u\x3do.which;if(u!\x3d\x3dl)return;m\x3d!0,i\x3dp;break;case d.topTextInput:if(i\x3do.data,i\x3d\x3d\x3dp\x26\x26m)return;break;default:return}else{switch(e){case d.topPaste:h\x3dnull;break;case d.topKeyPress:o.which\x26\x26!r(o)\x26\x26(h\x3dString.fromCharCode(o.which));break;case d.topCompositionEnd:h\x3do.data}if(null\x3d\x3d\x3dh)return;i\x3dh}if(i){var v\x3ds.getPooled(f.beforeInput,n,o);return v.data\x3di,h\x3dnull,a.accumulateTwoPhaseDispatches(v),v}}};t.exports\x3dv},{"./EventConstants":16,"./EventPropagators":21,"./ExecutionEnvironment":22,"./SyntheticInputEvent":87,"./keyOf":131}],4:[function(e,t){"use strict";function n(e,t){return e+t.charAt(0).toUpperCase()+t.substring(1)}var r\x3d{columnCount:!0,fillOpacity:!0,flex:!0,flexGrow:!0,flexShrink:!0,fontWeight:!0,lineClamp:!0,lineHeight:!0,opacity:!0,order:!0,orphans:!0,widows:!0,zIndex:!0,zoom:!0},o\x3d["Webkit","ms","Moz","O"];Object.keys(r).forEach(function(e){o.forEach(function(t){r[n(t,e)]\x3dr[e]})});var a\x3d{background:{backgroundImage:!0,backgroundPosition:!0,backgroundRepeat:!0,backgroundColor:!0},border:{borderWidth:!0,borderStyle:!0,borderColor:!0},borderBottom:{borderBottomWidth:!0,borderBottomStyle:!0,borderBottomColor:!0},borderLeft:{borderLeftWidth:!0,borderLeftStyle:!0,borderLeftColor:!0},borderRight:{borderRightWidth:!0,borderRightStyle:!0,borderRightColor:!0},borderTop:{borderTopWidth:!0,borderTopStyle:!0,borderTopColor:!0},font:{fontStyle:!0,fontVariant:!0,fontWeight:!0,fontSize:!0,lineHeight:!0,fontFamily:!0}},i\x3d{isUnitlessNumber:r,shorthandPropertyExpansions:a};t.exports\x3di},{}],5:[function(e,t){"use strict";var n\x3de("./CSSProperty"),r\x3de("./ExecutionEnvironment"),o\x3d(e("./camelizeStyleName"),e("./dangerousStyleValue")),a\x3de("./hyphenateStyleName"),i\x3de("./memoizeStringOnly"),s\x3d(e("./warning"),i(function(e){return a(e)})),u\x3d"cssFloat";r.canUseDOM\x26\x26void 0\x3d\x3d\x3ddocument.documentElement.style.cssFloat\x26\x26(u\x3d"styleFloat");var c\x3d{createMarkupForStyles:function(e){var t\x3d"";for(var n in e)if(e.hasOwnProperty(n)){var r\x3de[n];null!\x3dr\x26\x26(t+\x3ds(n)+":",t+\x3do(n,r)+";")}return t||null},setValueForStyles:function(e,t){var r\x3de.style;for(var a in t)if(t.hasOwnProperty(a)){var i\x3do(a,t[a]);if("float"\x3d\x3d\x3da\x26\x26(a\x3du),i)r[a]\x3di;else{var s\x3dn.shorthandPropertyExpansions[a];if(s)for(var c in s)r[c]\x3d"";else r[a]\x3d""}}}};t.exports\x3dc},{"./CSSProperty":4,"./ExecutionEnvironment":22,"./camelizeStyleName":98,"./dangerousStyleValue":103,"./hyphenateStyleName":122,"./memoizeStringOnly":133,"./warning":141}],6:[function(e,t){"use strict";function n(){this._callbacks\x3dnull,this._contexts\x3dnull}var r\x3de("./PooledClass"),o\x3de("./Object.assign"),a\x3de("./invariant");o(n.prototype,{enqueue:function(e,t){this._callbacks\x3dthis._callbacks||[],this._contexts\x3dthis._contexts||[],this._callbacks.push(e),this._contexts.push(t)},notifyAll:function(){var e\x3dthis._callbacks,t\x3dthis._contexts;if(e){a(e.length\x3d\x3d\x3dt.length),this._callbacks\x3dnull,this._contexts\x3dnull;for(var n\x3d0,r\x3de.length;r\x3en;n++)e[n].call(t[n]);e.length\x3d0,t.length\x3d0}},reset:function(){this._callbacks\x3dnull,this._contexts\x3dnull},destructor:function(){this.reset()}}),r.addPoolingTo(n),t.exports\x3dn},{"./Object.assign":27,"./PooledClass":28,"./invariant":124}],7:[function(e,t){"use strict";function n(e){return"SELECT"\x3d\x3d\x3de.nodeName||"INPUT"\x3d\x3d\x3de.nodeName\x26\x26"file"\x3d\x3d\x3de.type}function r(e){var t\x3dM.getPooled(P.change,w,e);E.accumulateTwoPhaseDispatches(t),R.batchedUpdates(o,t)}function o(e){y.enqueueEvents(e),y.processEventQueue()}function a(e,t){_\x3de,w\x3dt,_.attachEvent("onchange",r)}function i(){_\x26\x26(_.detachEvent("onchange",r),_\x3dnull,w\x3dnull)}function s(e,t,n){return e\x3d\x3d\x3dx.topChange?n:void 0}function u(e,t,n){e\x3d\x3d\x3dx.topFocus?(i(),a(t,n)):e\x3d\x3d\x3dx.topBlur\x26\x26i()}function c(e,t){_\x3de,w\x3dt,T\x3de.value,N\x3dObject.getOwnPropertyDescriptor(e.constructor.prototype,"value"),Object.defineProperty(_,"value",k),_.attachEvent("onpropertychange",p)}function l(){_\x26\x26(delete _.value,_.detachEvent("onpropertychange",p),_\x3dnull,w\x3dnull,T\x3dnull,N\x3dnull)}function p(e){if("value"\x3d\x3d\x3de.propertyName){var t\x3de.srcElement.value;t!\x3d\x3dT\x26\x26(T\x3dt,r(e))}}function d(e,t,n){return e\x3d\x3d\x3dx.topInput?n:void 0}function f(e,t,n){e\x3d\x3d\x3dx.topFocus?(l(),c(t,n)):e\x3d\x3d\x3dx.topBlur\x26\x26l()}function h(e){return e!\x3d\x3dx.topSelectionChange\x26\x26e!\x3d\x3dx.topKeyUp\x26\x26e!\x3d\x3dx.topKeyDown||!_||_.value\x3d\x3d\x3dT?void 0:(T\x3d_.value,w)}function m(e){return"INPUT"\x3d\x3d\x3de.nodeName\x26\x26("checkbox"\x3d\x3d\x3de.type||"radio"\x3d\x3d\x3de.type)}function v(e,t,n){return e\x3d\x3d\x3dx.topClick?n:void 0}var g\x3de("./EventConstants"),y\x3de("./EventPluginHub"),E\x3de("./EventPropagators"),C\x3de("./ExecutionEnvironment"),R\x3de("./ReactUpdates"),M\x3de("./SyntheticEvent"),b\x3de("./isEventSupported"),O\x3de("./isTextInputElement"),D\x3de("./keyOf"),x\x3dg.topLevelTypes,P\x3d{change:{phasedRegistrationNames:{bubbled:D({onChange:null}),captured:D({onChangeCapture:null})},dependencies:[x.topBlur,x.topChange,x.topClick,x.topFocus,x.topInput,x.topKeyDown,x.topKeyUp,x.topSelectionChange]}},_\x3dnull,w\x3dnull,T\x3dnull,N\x3dnull,I\x3d!1;C.canUseDOM\x26\x26(I\x3db("change")\x26\x26(!("documentMode"in document)||document.documentMode\x3e8));var S\x3d!1;C.canUseDOM\x26\x26(S\x3db("input")\x26\x26(!("documentMode"in document)||document.documentMode\x3e9));var k\x3d{get:function(){return N.get.call(this)},set:function(e){T\x3d""+e,N.set.call(this,e)}},A\x3d{eventTypes:P,extractEvents:function(e,t,r,o){var a,i;if(n(t)?I?a\x3ds:i\x3du:O(t)?S?a\x3dd:(a\x3dh,i\x3df):m(t)\x26\x26(a\x3dv),a){var c\x3da(e,t,r);if(c){var l\x3dM.getPooled(P.change,c,o);return E.accumulateTwoPhaseDispatches(l),l}}i\x26\x26i(e,t,r)}};t.exports\x3dA},{"./EventConstants":16,"./EventPluginHub":18,"./EventPropagators":21,"./ExecutionEnvironment":22,"./ReactUpdates":77,"./SyntheticEvent":85,"./isEventSupported":125,"./isTextInputElement":127,"./keyOf":131}],8:[function(e,t){"use strict";var n\x3d0,r\x3d{createReactRootIndex:function(){return n++}};t.exports\x3dr},{}],9:[function(e,t){"use strict";function n(e){switch(e){case g.topCompositionStart:return E.compositionStart;case g.topCompositionEnd:return E.compositionEnd;case g.topCompositionUpdate:return E.compositionUpdate}}function r(e,t){return e\x3d\x3d\x3dg.topKeyDown\x26\x26t.keyCode\x3d\x3d\x3dh}function o(e,t){switch(e){case g.topKeyUp:return-1!\x3d\x3df.indexOf(t.keyCode);case g.topKeyDown:return t.keyCode!\x3d\x3dh;case g.topKeyPress:case g.topMouseDown:case g.topBlur:return!0;default:return!1}}function a(e){this.root\x3de,this.startSelection\x3dc.getSelection(e),this.startValue\x3dthis.getText()}var i\x3de("./EventConstants"),s\x3de("./EventPropagators"),u\x3de("./ExecutionEnvironment"),c\x3de("./ReactInputSelection"),l\x3de("./SyntheticCompositionEvent"),p\x3de("./getTextContentAccessor"),d\x3de("./keyOf"),f\x3d[9,13,27,32],h\x3d229,m\x3du.canUseDOM\x26\x26"CompositionEvent"in window,v\x3d!m||"documentMode"in document\x26\x26document.documentMode\x3e8\x26\x26document.documentMode\x3c\x3d11,g\x3di.topLevelTypes,y\x3dnull,E\x3d{compositionEnd:{phasedRegistrationNames:{bubbled:d({onCompositionEnd:null}),captured:d({onCompositionEndCapture:null})},dependencies:[g.topBlur,g.topCompositionEnd,g.topKeyDown,g.topKeyPress,g.topKeyUp,g.topMouseDown]},compositionStart:{phasedRegistrationNames:{bubbled:d({onCompositionStart:null}),captured:d({onCompositionStartCapture:null})},dependencies:[g.topBlur,g.topCompositionStart,g.topKeyDown,g.topKeyPress,g.topKeyUp,g.topMouseDown]},compositionUpdate:{phasedRegistrationNames:{bubbled:d({onCompositionUpdate:null}),captured:d({onCompositionUpdateCapture:null})},dependencies:[g.topBlur,g.topCompositionUpdate,g.topKeyDown,g.topKeyPress,g.topKeyUp,g.topMouseDown]}};a.prototype.getText\x3dfunction(){return this.root.value||this.root[p()]},a.prototype.getData\x3dfunction(){var e\x3dthis.getText(),t\x3dthis.startSelection.start,n\x3dthis.startValue.length-this.startSelection.end;return e.substr(t,e.length-n-t)};var C\x3d{eventTypes:E,extractEvents:function(e,t,i,u){var c,p;if(m?c\x3dn(e):y?o(e,u)\x26\x26(c\x3dE.compositionEnd):r(e,u)\x26\x26(c\x3dE.compositionStart),v\x26\x26(y||c!\x3d\x3dE.compositionStart?c\x3d\x3d\x3dE.compositionEnd\x26\x26y\x26\x26(p\x3dy.getData(),y\x3dnull):y\x3dnew a(t)),c){var d\x3dl.getPooled(c,i,u);return p\x26\x26(d.data\x3dp),s.accumulateTwoPhaseDispatches(d),d}}};t.exports\x3dC},{"./EventConstants":16,"./EventPropagators":21,"./ExecutionEnvironment":22,"./ReactInputSelection":57,"./SyntheticCompositionEvent":83,"./getTextContentAccessor":119,"./keyOf":131}],10:[function(e,t){"use strict";function n(e,t,n){e.insertBefore(t,e.childNodes[n]||null)}var r,o\x3de("./Danger"),a\x3de("./ReactMultiChildUpdateTypes"),i\x3de("./getTextContentAccessor"),s\x3de("./invariant"),u\x3di();r\x3d"textContent"\x3d\x3d\x3du?function(e,t){e.textContent\x3dt}:function(e,t){for(;e.firstChild;)e.removeChild(e.firstChild);if(t){var n\x3de.ownerDocument||document;e.appendChild(n.createTextNode(t))}};var c\x3d{dangerouslyReplaceNodeWithMarkup:o.dangerouslyReplaceNodeWithMarkup,updateTextContent:r,processUpdates:function(e,t){for(var i,u\x3dnull,c\x3dnull,l\x3d0;i\x3de[l];l++)if(i.type\x3d\x3d\x3da.MOVE_EXISTING||i.type\x3d\x3d\x3da.REMOVE_NODE){var p\x3di.fromIndex,d\x3di.parentNode.childNodes[p],f\x3di.parentID;s(d),u\x3du||{},u[f]\x3du[f]||[],u[f][p]\x3dd,c\x3dc||[],c.push(d)}var h\x3do.dangerouslyRenderMarkup(t);if(c)for(var m\x3d0;m\x3cc.length;m++)c[m].parentNode.removeChild(c[m]);for(var v\x3d0;i\x3de[v];v++)switch(i.type){case a.INSERT_MARKUP:n(i.parentNode,h[i.markupIndex],i.toIndex);break;case a.MOVE_EXISTING:n(i.parentNode,u[i.parentID][i.fromIndex],i.toIndex);break;case a.TEXT_CONTENT:r(i.parentNode,i.textContent);break;case a.REMOVE_NODE:}}};t.exports\x3dc},{"./Danger":13,"./ReactMultiChildUpdateTypes":63,"./getTextContentAccessor":119,"./invariant":124}],11:[function(e,t){"use strict";function n(e,t){return(e\x26t)\x3d\x3d\x3dt}var r\x3de("./invariant"),o\x3d{MUST_USE_ATTRIBUTE:1,MUST_USE_PROPERTY:2,HAS_SIDE_EFFECTS:4,HAS_BOOLEAN_VALUE:8,HAS_NUMERIC_VALUE:16,HAS_POSITIVE_NUMERIC_VALUE:48,HAS_OVERLOADED_BOOLEAN_VALUE:64,injectDOMPropertyConfig:function(e){var t\x3de.Properties||{},a\x3de.DOMAttributeNames||{},s\x3de.DOMPropertyNames||{},u\x3de.DOMMutationMethods||{};e.isCustomAttribute\x26\x26i._isCustomAttributeFunctions.push(e.isCustomAttribute);for(var c in t){r(!i.isStandardName.hasOwnProperty(c)),i.isStandardName[c]\x3d!0;var l\x3dc.toLowerCase();if(i.getPossibleStandardName[l]\x3dc,a.hasOwnProperty(c)){var p\x3da[c];i.getPossibleStandardName[p]\x3dc,i.getAttributeName[c]\x3dp}else i.getAttributeName[c]\x3dl;i.getPropertyName[c]\x3ds.hasOwnProperty(c)?s[c]:c,i.getMutationMethod[c]\x3du.hasOwnProperty(c)?u[c]:null;var d\x3dt[c];i.mustUseAttribute[c]\x3dn(d,o.MUST_USE_ATTRIBUTE),i.mustUseProperty[c]\x3dn(d,o.MUST_USE_PROPERTY),i.hasSideEffects[c]\x3dn(d,o.HAS_SIDE_EFFECTS),i.hasBooleanValue[c]\x3dn(d,o.HAS_BOOLEAN_VALUE),i.hasNumericValue[c]\x3dn(d,o.HAS_NUMERIC_VALUE),i.hasPositiveNumericValue[c]\x3dn(d,o.HAS_POSITIVE_NUMERIC_VALUE),i.hasOverloadedBooleanValue[c]\x3dn(d,o.HAS_OVERLOADED_BOOLEAN_VALUE),r(!i.mustUseAttribute[c]||!i.mustUseProperty[c]),r(i.mustUseProperty[c]||!i.hasSideEffects[c]),r(!!i.hasBooleanValue[c]+!!i.hasNumericValue[c]+!!i.hasOverloadedBooleanValue[c]\x3c\x3d1)}}},a\x3d{},i\x3d{ID_ATTRIBUTE_NAME:"data-reactid",isStandardName:{},getPossibleStandardName:{},getAttributeName:{},getPropertyName:{},getMutationMethod:{},mustUseAttribute:{},mustUseProperty:{},hasSideEffects:{},hasBooleanValue:{},hasNumericValue:{},hasPositiveNumericValue:{},hasOverloadedBooleanValue:{},_isCustomAttributeFunctions:[],isCustomAttribute:function(e){for(var t\x3d0;t\x3ci._isCustomAttributeFunctions.length;t++){var n\x3di._isCustomAttributeFunctions[t];if(n(e))return!0}return!1},getDefaultValueForProperty:function(e,t){var n,r\x3da[e];return r||(a[e]\x3dr\x3d{}),t in r||(n\x3ddocument.createElement(e),r[t]\x3dn[t]),r[t]},injection:o};t.exports\x3di},{"./invariant":124}],12:[function(e,t){"use strict";function n(e,t){return null\x3d\x3dt||r.hasBooleanValue[e]\x26\x26!t||r.hasNumericValue[e]\x26\x26isNaN(t)||r.hasPositiveNumericValue[e]\x26\x261\x3et||r.hasOverloadedBooleanValue[e]\x26\x26t\x3d\x3d\x3d!1}var r\x3de("./DOMProperty"),o\x3de("./escapeTextForBrowser"),a\x3de("./memoizeStringOnly"),i\x3d(e("./warning"),a(function(e){return o(e)+\'\x3d"\'})),s\x3d{createMarkupForID:function(e){return i(r.ID_ATTRIBUTE_NAME)+o(e)+\'"\'},createMarkupForProperty:function(e,t){if(r.isStandardName.hasOwnProperty(e)\x26\x26r.isStandardName[e]){if(n(e,t))return"";var a\x3dr.getAttributeName[e];return r.hasBooleanValue[e]||r.hasOverloadedBooleanValue[e]\x26\x26t\x3d\x3d\x3d!0?o(a):i(a)+o(t)+\'"\'}return r.isCustomAttribute(e)?null\x3d\x3dt?"":i(e)+o(t)+\'"\':null},setValueForProperty:function(e,t,o){if(r.isStandardName.hasOwnProperty(t)\x26\x26r.isStandardName[t]){var a\x3dr.getMutationMethod[t];if(a)a(e,o);else if(n(t,o))this.deleteValueForProperty(e,t);else if(r.mustUseAttribute[t])e.setAttribute(r.getAttributeName[t],""+o);else{var i\x3dr.getPropertyName[t];r.hasSideEffects[t]\x26\x26""+e[i]\x3d\x3d""+o||(e[i]\x3do)}}else r.isCustomAttribute(t)\x26\x26(null\x3d\x3do?e.removeAttribute(t):e.setAttribute(t,""+o))},deleteValueForProperty:function(e,t){if(r.isStandardName.hasOwnProperty(t)\x26\x26r.isStandardName[t]){var n\x3dr.getMutationMethod[t];if(n)n(e,void 0);else if(r.mustUseAttribute[t])e.removeAttribute(r.getAttributeName[t]);else{var o\x3dr.getPropertyName[t],a\x3dr.getDefaultValueForProperty(e.nodeName,o);r.hasSideEffects[t]\x26\x26""+e[o]\x3d\x3d\x3da||(e[o]\x3da)}}else r.isCustomAttribute(t)\x26\x26e.removeAttribute(t)}};t.exports\x3ds},{"./DOMProperty":11,"./escapeTextForBrowser":107,"./memoizeStringOnly":133,"./warning":141}],13:[function(e,t){"use strict";function n(e){return e.substring(1,e.indexOf(" "))}var r\x3de("./ExecutionEnvironment"),o\x3de("./createNodesFromMarkup"),a\x3de("./emptyFunction"),i\x3de("./getMarkupWrap"),s\x3de("./invariant"),u\x3d/^(\x3c[^ \\/\x3e]+)/,c\x3d"data-danger-index",l\x3d{dangerouslyRenderMarkup:function(e){s(r.canUseDOM);for(var t,l\x3d{},p\x3d0;p\x3ce.length;p++)s(e[p]),t\x3dn(e[p]),t\x3di(t)?t:"*",l[t]\x3dl[t]||[],l[t][p]\x3de[p];var d\x3d[],f\x3d0;for(t in l)if(l.hasOwnProperty(t)){var h\x3dl[t];for(var m in h)if(h.hasOwnProperty(m)){var v\x3dh[m];h[m]\x3dv.replace(u,"$1 "+c+\'\x3d"\'+m+\'" \')}var g\x3do(h.join(""),a);for(p\x3d0;p\x3cg.length;++p){var y\x3dg[p];y.hasAttribute\x26\x26y.hasAttribute(c)\x26\x26(m\x3d+y.getAttribute(c),y.removeAttribute(c),s(!d.hasOwnProperty(m)),d[m]\x3dy,f+\x3d1)}}return s(f\x3d\x3d\x3dd.length),s(d.length\x3d\x3d\x3de.length),d},dangerouslyReplaceNodeWithMarkup:function(e,t){s(r.canUseDOM),s(t),s("html"!\x3d\x3de.tagName.toLowerCase());var n\x3do(t,a)[0];e.parentNode.replaceChild(n,e)}};t.exports\x3dl},{"./ExecutionEnvironment":22,"./createNodesFromMarkup":102,"./emptyFunction":105,"./getMarkupWrap":116,"./invariant":124}],14:[function(e,t){"use strict";var n\x3de("./keyOf"),r\x3d[n({ResponderEventPlugin:null}),n({SimpleEventPlugin:null}),n({TapEventPlugin:null}),n({EnterLeaveEventPlugin:null}),n({ChangeEventPlugin:null}),n({SelectEventPlugin:null}),n({CompositionEventPlugin:null}),n({BeforeInputEventPlugin:null}),n({AnalyticsEventPlugin:null}),n({MobileSafariClickEventPlugin:null})];t.exports\x3dr},{"./keyOf":131}],15:[function(e,t){"use strict";var n\x3de("./EventConstants"),r\x3de("./EventPropagators"),o\x3de("./SyntheticMouseEvent"),a\x3de("./ReactMount"),i\x3de("./keyOf"),s\x3dn.topLevelTypes,u\x3da.getFirstReactDOM,c\x3d{mouseEnter:{registrationName:i({onMouseEnter:null}),dependencies:[s.topMouseOut,s.topMouseOver]},mouseLeave:{registrationName:i({onMouseLeave:null}),dependencies:[s.topMouseOut,s.topMouseOver]}},l\x3d[null,null],p\x3d{eventTypes:c,extractEvents:function(e,t,n,i){if(e\x3d\x3d\x3ds.topMouseOver\x26\x26(i.relatedTarget||i.fromElement))return null;if(e!\x3d\x3ds.topMouseOut\x26\x26e!\x3d\x3ds.topMouseOver)return null;var p;if(t.window\x3d\x3d\x3dt)p\x3dt;else{var d\x3dt.ownerDocument;p\x3dd?d.defaultView||d.parentWindow:window}var f,h;if(e\x3d\x3d\x3ds.topMouseOut?(f\x3dt,h\x3du(i.relatedTarget||i.toElement)||p):(f\x3dp,h\x3dt),f\x3d\x3d\x3dh)return null;var m\x3df?a.getID(f):"",v\x3dh?a.getID(h):"",g\x3do.getPooled(c.mouseLeave,m,i);g.type\x3d"mouseleave",g.target\x3df,g.relatedTarget\x3dh;var y\x3do.getPooled(c.mouseEnter,v,i);return y.type\x3d"mouseenter",y.target\x3dh,y.relatedTarget\x3df,r.accumulateEnterLeaveDispatches(g,y,m,v),l[0]\x3dg,l[1]\x3dy,l}};t.exports\x3dp},{"./EventConstants":16,"./EventPropagators":21,"./ReactMount":61,"./SyntheticMouseEvent":89,"./keyOf":131}],16:[function(e,t){"use strict";var n\x3de("./keyMirror"),r\x3dn({bubbled:null,captured:null}),o\x3dn({topBlur:null,topChange:null,topClick:null,topCompositionEnd:null,topCompositionStart:null,topCompositionUpdate:null,topContextMenu:null,topCopy:null,topCut:null,topDoubleClick:null,topDrag:null,topDragEnd:null,topDragEnter:null,topDragExit:null,topDragLeave:null,topDragOver:null,topDragStart:null,topDrop:null,topError:null,topFocus:null,topInput:null,topKeyDown:null,topKeyPress:null,topKeyUp:null,topLoad:null,topMouseDown:null,topMouseMove:null,topMouseOut:null,topMouseOver:null,topMouseUp:null,topPaste:null,topReset:null,topScroll:null,topSelectionChange:null,topSubmit:null,topTextInput:null,topTouchCancel:null,topTouchEnd:null,topTouchMove:null,topTouchStart:null,topWheel:null}),a\x3d{topLevelTypes:o,PropagationPhases:r};t.exports\x3da},{"./keyMirror":130}],17:[function(e,t){var n\x3de("./emptyFunction"),r\x3d{listen:function(e,t,n){return e.addEventListener?(e.addEventListener(t,n,!1),{remove:function(){e.removeEventListener(t,n,!1)}}):e.attachEvent?(e.attachEvent("on"+t,n),{remove:function(){e.detachEvent("on"+t,n)}}):void 0},capture:function(e,t,r){return e.addEventListener?(e.addEventListener(t,r,!0),{remove:function(){e.removeEventListener(t,r,!0)}}):{remove:n}},registerDefault:function(){}};t.exports\x3dr},{"./emptyFunction":105}],18:[function(e,t){"use strict";var n\x3de("./EventPluginRegistry"),r\x3de("./EventPluginUtils"),o\x3de("./accumulateInto"),a\x3de("./forEachAccumulated"),i\x3de("./invariant"),s\x3d{},u\x3dnull,c\x3dfunction(e){if(e){var t\x3dr.executeDispatch,o\x3dn.getPluginModuleForEvent(e);o\x26\x26o.executeDispatch\x26\x26(t\x3do.executeDispatch),r.executeDispatchesInOrder(e,t),e.isPersistent()||e.constructor.release(e)}},l\x3dnull,p\x3d{injection:{injectMount:r.injection.injectMount,injectInstanceHandle:function(e){l\x3de},getInstanceHandle:function(){return l},injectEventPluginOrder:n.injectEventPluginOrder,injectEventPluginsByName:n.injectEventPluginsByName},eventNameDispatchConfigs:n.eventNameDispatchConfigs,registrationNameModules:n.registrationNameModules,putListener:function(e,t,n){i(!n||"function"\x3d\x3dtypeof n);var r\x3ds[t]||(s[t]\x3d{});r[e]\x3dn},getListener:function(e,t){var n\x3ds[t];return n\x26\x26n[e]},deleteListener:function(e,t){var n\x3ds[t];n\x26\x26delete n[e]},deleteAllListeners:function(e){for(var t in s)delete s[t][e]},extractEvents:function(e,t,r,a){for(var i,s\x3dn.plugins,u\x3d0,c\x3ds.length;c\x3eu;u++){var l\x3ds[u];if(l){var p\x3dl.extractEvents(e,t,r,a);p\x26\x26(i\x3do(i,p))}}return i},enqueueEvents:function(e){e\x26\x26(u\x3do(u,e))},processEventQueue:function(){var e\x3du;u\x3dnull,a(e,c),i(!u)},__purge:function(){s\x3d{}},__getListenerBank:function(){return s}};t.exports\x3dp},{"./EventPluginRegistry":19,"./EventPluginUtils":20,"./accumulateInto":95,"./forEachAccumulated":110,"./invariant":124}],19:[function(e,t){"use strict";function n(){if(i)for(var e in s){var t\x3ds[e],n\x3di.indexOf(e);if(a(n\x3e-1),!u.plugins[n]){a(t.extractEvents),u.plugins[n]\x3dt;var o\x3dt.eventTypes;for(var c in o)a(r(o[c],t,c))}}}function r(e,t,n){a(!u.eventNameDispatchConfigs.hasOwnProperty(n)),u.eventNameDispatchConfigs[n]\x3de;var r\x3de.phasedRegistrationNames;if(r){for(var i in r)if(r.hasOwnProperty(i)){var s\x3dr[i];o(s,t,n)}return!0}return e.registrationName?(o(e.registrationName,t,n),!0):!1}function o(e,t,n){a(!u.registrationNameModules[e]),u.registrationNameModules[e]\x3dt,u.registrationNameDependencies[e]\x3dt.eventTypes[n].dependencies}var a\x3de("./invariant"),i\x3dnull,s\x3d{},u\x3d{plugins:[],eventNameDispatchConfigs:{},registrationNameModules:{},registrationNameDependencies:{},injectEventPluginOrder:function(e){a(!i),i\x3dArray.prototype.slice.call(e),n()},injectEventPluginsByName:function(e){var t\x3d!1;for(var r in e)if(e.hasOwnProperty(r)){var o\x3de[r];s.hasOwnProperty(r)\x26\x26s[r]\x3d\x3d\x3do||(a(!s[r]),s[r]\x3do,t\x3d!0)}t\x26\x26n()},getPluginModuleForEvent:function(e){var t\x3de.dispatchConfig;if(t.registrationName)return u.registrationNameModules[t.registrationName]||null;for(var n in t.phasedRegistrationNames)if(t.phasedRegistrationNames.hasOwnProperty(n)){var r\x3du.registrationNameModules[t.phasedRegistrationNames[n]];if(r)return r}return null},_resetEventPlugins:function(){i\x3dnull;for(var e in s)s.hasOwnProperty(e)\x26\x26delete s[e];u.plugins.length\x3d0;var t\x3du.eventNameDispatchConfigs;for(var n in t)t.hasOwnProperty(n)\x26\x26delete t[n];var r\x3du.registrationNameModules;for(var o in r)r.hasOwnProperty(o)\x26\x26delete r[o]}};t.exports\x3du},{"./invariant":124}],20:[function(e,t){"use strict";function n(e){return e\x3d\x3d\x3dm.topMouseUp||e\x3d\x3d\x3dm.topTouchEnd||e\x3d\x3d\x3dm.topTouchCancel}function r(e){return e\x3d\x3d\x3dm.topMouseMove||e\x3d\x3d\x3dm.topTouchMove}function o(e){return e\x3d\x3d\x3dm.topMouseDown||e\x3d\x3d\x3dm.topTouchStart}function a(e,t){var n\x3de._dispatchListeners,r\x3de._dispatchIDs;if(Array.isArray(n))for(var o\x3d0;o\x3cn.length\x26\x26!e.isPropagationStopped();o++)t(e,n[o],r[o]);else n\x26\x26t(e,n,r)}function i(e,t,n){e.currentTarget\x3dh.Mount.getNode(n);var r\x3dt(e,n);return e.currentTarget\x3dnull,r}function s(e,t){a(e,t),e._dispatchListeners\x3dnull,e._dispatchIDs\x3dnull}function u(e){var t\x3de._dispatchListeners,n\x3de._dispatchIDs;if(Array.isArray(t)){for(var r\x3d0;r\x3ct.length\x26\x26!e.isPropagationStopped();r++)if(t[r](e,n[r]))return n[r]}else if(t\x26\x26t(e,n))return n;return null}function c(e){var t\x3du(e);return e._dispatchIDs\x3dnull,e._dispatchListeners\x3dnull,t}function l(e){var t\x3de._dispatchListeners,n\x3de._dispatchIDs;f(!Array.isArray(t));var r\x3dt?t(e,n):null;return e._dispatchListeners\x3dnull,e._dispatchIDs\x3dnull,r}function p(e){return!!e._dispatchListeners}var d\x3de("./EventConstants"),f\x3de("./invariant"),h\x3d{Mount:null,injectMount:function(e){h.Mount\x3de}},m\x3dd.topLevelTypes,v\x3d{isEndish:n,isMoveish:r,isStartish:o,executeDirectDispatch:l,executeDispatch:i,executeDispatchesInOrder:s,executeDispatchesInOrderStopAtTrue:c,hasDispatches:p,injection:h,useTouchEvents:!1};t.exports\x3dv},{"./EventConstants":16,"./invariant":124}],21:[function(e,t){"use strict";function n(e,t,n){var r\x3dt.dispatchConfig.phasedRegistrationNames[n];return m(e,r)}function r(e,t,r){var o\x3dt?h.bubbled:h.captured,a\x3dn(e,r,o);a\x26\x26(r._dispatchListeners\x3dd(r._dispatchListeners,a),r._dispatchIDs\x3dd(r._dispatchIDs,e))}function o(e){e\x26\x26e.dispatchConfig.phasedRegistrationNames\x26\x26p.injection.getInstanceHandle().traverseTwoPhase(e.dispatchMarker,r,e)}function a(e,t,n){if(n\x26\x26n.dispatchConfig.registrationName){var r\x3dn.dispatchConfig.registrationName,o\x3dm(e,r);o\x26\x26(n._dispatchListeners\x3dd(n._dispatchListeners,o),n._dispatchIDs\x3dd(n._dispatchIDs,e))}}function i(e){e\x26\x26e.dispatchConfig.registrationName\x26\x26a(e.dispatchMarker,null,e)}function s(e){f(e,o)}function u(e,t,n,r){p.injection.getInstanceHandle().traverseEnterLeave(n,r,a,e,t)}function c(e){f(e,i)}var l\x3de("./EventConstants"),p\x3de("./EventPluginHub"),d\x3de("./accumulateInto"),f\x3de("./forEachAccumulated"),h\x3dl.PropagationPhases,m\x3dp.getListener,v\x3d{accumulateTwoPhaseDispatches:s,accumulateDirectDispatches:c,accumulateEnterLeaveDispatches:u};t.exports\x3dv},{"./EventConstants":16,"./EventPluginHub":18,"./accumulateInto":95,"./forEachAccumulated":110}],22:[function(e,t){"use strict";var n\x3d!("undefined"\x3d\x3dtypeof window||!window.document||!window.document.createElement),r\x3d{canUseDOM:n,canUseWorkers:"undefined"!\x3dtypeof Worker,canUseEventListeners:n\x26\x26!(!window.addEventListener\x26\x26!window.attachEvent),canUseViewport:n\x26\x26!!window.screen,isInWorker:!n};t.exports\x3dr},{}],23:[function(e,t){"use strict";var n,r\x3de("./DOMProperty"),o\x3de("./ExecutionEnvironment"),a\x3dr.injection.MUST_USE_ATTRIBUTE,i\x3dr.injection.MUST_USE_PROPERTY,s\x3dr.injection.HAS_BOOLEAN_VALUE,u\x3dr.injection.HAS_SIDE_EFFECTS,c\x3dr.injection.HAS_NUMERIC_VALUE,l\x3dr.injection.HAS_POSITIVE_NUMERIC_VALUE,p\x3dr.injection.HAS_OVERLOADED_BOOLEAN_VALUE;if(o.canUseDOM){var d\x3ddocument.implementation;n\x3dd\x26\x26d.hasFeature\x26\x26d.hasFeature("http://www.w3.org/TR/SVG11/feature#BasicStructure","1.1")}var f\x3d{isCustomAttribute:RegExp.prototype.test.bind(/^(data|aria)-[a-z_][a-z\\d_.\\-]*$/),Properties:{accept:null,acceptCharset:null,accessKey:null,action:null,allowFullScreen:a|s,allowTransparency:a,alt:null,async:s,autoComplete:null,autoPlay:s,cellPadding:null,cellSpacing:null,charSet:a,checked:i|s,classID:a,className:n?a:i,cols:a|l,colSpan:null,content:null,contentEditable:null,contextMenu:a,controls:i|s,coords:null,crossOrigin:null,data:null,dateTime:a,defer:s,dir:null,disabled:a|s,download:p,draggable:null,encType:null,form:a,formNoValidate:s,frameBorder:a,height:a,hidden:a|s,href:null,hrefLang:null,htmlFor:null,httpEquiv:null,icon:null,id:i,label:null,lang:null,list:a,loop:i|s,manifest:a,max:null,maxLength:a,media:a,mediaGroup:null,method:null,min:null,multiple:i|s,muted:i|s,name:null,noValidate:s,open:null,pattern:null,placeholder:null,poster:null,preload:null,radioGroup:null,readOnly:i|s,rel:null,required:s,role:a,rows:a|l,rowSpan:null,sandbox:null,scope:null,scrolling:null,seamless:a|s,selected:i|s,shape:null,size:a|l,sizes:a,span:l,spellCheck:null,src:null,srcDoc:i,srcSet:a,start:c,step:null,style:null,tabIndex:null,target:null,title:null,type:null,useMap:null,value:i|u,width:a,wmode:a,autoCapitalize:null,autoCorrect:null,itemProp:a,itemScope:a|s,itemType:a,property:null},DOMAttributeNames:{acceptCharset:"accept-charset",className:"class",htmlFor:"for",httpEquiv:"http-equiv"},DOMPropertyNames:{autoCapitalize:"autocapitalize",autoComplete:"autocomplete",autoCorrect:"autocorrect",autoFocus:"autofocus",autoPlay:"autoplay",encType:"enctype",hrefLang:"hreflang",radioGroup:"radiogroup",spellCheck:"spellcheck",srcDoc:"srcdoc",srcSet:"srcset"}};t.exports\x3df},{"./DOMProperty":11,"./ExecutionEnvironment":22}],24:[function(e,t){"use strict";function n(e){u(null\x3d\x3de.props.checkedLink||null\x3d\x3de.props.valueLink)}function r(e){n(e),u(null\x3d\x3de.props.value\x26\x26null\x3d\x3de.props.onChange)}function o(e){n(e),u(null\x3d\x3de.props.checked\x26\x26null\x3d\x3de.props.onChange)}function a(e){this.props.valueLink.requestChange(e.target.value)}function i(e){this.props.checkedLink.requestChange(e.target.checked)}var s\x3de("./ReactPropTypes"),u\x3de("./invariant"),c\x3d{button:!0,checkbox:!0,image:!0,hidden:!0,radio:!0,reset:!0,submit:!0},l\x3d{Mixin:{propTypes:{value:function(e,t){return!e[t]||c[e.type]||e.onChange||e.readOnly||e.disabled?void 0:new Error("You provided a `value` prop to a form field without an `onChange` handler. This will render a read-only field. If the field should be mutable use `defaultValue`. Otherwise, set either `onChange` or `readOnly`.")},checked:function(e,t){return!e[t]||e.onChange||e.readOnly||e.disabled?void 0:new Error("You provided a `checked` prop to a form field without an `onChange` handler. This will render a read-only field. If the field should be mutable use `defaultChecked`. Otherwise, set either `onChange` or `readOnly`.")},onChange:s.func}},getValue:function(e){return e.props.valueLink?(r(e),e.props.valueLink.value):e.props.value},getChecked:function(e){return e.props.checkedLink?(o(e),e.props.checkedLink.value):e.props.checked},getOnChange:function(e){return e.props.valueLink?(r(e),a):e.props.checkedLink?(o(e),i):e.props.onChange}};t.exports\x3dl},{"./ReactPropTypes":70,"./invariant":124}],25:[function(e,t){"use strict";function n(e){e.remove()}var r\x3de("./ReactBrowserEventEmitter"),o\x3de("./accumulateInto"),a\x3de("./forEachAccumulated"),i\x3de("./invariant"),s\x3d{trapBubbledEvent:function(e,t){i(this.isMounted());var n\x3dr.trapBubbledEvent(e,t,this.getDOMNode());this._localEventListeners\x3do(this._localEventListeners,n)},componentWillUnmount:function(){this._localEventListeners\x26\x26a(this._localEventListeners,n)}};t.exports\x3ds},{"./ReactBrowserEventEmitter":30,"./accumulateInto":95,"./forEachAccumulated":110,"./invariant":124}],26:[function(e,t){"use strict";var n\x3de("./EventConstants"),r\x3de("./emptyFunction"),o\x3dn.topLevelTypes,a\x3d{eventTypes:null,extractEvents:function(e,t,n,a){if(e\x3d\x3d\x3do.topTouchStart){var i\x3da.target;i\x26\x26!i.onclick\x26\x26(i.onclick\x3dr)}}};t.exports\x3da},{"./EventConstants":16,"./emptyFunction":105}],27:[function(e,t){function n(e){if(null\x3d\x3de)throw new TypeError("Object.assign target cannot be null or undefined");for(var t\x3dObject(e),n\x3dObject.prototype.hasOwnProperty,r\x3d1;r\x3carguments.length;r++){var o\x3darguments[r];if(null!\x3do){var a\x3dObject(o);for(var i in a)n.call(a,i)\x26\x26(t[i]\x3da[i])}}return t}t.exports\x3dn},{}],28:[function(e,t){"use strict";var n\x3de("./invariant"),r\x3dfunction(e){var t\x3dthis;if(t.instancePool.length){var n\x3dt.instancePool.pop();return t.call(n,e),n}return new t(e)},o\x3dfunction(e,t){var n\x3dthis;if(n.instancePool.length){var r\x3dn.instancePool.pop();return n.call(r,e,t),r}return new n(e,t)},a\x3dfunction(e,t,n){var r\x3dthis;if(r.instancePool.length){var o\x3dr.instancePool.pop();return r.call(o,e,t,n),o}return new r(e,t,n)\n},i\x3dfunction(e,t,n,r,o){var a\x3dthis;if(a.instancePool.length){var i\x3da.instancePool.pop();return a.call(i,e,t,n,r,o),i}return new a(e,t,n,r,o)},s\x3dfunction(e){var t\x3dthis;n(e instanceof t),e.destructor\x26\x26e.destructor(),t.instancePool.length\x3ct.poolSize\x26\x26t.instancePool.push(e)},u\x3d10,c\x3dr,l\x3dfunction(e,t){var n\x3de;return n.instancePool\x3d[],n.getPooled\x3dt||c,n.poolSize||(n.poolSize\x3du),n.release\x3ds,n},p\x3d{addPoolingTo:l,oneArgumentPooler:r,twoArgumentPooler:o,threeArgumentPooler:a,fiveArgumentPooler:i};t.exports\x3dp},{"./invariant":124}],29:[function(e,t){"use strict";var n\x3de("./ReactEmptyComponent"),r\x3de("./ReactMount"),o\x3de("./invariant"),a\x3d{getDOMNode:function(){return o(this.isMounted()),n.isNullComponentID(this._rootNodeID)?null:r.getNode(this._rootNodeID)}};t.exports\x3da},{"./ReactEmptyComponent":52,"./ReactMount":61,"./invariant":124}],30:[function(e,t){"use strict";function n(e){return Object.prototype.hasOwnProperty.call(e,h)||(e[h]\x3dd++,l[e[h]]\x3d{}),l[e[h]]}var r\x3de("./EventConstants"),o\x3de("./EventPluginHub"),a\x3de("./EventPluginRegistry"),i\x3de("./ReactEventEmitterMixin"),s\x3de("./ViewportMetrics"),u\x3de("./Object.assign"),c\x3de("./isEventSupported"),l\x3d{},p\x3d!1,d\x3d0,f\x3d{topBlur:"blur",topChange:"change",topClick:"click",topCompositionEnd:"compositionend",topCompositionStart:"compositionstart",topCompositionUpdate:"compositionupdate",topContextMenu:"contextmenu",topCopy:"copy",topCut:"cut",topDoubleClick:"dblclick",topDrag:"drag",topDragEnd:"dragend",topDragEnter:"dragenter",topDragExit:"dragexit",topDragLeave:"dragleave",topDragOver:"dragover",topDragStart:"dragstart",topDrop:"drop",topFocus:"focus",topInput:"input",topKeyDown:"keydown",topKeyPress:"keypress",topKeyUp:"keyup",topMouseDown:"mousedown",topMouseMove:"mousemove",topMouseOut:"mouseout",topMouseOver:"mouseover",topMouseUp:"mouseup",topPaste:"paste",topScroll:"scroll",topSelectionChange:"selectionchange",topTextInput:"textInput",topTouchCancel:"touchcancel",topTouchEnd:"touchend",topTouchMove:"touchmove",topTouchStart:"touchstart",topWheel:"wheel"},h\x3d"_reactListenersID"+String(Math.random()).slice(2),m\x3du({},i,{ReactEventListener:null,injection:{injectReactEventListener:function(e){e.setHandleTopLevel(m.handleTopLevel),m.ReactEventListener\x3de}},setEnabled:function(e){m.ReactEventListener\x26\x26m.ReactEventListener.setEnabled(e)},isEnabled:function(){return!(!m.ReactEventListener||!m.ReactEventListener.isEnabled())},listenTo:function(e,t){for(var o\x3dt,i\x3dn(o),s\x3da.registrationNameDependencies[e],u\x3dr.topLevelTypes,l\x3d0,p\x3ds.length;p\x3el;l++){var d\x3ds[l];i.hasOwnProperty(d)\x26\x26i[d]||(d\x3d\x3d\x3du.topWheel?c("wheel")?m.ReactEventListener.trapBubbledEvent(u.topWheel,"wheel",o):c("mousewheel")?m.ReactEventListener.trapBubbledEvent(u.topWheel,"mousewheel",o):m.ReactEventListener.trapBubbledEvent(u.topWheel,"DOMMouseScroll",o):d\x3d\x3d\x3du.topScroll?c("scroll",!0)?m.ReactEventListener.trapCapturedEvent(u.topScroll,"scroll",o):m.ReactEventListener.trapBubbledEvent(u.topScroll,"scroll",m.ReactEventListener.WINDOW_HANDLE):d\x3d\x3d\x3du.topFocus||d\x3d\x3d\x3du.topBlur?(c("focus",!0)?(m.ReactEventListener.trapCapturedEvent(u.topFocus,"focus",o),m.ReactEventListener.trapCapturedEvent(u.topBlur,"blur",o)):c("focusin")\x26\x26(m.ReactEventListener.trapBubbledEvent(u.topFocus,"focusin",o),m.ReactEventListener.trapBubbledEvent(u.topBlur,"focusout",o)),i[u.topBlur]\x3d!0,i[u.topFocus]\x3d!0):f.hasOwnProperty(d)\x26\x26m.ReactEventListener.trapBubbledEvent(d,f[d],o),i[d]\x3d!0)}},trapBubbledEvent:function(e,t,n){return m.ReactEventListener.trapBubbledEvent(e,t,n)},trapCapturedEvent:function(e,t,n){return m.ReactEventListener.trapCapturedEvent(e,t,n)},ensureScrollValueMonitoring:function(){if(!p){var e\x3ds.refreshScrollValues;m.ReactEventListener.monitorScrollValue(e),p\x3d!0}},eventNameDispatchConfigs:o.eventNameDispatchConfigs,registrationNameModules:o.registrationNameModules,putListener:o.putListener,getListener:o.getListener,deleteListener:o.deleteListener,deleteAllListeners:o.deleteAllListeners});t.exports\x3dm},{"./EventConstants":16,"./EventPluginHub":18,"./EventPluginRegistry":19,"./Object.assign":27,"./ReactEventEmitterMixin":54,"./ViewportMetrics":94,"./isEventSupported":125}],31:[function(e,t){"use strict";function n(e,t){this.forEachFunction\x3de,this.forEachContext\x3dt}function r(e,t,n,r){var o\x3de;o.forEachFunction.call(o.forEachContext,t,r)}function o(e,t,o){if(null\x3d\x3de)return e;var a\x3dn.getPooled(t,o);p(e,r,a),n.release(a)}function a(e,t,n){this.mapResult\x3de,this.mapFunction\x3dt,this.mapContext\x3dn}function i(e,t,n,r){var o\x3de,a\x3do.mapResult,i\x3d!a.hasOwnProperty(n);if(i){var s\x3do.mapFunction.call(o.mapContext,t,r);a[n]\x3ds}}function s(e,t,n){if(null\x3d\x3de)return e;var r\x3d{},o\x3da.getPooled(r,t,n);return p(e,i,o),a.release(o),r}function u(){return null}function c(e){return p(e,u,null)}var l\x3de("./PooledClass"),p\x3de("./traverseAllChildren"),d\x3d(e("./warning"),l.twoArgumentPooler),f\x3dl.threeArgumentPooler;l.addPoolingTo(n,d),l.addPoolingTo(a,f);var h\x3d{forEach:o,map:s,count:c};t.exports\x3dh},{"./PooledClass":28,"./traverseAllChildren":140,"./warning":141}],32:[function(e,t){"use strict";var n\x3de("./ReactElement"),r\x3de("./ReactOwner"),o\x3de("./ReactUpdates"),a\x3de("./Object.assign"),i\x3de("./invariant"),s\x3de("./keyMirror"),u\x3ds({MOUNTED:null,UNMOUNTED:null}),c\x3d!1,l\x3dnull,p\x3dnull,d\x3d{injection:{injectEnvironment:function(e){i(!c),p\x3de.mountImageIntoNode,l\x3de.unmountIDFromEnvironment,d.BackendIDOperations\x3de.BackendIDOperations,c\x3d!0}},LifeCycle:u,BackendIDOperations:null,Mixin:{isMounted:function(){return this._lifeCycleState\x3d\x3d\x3du.MOUNTED},setProps:function(e,t){var n\x3dthis._pendingElement||this._currentElement;this.replaceProps(a({},n.props,e),t)},replaceProps:function(e,t){i(this.isMounted()),i(0\x3d\x3d\x3dthis._mountDepth),this._pendingElement\x3dn.cloneAndReplaceProps(this._pendingElement||this._currentElement,e),o.enqueueUpdate(this,t)},_setPropsInternal:function(e,t){var r\x3dthis._pendingElement||this._currentElement;this._pendingElement\x3dn.cloneAndReplaceProps(r,a({},r.props,e)),o.enqueueUpdate(this,t)},construct:function(e){this.props\x3de.props,this._owner\x3de._owner,this._lifeCycleState\x3du.UNMOUNTED,this._pendingCallbacks\x3dnull,this._currentElement\x3de,this._pendingElement\x3dnull},mountComponent:function(e,t,n){i(!this.isMounted());var o\x3dthis._currentElement.ref;if(null!\x3do){var a\x3dthis._currentElement._owner;r.addComponentAsRefTo(this,o,a)}this._rootNodeID\x3de,this._lifeCycleState\x3du.MOUNTED,this._mountDepth\x3dn},unmountComponent:function(){i(this.isMounted());var e\x3dthis._currentElement.ref;null!\x3de\x26\x26r.removeComponentAsRefFrom(this,e,this._owner),l(this._rootNodeID),this._rootNodeID\x3dnull,this._lifeCycleState\x3du.UNMOUNTED},receiveComponent:function(e,t){i(this.isMounted()),this._pendingElement\x3de,this.performUpdateIfNecessary(t)},performUpdateIfNecessary:function(e){if(null!\x3dthis._pendingElement){var t\x3dthis._currentElement,n\x3dthis._pendingElement;this._currentElement\x3dn,this.props\x3dn.props,this._owner\x3dn._owner,this._pendingElement\x3dnull,this.updateComponent(e,t)}},updateComponent:function(e,t){var n\x3dthis._currentElement;(n._owner!\x3d\x3dt._owner||n.ref!\x3d\x3dt.ref)\x26\x26(null!\x3dt.ref\x26\x26r.removeComponentAsRefFrom(this,t.ref,t._owner),null!\x3dn.ref\x26\x26r.addComponentAsRefTo(this,n.ref,n._owner))},mountComponentIntoNode:function(e,t,n){var r\x3do.ReactReconcileTransaction.getPooled();r.perform(this._mountComponentIntoNode,this,e,t,r,n),o.ReactReconcileTransaction.release(r)},_mountComponentIntoNode:function(e,t,n,r){var o\x3dthis.mountComponent(e,n,0);p(o,t,r)},isOwnedBy:function(e){return this._owner\x3d\x3d\x3de},getSiblingByRef:function(e){var t\x3dthis._owner;return t\x26\x26t.refs?t.refs[e]:null}}};t.exports\x3dd},{"./Object.assign":27,"./ReactElement":50,"./ReactOwner":65,"./ReactUpdates":77,"./invariant":124,"./keyMirror":130}],33:[function(e,t){"use strict";var n\x3de("./ReactDOMIDOperations"),r\x3de("./ReactMarkupChecksum"),o\x3de("./ReactMount"),a\x3de("./ReactPerf"),i\x3de("./ReactReconcileTransaction"),s\x3de("./getReactRootElementInContainer"),u\x3de("./invariant"),c\x3de("./setInnerHTML"),l\x3d1,p\x3d9,d\x3d{ReactReconcileTransaction:i,BackendIDOperations:n,unmountIDFromEnvironment:function(e){o.purgeID(e)},mountImageIntoNode:a.measure("ReactComponentBrowserEnvironment","mountImageIntoNode",function(e,t,n){if(u(t\x26\x26(t.nodeType\x3d\x3d\x3dl||t.nodeType\x3d\x3d\x3dp)),n){if(r.canReuseMarkup(e,s(t)))return;u(t.nodeType!\x3d\x3dp)}u(t.nodeType!\x3d\x3dp),c(t,e)})};t.exports\x3dd},{"./ReactDOMIDOperations":41,"./ReactMarkupChecksum":60,"./ReactMount":61,"./ReactPerf":66,"./ReactReconcileTransaction":72,"./getReactRootElementInContainer":118,"./invariant":124,"./setInnerHTML":136}],34:[function(e,t){"use strict";function n(e){var t\x3de._owner||null;return t\x26\x26t.constructor\x26\x26t.constructor.displayName?" Check the render method of `"+t.constructor.displayName+"`.":""}function r(e,t){for(var n in t)t.hasOwnProperty(n)\x26\x26D("function"\x3d\x3dtypeof t[n])}function o(e,t){var n\x3dS.hasOwnProperty(t)?S[t]:null;L.hasOwnProperty(t)\x26\x26D(n\x3d\x3d\x3dN.OVERRIDE_BASE),e.hasOwnProperty(t)\x26\x26D(n\x3d\x3d\x3dN.DEFINE_MANY||n\x3d\x3d\x3dN.DEFINE_MANY_MERGED)}function a(e){var t\x3de._compositeLifeCycleState;D(e.isMounted()||t\x3d\x3d\x3dA.MOUNTING),D(null\x3d\x3df.current),D(t!\x3d\x3dA.UNMOUNTING)}function i(e,t){if(t){D(!g.isValidFactory(t)),D(!h.isValidElement(t));var n\x3de.prototype;t.hasOwnProperty(T)\x26\x26k.mixins(e,t.mixins);for(var r in t)if(t.hasOwnProperty(r)\x26\x26r!\x3d\x3dT){var a\x3dt[r];if(o(n,r),k.hasOwnProperty(r))k[r](e,a);else{var i\x3dS.hasOwnProperty(r),s\x3dn.hasOwnProperty(r),u\x3da\x26\x26a.__reactDontBind,p\x3d"function"\x3d\x3dtypeof a,d\x3dp\x26\x26!i\x26\x26!s\x26\x26!u;if(d)n.__reactAutoBindMap||(n.__reactAutoBindMap\x3d{}),n.__reactAutoBindMap[r]\x3da,n[r]\x3da;else if(s){var f\x3dS[r];D(i\x26\x26(f\x3d\x3d\x3dN.DEFINE_MANY_MERGED||f\x3d\x3d\x3dN.DEFINE_MANY)),f\x3d\x3d\x3dN.DEFINE_MANY_MERGED?n[r]\x3dc(n[r],a):f\x3d\x3d\x3dN.DEFINE_MANY\x26\x26(n[r]\x3dl(n[r],a))}else n[r]\x3da}}}}function s(e,t){if(t)for(var n in t){var r\x3dt[n];if(t.hasOwnProperty(n)){var o\x3dn in k;D(!o);var a\x3dn in e;D(!a),e[n]\x3dr}}}function u(e,t){return D(e\x26\x26t\x26\x26"object"\x3d\x3dtypeof e\x26\x26"object"\x3d\x3dtypeof t),_(t,function(t,n){D(void 0\x3d\x3d\x3de[n]),e[n]\x3dt}),e}function c(e,t){return function(){var n\x3de.apply(this,arguments),r\x3dt.apply(this,arguments);return null\x3d\x3dn?r:null\x3d\x3dr?n:u(n,r)}}function l(e,t){return function(){e.apply(this,arguments),t.apply(this,arguments)}}var p\x3de("./ReactComponent"),d\x3de("./ReactContext"),f\x3de("./ReactCurrentOwner"),h\x3de("./ReactElement"),m\x3d(e("./ReactElementValidator"),e("./ReactEmptyComponent")),v\x3de("./ReactErrorUtils"),g\x3de("./ReactLegacyElement"),y\x3de("./ReactOwner"),E\x3de("./ReactPerf"),C\x3de("./ReactPropTransferer"),R\x3de("./ReactPropTypeLocations"),M\x3d(e("./ReactPropTypeLocationNames"),e("./ReactUpdates")),b\x3de("./Object.assign"),O\x3de("./instantiateReactComponent"),D\x3de("./invariant"),x\x3de("./keyMirror"),P\x3de("./keyOf"),_\x3d(e("./monitorCodeUse"),e("./mapObject")),w\x3de("./shouldUpdateReactComponent"),T\x3d(e("./warning"),P({mixins:null})),N\x3dx({DEFINE_ONCE:null,DEFINE_MANY:null,OVERRIDE_BASE:null,DEFINE_MANY_MERGED:null}),I\x3d[],S\x3d{mixins:N.DEFINE_MANY,statics:N.DEFINE_MANY,propTypes:N.DEFINE_MANY,contextTypes:N.DEFINE_MANY,childContextTypes:N.DEFINE_MANY,getDefaultProps:N.DEFINE_MANY_MERGED,getInitialState:N.DEFINE_MANY_MERGED,getChildContext:N.DEFINE_MANY_MERGED,render:N.DEFINE_ONCE,componentWillMount:N.DEFINE_MANY,componentDidMount:N.DEFINE_MANY,componentWillReceiveProps:N.DEFINE_MANY,shouldComponentUpdate:N.DEFINE_ONCE,componentWillUpdate:N.DEFINE_MANY,componentDidUpdate:N.DEFINE_MANY,componentWillUnmount:N.DEFINE_MANY,updateComponent:N.OVERRIDE_BASE},k\x3d{displayName:function(e,t){e.displayName\x3dt},mixins:function(e,t){if(t)for(var n\x3d0;n\x3ct.length;n++)i(e,t[n])},childContextTypes:function(e,t){r(e,t,R.childContext),e.childContextTypes\x3db({},e.childContextTypes,t)},contextTypes:function(e,t){r(e,t,R.context),e.contextTypes\x3db({},e.contextTypes,t)},getDefaultProps:function(e,t){e.getDefaultProps\x3de.getDefaultProps?c(e.getDefaultProps,t):t},propTypes:function(e,t){r(e,t,R.prop),e.propTypes\x3db({},e.propTypes,t)},statics:function(e,t){s(e,t)}},A\x3dx({MOUNTING:null,UNMOUNTING:null,RECEIVING_PROPS:null}),L\x3d{construct:function(){p.Mixin.construct.apply(this,arguments),y.Mixin.construct.apply(this,arguments),this.state\x3dnull,this._pendingState\x3dnull,this.context\x3dnull,this._compositeLifeCycleState\x3dnull},isMounted:function(){return p.Mixin.isMounted.call(this)\x26\x26this._compositeLifeCycleState!\x3d\x3dA.MOUNTING},mountComponent:E.measure("ReactCompositeComponent","mountComponent",function(e,t,n){p.Mixin.mountComponent.call(this,e,t,n),this._compositeLifeCycleState\x3dA.MOUNTING,this.__reactAutoBindMap\x26\x26this._bindAutoBindMethods(),this.context\x3dthis._processContext(this._currentElement._context),this.props\x3dthis._processProps(this.props),this.state\x3dthis.getInitialState?this.getInitialState():null,D("object"\x3d\x3dtypeof this.state\x26\x26!Array.isArray(this.state)),this._pendingState\x3dnull,this._pendingForceUpdate\x3d!1,this.componentWillMount\x26\x26(this.componentWillMount(),this._pendingState\x26\x26(this.state\x3dthis._pendingState,this._pendingState\x3dnull)),this._renderedComponent\x3dO(this._renderValidatedComponent(),this._currentElement.type),this._compositeLifeCycleState\x3dnull;var r\x3dthis._renderedComponent.mountComponent(e,t,n+1);return this.componentDidMount\x26\x26t.getReactMountReady().enqueue(this.componentDidMount,this),r}),unmountComponent:function(){this._compositeLifeCycleState\x3dA.UNMOUNTING,this.componentWillUnmount\x26\x26this.componentWillUnmount(),this._compositeLifeCycleState\x3dnull,this._renderedComponent.unmountComponent(),this._renderedComponent\x3dnull,p.Mixin.unmountComponent.call(this)},setState:function(e,t){D("object"\x3d\x3dtypeof e||null\x3d\x3de),this.replaceState(b({},this._pendingState||this.state,e),t)},replaceState:function(e,t){a(this),this._pendingState\x3de,this._compositeLifeCycleState!\x3d\x3dA.MOUNTING\x26\x26M.enqueueUpdate(this,t)},_processContext:function(e){var t\x3dnull,n\x3dthis.constructor.contextTypes;if(n){t\x3d{};for(var r in n)t[r]\x3de[r]}return t},_processChildContext:function(e){var t\x3dthis.getChildContext\x26\x26this.getChildContext();if(this.constructor.displayName||"ReactCompositeComponent",t){D("object"\x3d\x3dtypeof this.constructor.childContextTypes);for(var n in t)D(n in this.constructor.childContextTypes);return b({},e,t)}return e},_processProps:function(e){return e},_checkPropTypes:function(e,t,r){var o\x3dthis.constructor.displayName;for(var a in e)if(e.hasOwnProperty(a)){var i\x3de[a](t,a,o,r);i instanceof Error\x26\x26n(this)}},performUpdateIfNecessary:function(e){var t\x3dthis._compositeLifeCycleState;if(t!\x3d\x3dA.MOUNTING\x26\x26t!\x3d\x3dA.RECEIVING_PROPS\x26\x26(null!\x3dthis._pendingElement||null!\x3dthis._pendingState||this._pendingForceUpdate)){var n\x3dthis.context,r\x3dthis.props,o\x3dthis._currentElement;null!\x3dthis._pendingElement\x26\x26(o\x3dthis._pendingElement,n\x3dthis._processContext(o._context),r\x3dthis._processProps(o.props),this._pendingElement\x3dnull,this._compositeLifeCycleState\x3dA.RECEIVING_PROPS,this.componentWillReceiveProps\x26\x26this.componentWillReceiveProps(r,n)),this._compositeLifeCycleState\x3dnull;var a\x3dthis._pendingState||this.state;this._pendingState\x3dnull;var i\x3dthis._pendingForceUpdate||!this.shouldComponentUpdate||this.shouldComponentUpdate(r,a,n);i?(this._pendingForceUpdate\x3d!1,this._performComponentUpdate(o,r,a,n,e)):(this._currentElement\x3do,this.props\x3dr,this.state\x3da,this.context\x3dn,this._owner\x3do._owner)}},_performComponentUpdate:function(e,t,n,r,o){var a\x3dthis._currentElement,i\x3dthis.props,s\x3dthis.state,u\x3dthis.context;this.componentWillUpdate\x26\x26this.componentWillUpdate(t,n,r),this._currentElement\x3de,this.props\x3dt,this.state\x3dn,this.context\x3dr,this._owner\x3de._owner,this.updateComponent(o,a),this.componentDidUpdate\x26\x26o.getReactMountReady().enqueue(this.componentDidUpdate.bind(this,i,s,u),this)},receiveComponent:function(e,t){(e!\x3d\x3dthis._currentElement||null\x3d\x3de._owner)\x26\x26p.Mixin.receiveComponent.call(this,e,t)},updateComponent:E.measure("ReactCompositeComponent","updateComponent",function(e,t){p.Mixin.updateComponent.call(this,e,t);var n\x3dthis._renderedComponent,r\x3dn._currentElement,o\x3dthis._renderValidatedComponent();if(w(r,o))n.receiveComponent(o,e);else{var a\x3dthis._rootNodeID,i\x3dn._rootNodeID;n.unmountComponent(),this._renderedComponent\x3dO(o,this._currentElement.type);var s\x3dthis._renderedComponent.mountComponent(a,e,this._mountDepth+1);p.BackendIDOperations.dangerouslyReplaceNodeWithMarkupByID(i,s)}}),forceUpdate:function(e){var t\x3dthis._compositeLifeCycleState;D(this.isMounted()||t\x3d\x3d\x3dA.MOUNTING),D(t!\x3d\x3dA.UNMOUNTING\x26\x26null\x3d\x3df.current),this._pendingForceUpdate\x3d!0,M.enqueueUpdate(this,e)},_renderValidatedComponent:E.measure("ReactCompositeComponent","_renderValidatedComponent",function(){var e,t\x3dd.current;d.current\x3dthis._processChildContext(this._currentElement._context),f.current\x3dthis;try{e\x3dthis.render(),null\x3d\x3d\x3de||e\x3d\x3d\x3d!1?(e\x3dm.getEmptyComponent(),m.registerNullComponentID(this._rootNodeID)):m.deregisterNullComponentID(this._rootNodeID)}finally{d.current\x3dt,f.current\x3dnull}return D(h.isValidElement(e)),e}),_bindAutoBindMethods:function(){for(var e in this.__reactAutoBindMap)if(this.__reactAutoBindMap.hasOwnProperty(e)){var t\x3dthis.__reactAutoBindMap[e];this[e]\x3dthis._bindAutoBindMethod(v.guard(t,this.constructor.displayName+"."+e))}},_bindAutoBindMethod:function(e){var t\x3dthis,n\x3de.bind(t);return n}},U\x3dfunction(){};b(U.prototype,p.Mixin,y.Mixin,C.Mixin,L);var F\x3d{LifeCycle:A,Base:U,createClass:function(e){var t\x3dfunction(){};t.prototype\x3dnew U,t.prototype.constructor\x3dt,I.forEach(i.bind(null,t)),i(t,e),t.getDefaultProps\x26\x26(t.defaultProps\x3dt.getDefaultProps()),D(t.prototype.render);for(var n in S)t.prototype[n]||(t.prototype[n]\x3dnull);return g.wrapFactory(h.createFactory(t))},injection:{injectMixin:function(e){I.push(e)}}};t.exports\x3dF},{"./Object.assign":27,"./ReactComponent":32,"./ReactContext":35,"./ReactCurrentOwner":36,"./ReactElement":50,"./ReactElementValidator":51,"./ReactEmptyComponent":52,"./ReactErrorUtils":53,"./ReactLegacyElement":59,"./ReactOwner":65,"./ReactPerf":66,"./ReactPropTransferer":67,"./ReactPropTypeLocationNames":68,"./ReactPropTypeLocations":69,"./ReactUpdates":77,"./instantiateReactComponent":123,"./invariant":124,"./keyMirror":130,"./keyOf":131,"./mapObject":132,"./monitorCodeUse":134,"./shouldUpdateReactComponent":138,"./warning":141}],35:[function(e,t){"use strict";var n\x3de("./Object.assign"),r\x3d{current:{},withContext:function(e,t){var o,a\x3dr.current;r.current\x3dn({},a,e);try{o\x3dt()}finally{r.current\x3da}return o}};t.exports\x3dr},{"./Object.assign":27}],36:[function(e,t){"use strict";var n\x3d{current:null};t.exports\x3dn},{}],37:[function(e,t){"use strict";function n(e){return o.markNonLegacyFactory(r.createFactory(e))}var r\x3de("./ReactElement"),o\x3d(e("./ReactElementValidator"),e("./ReactLegacyElement")),a\x3de("./mapObject"),i\x3da({a:"a",abbr:"abbr",address:"address",area:"area",article:"article",aside:"aside",audio:"audio",b:"b",base:"base",bdi:"bdi",bdo:"bdo",big:"big",blockquote:"blockquote",body:"body",br:"br",button:"button",canvas:"canvas",caption:"caption",cite:"cite",code:"code",col:"col",colgroup:"colgroup",data:"data",datalist:"datalist",dd:"dd",del:"del",details:"details",dfn:"dfn",dialog:"dialog",div:"div",dl:"dl",dt:"dt",em:"em",embed:"embed",fieldset:"fieldset",figcaption:"figcaption",figure:"figure",footer:"footer",form:"form",h1:"h1",h2:"h2",h3:"h3",h4:"h4",h5:"h5",h6:"h6",head:"head",header:"header",hr:"hr",html:"html",i:"i",iframe:"iframe",img:"img",input:"input",ins:"ins",kbd:"kbd",keygen:"keygen",label:"label",legend:"legend",li:"li",link:"link",main:"main",map:"map",mark:"mark",menu:"menu",menuitem:"menuitem",meta:"meta",meter:"meter",nav:"nav",noscript:"noscript",object:"object",ol:"ol",optgroup:"optgroup",option:"option",output:"output",p:"p",param:"param",picture:"picture",pre:"pre",progress:"progress",q:"q",rp:"rp",rt:"rt",ruby:"ruby",s:"s",samp:"samp",script:"script",section:"section",select:"select",small:"small",source:"source",span:"span",strong:"strong",style:"style",sub:"sub",summary:"summary",sup:"sup",table:"table",tbody:"tbody",td:"td",textarea:"textarea",tfoot:"tfoot",th:"th",thead:"thead",time:"time",title:"title",tr:"tr",track:"track",u:"u",ul:"ul","var":"var",video:"video",wbr:"wbr",circle:"circle",defs:"defs",ellipse:"ellipse",g:"g",line:"line",linearGradient:"linearGradient",mask:"mask",path:"path",pattern:"pattern",polygon:"polygon",polyline:"polyline",radialGradient:"radialGradient",rect:"rect",stop:"stop",svg:"svg",text:"text",tspan:"tspan"},n);t.exports\x3di},{"./ReactElement":50,"./ReactElementValidator":51,"./ReactLegacyElement":59,"./mapObject":132}],38:[function(e,t){"use strict";var n\x3de("./AutoFocusMixin"),r\x3de("./ReactBrowserComponentMixin"),o\x3de("./ReactCompositeComponent"),a\x3de("./ReactElement"),i\x3de("./ReactDOM"),s\x3de("./keyMirror"),u\x3da.createFactory(i.button.type),c\x3ds({onClick:!0,onDoubleClick:!0,onMouseDown:!0,onMouseMove:!0,onMouseUp:!0,onClickCapture:!0,onDoubleClickCapture:!0,onMouseDownCapture:!0,onMouseMoveCapture:!0,onMouseUpCapture:!0}),l\x3do.createClass({displayName:"ReactDOMButton",mixins:[n,r],render:function(){var e\x3d{};for(var t in this.props)!this.props.hasOwnProperty(t)||this.props.disabled\x26\x26c[t]||(e[t]\x3dthis.props[t]);return u(e,this.props.children)}});t.exports\x3dl},{"./AutoFocusMixin":2,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50,"./keyMirror":130}],39:[function(e,t){"use strict";function n(e){e\x26\x26(g(null\x3d\x3de.children||null\x3d\x3de.dangerouslySetInnerHTML),g(null\x3d\x3de.style||"object"\x3d\x3dtypeof e.style))}function r(e,t,n,r){var o\x3dd.findReactContainerForID(e);if(o){var a\x3do.nodeType\x3d\x3d\x3dO?o.ownerDocument:o;C(t,a)}r.getPutListenerQueue().enqueuePutListener(e,t,n)}function o(e){_.call(P,e)||(g(x.test(e)),P[e]\x3d!0)}function a(e){o(e),this._tag\x3de,this.tagName\x3de.toUpperCase()}var i\x3de("./CSSPropertyOperations"),s\x3de("./DOMProperty"),u\x3de("./DOMPropertyOperations"),c\x3de("./ReactBrowserComponentMixin"),l\x3de("./ReactComponent"),p\x3de("./ReactBrowserEventEmitter"),d\x3de("./ReactMount"),f\x3de("./ReactMultiChild"),h\x3de("./ReactPerf"),m\x3de("./Object.assign"),v\x3de("./escapeTextForBrowser"),g\x3de("./invariant"),y\x3d(e("./isEventSupported"),e("./keyOf")),E\x3d(e("./monitorCodeUse"),p.deleteListener),C\x3dp.listenTo,R\x3dp.registrationNameModules,M\x3d{string:!0,number:!0},b\x3dy({style:null}),O\x3d1,D\x3d{area:!0,base:!0,br:!0,col:!0,embed:!0,hr:!0,img:!0,input:!0,keygen:!0,link:!0,meta:!0,param:!0,source:!0,track:!0,wbr:!0},x\x3d/^[a-zA-Z][a-zA-Z:_\\.\\-\\d]*$/,P\x3d{},_\x3d{}.hasOwnProperty;a.displayName\x3d"ReactDOMComponent",a.Mixin\x3d{mountComponent:h.measure("ReactDOMComponent","mountComponent",function(e,t,r){l.Mixin.mountComponent.call(this,e,t,r),n(this.props);var o\x3dD[this._tag]?"":"\x3c/"+this._tag+"\x3e";return this._createOpenTagMarkupAndPutListeners(t)+this._createContentMarkup(t)+o}),_createOpenTagMarkupAndPutListeners:function(e){var t\x3dthis.props,n\x3d"\x3c"+this._tag;for(var o in t)if(t.hasOwnProperty(o)){var a\x3dt[o];if(null!\x3da)if(R.hasOwnProperty(o))r(this._rootNodeID,o,a,e);else{o\x3d\x3d\x3db\x26\x26(a\x26\x26(a\x3dt.style\x3dm({},t.style)),a\x3di.createMarkupForStyles(a));var s\x3du.createMarkupForProperty(o,a);s\x26\x26(n+\x3d" "+s)}}if(e.renderToStaticMarkup)return n+"\x3e";var c\x3du.createMarkupForID(this._rootNodeID);return n+" "+c+"\x3e"},_createContentMarkup:function(e){var t\x3dthis.props.dangerouslySetInnerHTML;if(null!\x3dt){if(null!\x3dt.__html)return t.__html}else{var n\x3dM[typeof this.props.children]?this.props.children:null,r\x3dnull!\x3dn?null:this.props.children;if(null!\x3dn)return v(n);if(null!\x3dr){var o\x3dthis.mountChildren(r,e);return o.join("")}}return""},receiveComponent:function(e,t){(e!\x3d\x3dthis._currentElement||null\x3d\x3de._owner)\x26\x26l.Mixin.receiveComponent.call(this,e,t)},updateComponent:h.measure("ReactDOMComponent","updateComponent",function(e,t){n(this._currentElement.props),l.Mixin.updateComponent.call(this,e,t),this._updateDOMProperties(t.props,e),this._updateDOMChildren(t.props,e)}),_updateDOMProperties:function(e,t){var n,o,a,i\x3dthis.props;for(n in e)if(!i.hasOwnProperty(n)\x26\x26e.hasOwnProperty(n))if(n\x3d\x3d\x3db){var u\x3de[n];for(o in u)u.hasOwnProperty(o)\x26\x26(a\x3da||{},a[o]\x3d"")}else R.hasOwnProperty(n)?E(this._rootNodeID,n):(s.isStandardName[n]||s.isCustomAttribute(n))\x26\x26l.BackendIDOperations.deletePropertyByID(this._rootNodeID,n);for(n in i){var c\x3di[n],p\x3de[n];if(i.hasOwnProperty(n)\x26\x26c!\x3d\x3dp)if(n\x3d\x3d\x3db)if(c\x26\x26(c\x3di.style\x3dm({},c)),p){for(o in p)!p.hasOwnProperty(o)||c\x26\x26c.hasOwnProperty(o)||(a\x3da||{},a[o]\x3d"");for(o in c)c.hasOwnProperty(o)\x26\x26p[o]!\x3d\x3dc[o]\x26\x26(a\x3da||{},a[o]\x3dc[o])}else a\x3dc;else R.hasOwnProperty(n)?r(this._rootNodeID,n,c,t):(s.isStandardName[n]||s.isCustomAttribute(n))\x26\x26l.BackendIDOperations.updatePropertyByID(this._rootNodeID,n,c)}a\x26\x26l.BackendIDOperations.updateStylesByID(this._rootNodeID,a)},_updateDOMChildren:function(e,t){var n\x3dthis.props,r\x3dM[typeof e.children]?e.children:null,o\x3dM[typeof n.children]?n.children:null,a\x3de.dangerouslySetInnerHTML\x26\x26e.dangerouslySetInnerHTML.__html,i\x3dn.dangerouslySetInnerHTML\x26\x26n.dangerouslySetInnerHTML.__html,s\x3dnull!\x3dr?null:e.children,u\x3dnull!\x3do?null:n.children,c\x3dnull!\x3dr||null!\x3da,p\x3dnull!\x3do||null!\x3di;null!\x3ds\x26\x26null\x3d\x3du?this.updateChildren(null,t):c\x26\x26!p\x26\x26this.updateTextContent(""),null!\x3do?r!\x3d\x3do\x26\x26this.updateTextContent(""+o):null!\x3di?a!\x3d\x3di\x26\x26l.BackendIDOperations.updateInnerHTMLByID(this._rootNodeID,i):null!\x3du\x26\x26this.updateChildren(u,t)},unmountComponent:function(){this.unmountChildren(),p.deleteAllListeners(this._rootNodeID),l.Mixin.unmountComponent.call(this)}},m(a.prototype,l.Mixin,a.Mixin,f.Mixin,c),t.exports\x3da},{"./CSSPropertyOperations":5,"./DOMProperty":11,"./DOMPropertyOperations":12,"./Object.assign":27,"./ReactBrowserComponentMixin":29,"./ReactBrowserEventEmitter":30,"./ReactComponent":32,"./ReactMount":61,"./ReactMultiChild":62,"./ReactPerf":66,"./escapeTextForBrowser":107,"./invariant":124,"./isEventSupported":125,"./keyOf":131,"./monitorCodeUse":134}],40:[function(e,t){"use strict";var n\x3de("./EventConstants"),r\x3de("./LocalEventTrapMixin"),o\x3de("./ReactBrowserComponentMixin"),a\x3de("./ReactCompositeComponent"),i\x3de("./ReactElement"),s\x3de("./ReactDOM"),u\x3di.createFactory(s.form.type),c\x3da.createClass({displayName:"ReactDOMForm",mixins:[o,r],render:function(){return u(this.props)},componentDidMount:function(){this.trapBubbledEvent(n.topLevelTypes.topReset,"reset"),this.trapBubbledEvent(n.topLevelTypes.topSubmit,"submit")}});t.exports\x3dc},{"./EventConstants":16,"./LocalEventTrapMixin":25,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50}],41:[function(e,t){"use strict";var n\x3de("./CSSPropertyOperations"),r\x3de("./DOMChildrenOperations"),o\x3de("./DOMPropertyOperations"),a\x3de("./ReactMount"),i\x3de("./ReactPerf"),s\x3de("./invariant"),u\x3de("./setInnerHTML"),c\x3d{dangerouslySetInnerHTML:"`dangerouslySetInnerHTML` must be set using `updateInnerHTMLByID()`.",style:"`style` must be set using `updateStylesByID()`."},l\x3d{updatePropertyByID:i.measure("ReactDOMIDOperations","updatePropertyByID",function(e,t,n){var r\x3da.getNode(e);s(!c.hasOwnProperty(t)),null!\x3dn?o.setValueForProperty(r,t,n):o.deleteValueForProperty(r,t)}),deletePropertyByID:i.measure("ReactDOMIDOperations","deletePropertyByID",function(e,t,n){var r\x3da.getNode(e);s(!c.hasOwnProperty(t)),o.deleteValueForProperty(r,t,n)}),updateStylesByID:i.measure("ReactDOMIDOperations","updateStylesByID",function(e,t){var r\x3da.getNode(e);n.setValueForStyles(r,t)}),updateInnerHTMLByID:i.measure("ReactDOMIDOperations","updateInnerHTMLByID",function(e,t){var n\x3da.getNode(e);u(n,t)}),updateTextContentByID:i.measure("ReactDOMIDOperations","updateTextContentByID",function(e,t){var n\x3da.getNode(e);r.updateTextContent(n,t)}),dangerouslyReplaceNodeWithMarkupByID:i.measure("ReactDOMIDOperations","dangerouslyReplaceNodeWithMarkupByID",function(e,t){var n\x3da.getNode(e);r.dangerouslyReplaceNodeWithMarkup(n,t)}),dangerouslyProcessChildrenUpdates:i.measure("ReactDOMIDOperations","dangerouslyProcessChildrenUpdates",function(e,t){for(var n\x3d0;n\x3ce.length;n++)e[n].parentNode\x3da.getNode(e[n].parentID);r.processUpdates(e,t)})};t.exports\x3dl},{"./CSSPropertyOperations":5,"./DOMChildrenOperations":10,"./DOMPropertyOperations":12,"./ReactMount":61,"./ReactPerf":66,"./invariant":124,"./setInnerHTML":136}],42:[function(e,t){"use strict";var n\x3de("./EventConstants"),r\x3de("./LocalEventTrapMixin"),o\x3de("./ReactBrowserComponentMixin"),a\x3de("./ReactCompositeComponent"),i\x3de("./ReactElement"),s\x3de("./ReactDOM"),u\x3di.createFactory(s.img.type),c\x3da.createClass({displayName:"ReactDOMImg",tagName:"IMG",mixins:[o,r],render:function(){return u(this.props)},componentDidMount:function(){this.trapBubbledEvent(n.topLevelTypes.topLoad,"load"),this.trapBubbledEvent(n.topLevelTypes.topError,"error")}});t.exports\x3dc},{"./EventConstants":16,"./LocalEventTrapMixin":25,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50}],43:[function(e,t){"use strict";function n(){this.isMounted()\x26\x26this.forceUpdate()}var r\x3de("./AutoFocusMixin"),o\x3de("./DOMPropertyOperations"),a\x3de("./LinkedValueUtils"),i\x3de("./ReactBrowserComponentMixin"),s\x3de("./ReactCompositeComponent"),u\x3de("./ReactElement"),c\x3de("./ReactDOM"),l\x3de("./ReactMount"),p\x3de("./ReactUpdates"),d\x3de("./Object.assign"),f\x3de("./invariant"),h\x3du.createFactory(c.input.type),m\x3d{},v\x3ds.createClass({displayName:"ReactDOMInput",mixins:[r,a.Mixin,i],getInitialState:function(){var e\x3dthis.props.defaultValue;return{initialChecked:this.props.defaultChecked||!1,initialValue:null!\x3de?e:null}},render:function(){var e\x3dd({},this.props);e.defaultChecked\x3dnull,e.defaultValue\x3dnull;var t\x3da.getValue(this);e.value\x3dnull!\x3dt?t:this.state.initialValue;var n\x3da.getChecked(this);return e.checked\x3dnull!\x3dn?n:this.state.initialChecked,e.onChange\x3dthis._handleChange,h(e,this.props.children)},componentDidMount:function(){var e\x3dl.getID(this.getDOMNode());m[e]\x3dthis},componentWillUnmount:function(){var e\x3dthis.getDOMNode(),t\x3dl.getID(e);delete m[t]},componentDidUpdate:function(){var e\x3dthis.getDOMNode();null!\x3dthis.props.checked\x26\x26o.setValueForProperty(e,"checked",this.props.checked||!1);var t\x3da.getValue(this);null!\x3dt\x26\x26o.setValueForProperty(e,"value",""+t)},_handleChange:function(e){var t,r\x3da.getOnChange(this);r\x26\x26(t\x3dr.call(this,e)),p.asap(n,this);var o\x3dthis.props.name;if("radio"\x3d\x3d\x3dthis.props.type\x26\x26null!\x3do){for(var i\x3dthis.getDOMNode(),s\x3di;s.parentNode;)s\x3ds.parentNode;for(var u\x3ds.querySelectorAll("input[name\x3d"+JSON.stringify(""+o)+\'][type\x3d"radio"]\'),c\x3d0,d\x3du.length;d\x3ec;c++){var h\x3du[c];if(h!\x3d\x3di\x26\x26h.form\x3d\x3d\x3di.form){var v\x3dl.getID(h);f(v);var g\x3dm[v];f(g),p.asap(n,g)}}}return t}});t.exports\x3dv},{"./AutoFocusMixin":2,"./DOMPropertyOperations":12,"./LinkedValueUtils":24,"./Object.assign":27,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50,"./ReactMount":61,"./ReactUpdates":77,"./invariant":124}],44:[function(e,t){"use strict";var n\x3de("./ReactBrowserComponentMixin"),r\x3de("./ReactCompositeComponent"),o\x3de("./ReactElement"),a\x3de("./ReactDOM"),i\x3d(e("./warning"),o.createFactory(a.option.type)),s\x3dr.createClass({displayName:"ReactDOMOption",mixins:[n],componentWillMount:function(){},render:function(){return i(this.props,this.props.children)}});t.exports\x3ds},{"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50,"./warning":141}],45:[function(e,t){"use strict";function n(){this.isMounted()\x26\x26(this.setState({value:this._pendingValue}),this._pendingValue\x3d0)}function r(e,t){if(null!\x3de[t])if(e.multiple){if(!Array.isArray(e[t]))return new Error("The `"+t+"` prop supplied to \x3cselect\x3e must be an array if `multiple` is true.")}else if(Array.isArray(e[t]))return new Error("The `"+t+"` prop supplied to \x3cselect\x3e must be a scalar value if `multiple` is false.")}function o(e,t){var n,r,o,a\x3de.props.multiple,i\x3dnull!\x3dt?t:e.state.value,s\x3de.getDOMNode().options;if(a)for(n\x3d{},r\x3d0,o\x3di.length;o\x3er;++r)n[""+i[r]]\x3d!0;else n\x3d""+i;for(r\x3d0,o\x3ds.length;o\x3er;r++){var u\x3da?n.hasOwnProperty(s[r].value):s[r].value\x3d\x3d\x3dn;u!\x3d\x3ds[r].selected\x26\x26(s[r].selected\x3du)}}var a\x3de("./AutoFocusMixin"),i\x3de("./LinkedValueUtils"),s\x3de("./ReactBrowserComponentMixin"),u\x3de("./ReactCompositeComponent"),c\x3de("./ReactElement"),l\x3de("./ReactDOM"),p\x3de("./ReactUpdates"),d\x3de("./Object.assign"),f\x3dc.createFactory(l.select.type),h\x3du.createClass({displayName:"ReactDOMSelect",mixins:[a,i.Mixin,s],propTypes:{defaultValue:r,value:r},getInitialState:function(){return{value:this.props.defaultValue||(this.props.multiple?[]:"")}},componentWillMount:function(){this._pendingValue\x3dnull},componentWillReceiveProps:function(e){!this.props.multiple\x26\x26e.multiple?this.setState({value:[this.state.value]}):this.props.multiple\x26\x26!e.multiple\x26\x26this.setState({value:this.state.value[0]})\n},render:function(){var e\x3dd({},this.props);return e.onChange\x3dthis._handleChange,e.value\x3dnull,f(e,this.props.children)},componentDidMount:function(){o(this,i.getValue(this))},componentDidUpdate:function(e){var t\x3di.getValue(this),n\x3d!!e.multiple,r\x3d!!this.props.multiple;(null!\x3dt||n!\x3d\x3dr)\x26\x26o(this,t)},_handleChange:function(e){var t,r\x3di.getOnChange(this);r\x26\x26(t\x3dr.call(this,e));var o;if(this.props.multiple){o\x3d[];for(var a\x3de.target.options,s\x3d0,u\x3da.length;u\x3es;s++)a[s].selected\x26\x26o.push(a[s].value)}else o\x3de.target.value;return this._pendingValue\x3do,p.asap(n,this),t}});t.exports\x3dh},{"./AutoFocusMixin":2,"./LinkedValueUtils":24,"./Object.assign":27,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50,"./ReactUpdates":77}],46:[function(e,t){"use strict";function n(e,t,n,r){return e\x3d\x3d\x3dn\x26\x26t\x3d\x3d\x3dr}function r(e){var t\x3ddocument.selection,n\x3dt.createRange(),r\x3dn.text.length,o\x3dn.duplicate();o.moveToElementText(e),o.setEndPoint("EndToStart",n);var a\x3do.text.length,i\x3da+r;return{start:a,end:i}}function o(e){var t\x3dwindow.getSelection\x26\x26window.getSelection();if(!t||0\x3d\x3d\x3dt.rangeCount)return null;var r\x3dt.anchorNode,o\x3dt.anchorOffset,a\x3dt.focusNode,i\x3dt.focusOffset,s\x3dt.getRangeAt(0),u\x3dn(t.anchorNode,t.anchorOffset,t.focusNode,t.focusOffset),c\x3du?0:s.toString().length,l\x3ds.cloneRange();l.selectNodeContents(e),l.setEnd(s.startContainer,s.startOffset);var p\x3dn(l.startContainer,l.startOffset,l.endContainer,l.endOffset),d\x3dp?0:l.toString().length,f\x3dd+c,h\x3ddocument.createRange();h.setStart(r,o),h.setEnd(a,i);var m\x3dh.collapsed;return{start:m?f:d,end:m?d:f}}function a(e,t){var n,r,o\x3ddocument.selection.createRange().duplicate();"undefined"\x3d\x3dtypeof t.end?(n\x3dt.start,r\x3dn):t.start\x3et.end?(n\x3dt.end,r\x3dt.start):(n\x3dt.start,r\x3dt.end),o.moveToElementText(e),o.moveStart("character",n),o.setEndPoint("EndToStart",o),o.moveEnd("character",r-n),o.select()}function i(e,t){if(window.getSelection){var n\x3dwindow.getSelection(),r\x3de[c()].length,o\x3dMath.min(t.start,r),a\x3d"undefined"\x3d\x3dtypeof t.end?o:Math.min(t.end,r);if(!n.extend\x26\x26o\x3ea){var i\x3da;a\x3do,o\x3di}var s\x3du(e,o),l\x3du(e,a);if(s\x26\x26l){var p\x3ddocument.createRange();p.setStart(s.node,s.offset),n.removeAllRanges(),o\x3ea?(n.addRange(p),n.extend(l.node,l.offset)):(p.setEnd(l.node,l.offset),n.addRange(p))}}}var s\x3de("./ExecutionEnvironment"),u\x3de("./getNodeForCharacterOffset"),c\x3de("./getTextContentAccessor"),l\x3ds.canUseDOM\x26\x26document.selection,p\x3d{getOffsets:l?r:o,setOffsets:l?a:i};t.exports\x3dp},{"./ExecutionEnvironment":22,"./getNodeForCharacterOffset":117,"./getTextContentAccessor":119}],47:[function(e,t){"use strict";function n(){this.isMounted()\x26\x26this.forceUpdate()}var r\x3de("./AutoFocusMixin"),o\x3de("./DOMPropertyOperations"),a\x3de("./LinkedValueUtils"),i\x3de("./ReactBrowserComponentMixin"),s\x3de("./ReactCompositeComponent"),u\x3de("./ReactElement"),c\x3de("./ReactDOM"),l\x3de("./ReactUpdates"),p\x3de("./Object.assign"),d\x3de("./invariant"),f\x3d(e("./warning"),u.createFactory(c.textarea.type)),h\x3ds.createClass({displayName:"ReactDOMTextarea",mixins:[r,a.Mixin,i],getInitialState:function(){var e\x3dthis.props.defaultValue,t\x3dthis.props.children;null!\x3dt\x26\x26(d(null\x3d\x3de),Array.isArray(t)\x26\x26(d(t.length\x3c\x3d1),t\x3dt[0]),e\x3d""+t),null\x3d\x3de\x26\x26(e\x3d"");var n\x3da.getValue(this);return{initialValue:""+(null!\x3dn?n:e)}},render:function(){var e\x3dp({},this.props);return d(null\x3d\x3de.dangerouslySetInnerHTML),e.defaultValue\x3dnull,e.value\x3dnull,e.onChange\x3dthis._handleChange,f(e,this.state.initialValue)},componentDidUpdate:function(){var e\x3da.getValue(this);if(null!\x3de){var t\x3dthis.getDOMNode();o.setValueForProperty(t,"value",""+e)}},_handleChange:function(e){var t,r\x3da.getOnChange(this);return r\x26\x26(t\x3dr.call(this,e)),l.asap(n,this),t}});t.exports\x3dh},{"./AutoFocusMixin":2,"./DOMPropertyOperations":12,"./LinkedValueUtils":24,"./Object.assign":27,"./ReactBrowserComponentMixin":29,"./ReactCompositeComponent":34,"./ReactDOM":37,"./ReactElement":50,"./ReactUpdates":77,"./invariant":124,"./warning":141}],48:[function(e,t){"use strict";function n(){this.reinitializeTransaction()}var r\x3de("./ReactUpdates"),o\x3de("./Transaction"),a\x3de("./Object.assign"),i\x3de("./emptyFunction"),s\x3d{initialize:i,close:function(){p.isBatchingUpdates\x3d!1}},u\x3d{initialize:i,close:r.flushBatchedUpdates.bind(r)},c\x3d[u,s];a(n.prototype,o.Mixin,{getTransactionWrappers:function(){return c}});var l\x3dnew n,p\x3d{isBatchingUpdates:!1,batchedUpdates:function(e,t,n){var r\x3dp.isBatchingUpdates;p.isBatchingUpdates\x3d!0,r?e(t,n):l.perform(e,null,t,n)}};t.exports\x3dp},{"./Object.assign":27,"./ReactUpdates":77,"./Transaction":93,"./emptyFunction":105}],49:[function(e,t){"use strict";function n(){O.EventEmitter.injectReactEventListener(b),O.EventPluginHub.injectEventPluginOrder(s),O.EventPluginHub.injectInstanceHandle(D),O.EventPluginHub.injectMount(x),O.EventPluginHub.injectEventPluginsByName({SimpleEventPlugin:w,EnterLeaveEventPlugin:u,ChangeEventPlugin:o,CompositionEventPlugin:i,MobileSafariClickEventPlugin:p,SelectEventPlugin:P,BeforeInputEventPlugin:r}),O.NativeComponent.injectGenericComponentClass(m),O.NativeComponent.injectComponentClasses({button:v,form:g,img:y,input:E,option:C,select:R,textarea:M,html:N("html"),head:N("head"),body:N("body")}),O.CompositeComponent.injectMixin(d),O.DOMProperty.injectDOMPropertyConfig(l),O.DOMProperty.injectDOMPropertyConfig(T),O.EmptyComponent.injectEmptyComponent("noscript"),O.Updates.injectReconcileTransaction(f.ReactReconcileTransaction),O.Updates.injectBatchingStrategy(h),O.RootIndex.injectCreateReactRootIndex(c.canUseDOM?a.createReactRootIndex:_.createReactRootIndex),O.Component.injectEnvironment(f)}var r\x3de("./BeforeInputEventPlugin"),o\x3de("./ChangeEventPlugin"),a\x3de("./ClientReactRootIndex"),i\x3de("./CompositionEventPlugin"),s\x3de("./DefaultEventPluginOrder"),u\x3de("./EnterLeaveEventPlugin"),c\x3de("./ExecutionEnvironment"),l\x3de("./HTMLDOMPropertyConfig"),p\x3de("./MobileSafariClickEventPlugin"),d\x3de("./ReactBrowserComponentMixin"),f\x3de("./ReactComponentBrowserEnvironment"),h\x3de("./ReactDefaultBatchingStrategy"),m\x3de("./ReactDOMComponent"),v\x3de("./ReactDOMButton"),g\x3de("./ReactDOMForm"),y\x3de("./ReactDOMImg"),E\x3de("./ReactDOMInput"),C\x3de("./ReactDOMOption"),R\x3de("./ReactDOMSelect"),M\x3de("./ReactDOMTextarea"),b\x3de("./ReactEventListener"),O\x3de("./ReactInjection"),D\x3de("./ReactInstanceHandles"),x\x3de("./ReactMount"),P\x3de("./SelectEventPlugin"),_\x3de("./ServerReactRootIndex"),w\x3de("./SimpleEventPlugin"),T\x3de("./SVGDOMPropertyConfig"),N\x3de("./createFullPageComponent");t.exports\x3d{inject:n}},{"./BeforeInputEventPlugin":3,"./ChangeEventPlugin":7,"./ClientReactRootIndex":8,"./CompositionEventPlugin":9,"./DefaultEventPluginOrder":14,"./EnterLeaveEventPlugin":15,"./ExecutionEnvironment":22,"./HTMLDOMPropertyConfig":23,"./MobileSafariClickEventPlugin":26,"./ReactBrowserComponentMixin":29,"./ReactComponentBrowserEnvironment":33,"./ReactDOMButton":38,"./ReactDOMComponent":39,"./ReactDOMForm":40,"./ReactDOMImg":42,"./ReactDOMInput":43,"./ReactDOMOption":44,"./ReactDOMSelect":45,"./ReactDOMTextarea":47,"./ReactDefaultBatchingStrategy":48,"./ReactEventListener":55,"./ReactInjection":56,"./ReactInstanceHandles":58,"./ReactMount":61,"./SVGDOMPropertyConfig":78,"./SelectEventPlugin":79,"./ServerReactRootIndex":80,"./SimpleEventPlugin":81,"./createFullPageComponent":101}],50:[function(e,t){"use strict";var n\x3de("./ReactContext"),r\x3de("./ReactCurrentOwner"),o\x3d(e("./warning"),{key:!0,ref:!0}),a\x3dfunction(e,t,n,r,o,a){this.type\x3de,this.key\x3dt,this.ref\x3dn,this._owner\x3dr,this._context\x3do,this.props\x3da};a.prototype\x3d{_isReactElement:!0},a.createElement\x3dfunction(e,t,i){var s,u\x3d{},c\x3dnull,l\x3dnull;if(null!\x3dt){l\x3dvoid 0\x3d\x3d\x3dt.ref?null:t.ref,c\x3dnull\x3d\x3dt.key?null:""+t.key;for(s in t)t.hasOwnProperty(s)\x26\x26!o.hasOwnProperty(s)\x26\x26(u[s]\x3dt[s])}var p\x3darguments.length-2;if(1\x3d\x3d\x3dp)u.children\x3di;else if(p\x3e1){for(var d\x3dArray(p),f\x3d0;p\x3ef;f++)d[f]\x3darguments[f+2];u.children\x3dd}if(e.defaultProps){var h\x3de.defaultProps;for(s in h)"undefined"\x3d\x3dtypeof u[s]\x26\x26(u[s]\x3dh[s])}return new a(e,c,l,r.current,n.current,u)},a.createFactory\x3dfunction(e){var t\x3da.createElement.bind(null,e);return t.type\x3de,t},a.cloneAndReplaceProps\x3dfunction(e,t){var n\x3dnew a(e.type,e.key,e.ref,e._owner,e._context,t);return n},a.isValidElement\x3dfunction(e){var t\x3d!(!e||!e._isReactElement);return t},t.exports\x3da},{"./ReactContext":35,"./ReactCurrentOwner":36,"./warning":141}],51:[function(e,t){"use strict";function n(){var e\x3dp.current;return e\x26\x26e.constructor.displayName||void 0}function r(e,t){e._store.validated||null!\x3de.key||(e._store.validated\x3d!0,a("react_key_warning",\'Each child in an array should have a unique "key" prop.\',e,t))}function o(e,t,n){v.test(e)\x26\x26a("react_numeric_key_warning","Child objects should have non-numeric keys so ordering is preserved.",t,n)}function a(e,t,r,o){var a\x3dn(),i\x3do.displayName,s\x3da||i,u\x3df[e];if(!u.hasOwnProperty(s)){u[s]\x3d!0,t+\x3da?" Check the render method of "+a+".":" Check the renderComponent call using \x3c"+i+"\x3e.";var c\x3dnull;r._owner\x26\x26r._owner!\x3d\x3dp.current\x26\x26(c\x3dr._owner.constructor.displayName,t+\x3d" It was passed a child from "+c+"."),t+\x3d" See http://fb.me/react-warning-keys for more information.",d(e,{component:s,componentOwner:c}),console.warn(t)}}function i(){var e\x3dn()||"";h.hasOwnProperty(e)||(h[e]\x3d!0,d("react_object_map_children"))}function s(e,t){if(Array.isArray(e))for(var n\x3d0;n\x3ce.length;n++){var a\x3de[n];c.isValidElement(a)\x26\x26r(a,t)}else if(c.isValidElement(e))e._store.validated\x3d!0;else if(e\x26\x26"object"\x3d\x3dtypeof e){i();for(var s in e)o(s,e[s],t)}}function u(e,t,n,r){for(var o in t)if(t.hasOwnProperty(o)){var a;try{a\x3dt[o](n,o,e,r)}catch(i){a\x3di}a instanceof Error\x26\x26!(a.message in m)\x26\x26(m[a.message]\x3d!0,d("react_failed_descriptor_type_check",{message:a.message}))}}var c\x3de("./ReactElement"),l\x3de("./ReactPropTypeLocations"),p\x3de("./ReactCurrentOwner"),d\x3de("./monitorCodeUse"),f\x3d{react_key_warning:{},react_numeric_key_warning:{}},h\x3d{},m\x3d{},v\x3d/^\\d+$/,g\x3d{createElement:function(e){var t\x3dc.createElement.apply(this,arguments);if(null\x3d\x3dt)return t;for(var n\x3d2;n\x3carguments.length;n++)s(arguments[n],e);var r\x3de.displayName;return e.propTypes\x26\x26u(r,e.propTypes,t.props,l.prop),e.contextTypes\x26\x26u(r,e.contextTypes,t._context,l.context),t},createFactory:function(e){var t\x3dg.createElement.bind(null,e);return t.type\x3de,t}};t.exports\x3dg},{"./ReactCurrentOwner":36,"./ReactElement":50,"./ReactPropTypeLocations":69,"./monitorCodeUse":134}],52:[function(e,t){"use strict";function n(){return u(i),i()}function r(e){c[e]\x3d!0}function o(e){delete c[e]}function a(e){return c[e]}var i,s\x3de("./ReactElement"),u\x3de("./invariant"),c\x3d{},l\x3d{injectEmptyComponent:function(e){i\x3ds.createFactory(e)}},p\x3d{deregisterNullComponentID:o,getEmptyComponent:n,injection:l,isNullComponentID:a,registerNullComponentID:r};t.exports\x3dp},{"./ReactElement":50,"./invariant":124}],53:[function(e,t){"use strict";var n\x3d{guard:function(e){return e}};t.exports\x3dn},{}],54:[function(e,t){"use strict";function n(e){r.enqueueEvents(e),r.processEventQueue()}var r\x3de("./EventPluginHub"),o\x3d{handleTopLevel:function(e,t,o,a){var i\x3dr.extractEvents(e,t,o,a);n(i)}};t.exports\x3do},{"./EventPluginHub":18}],55:[function(e,t){"use strict";function n(e){var t\x3dl.getID(e),n\x3dc.getReactRootIDFromNodeID(t),r\x3dl.findReactContainerForID(n),o\x3dl.getFirstReactDOM(r);return o}function r(e,t){this.topLevelType\x3de,this.nativeEvent\x3dt,this.ancestors\x3d[]}function o(e){for(var t\x3dl.getFirstReactDOM(f(e.nativeEvent))||window,r\x3dt;r;)e.ancestors.push(r),r\x3dn(r);for(var o\x3d0,a\x3de.ancestors.length;a\x3eo;o++){t\x3de.ancestors[o];var i\x3dl.getID(t)||"";m._handleTopLevel(e.topLevelType,t,i,e.nativeEvent)}}function a(e){var t\x3dh(window);e(t)}var i\x3de("./EventListener"),s\x3de("./ExecutionEnvironment"),u\x3de("./PooledClass"),c\x3de("./ReactInstanceHandles"),l\x3de("./ReactMount"),p\x3de("./ReactUpdates"),d\x3de("./Object.assign"),f\x3de("./getEventTarget"),h\x3de("./getUnboundedScrollPosition");d(r.prototype,{destructor:function(){this.topLevelType\x3dnull,this.nativeEvent\x3dnull,this.ancestors.length\x3d0}}),u.addPoolingTo(r,u.twoArgumentPooler);var m\x3d{_enabled:!0,_handleTopLevel:null,WINDOW_HANDLE:s.canUseDOM?window:null,setHandleTopLevel:function(e){m._handleTopLevel\x3de},setEnabled:function(e){m._enabled\x3d!!e},isEnabled:function(){return m._enabled},trapBubbledEvent:function(e,t,n){var r\x3dn;return r?i.listen(r,t,m.dispatchEvent.bind(null,e)):void 0},trapCapturedEvent:function(e,t,n){var r\x3dn;return r?i.capture(r,t,m.dispatchEvent.bind(null,e)):void 0},monitorScrollValue:function(e){var t\x3da.bind(null,e);i.listen(window,"scroll",t),i.listen(window,"resize",t)},dispatchEvent:function(e,t){if(m._enabled){var n\x3dr.getPooled(e,t);try{p.batchedUpdates(o,n)}finally{r.release(n)}}}};t.exports\x3dm},{"./EventListener":17,"./ExecutionEnvironment":22,"./Object.assign":27,"./PooledClass":28,"./ReactInstanceHandles":58,"./ReactMount":61,"./ReactUpdates":77,"./getEventTarget":115,"./getUnboundedScrollPosition":120}],56:[function(e,t){"use strict";var n\x3de("./DOMProperty"),r\x3de("./EventPluginHub"),o\x3de("./ReactComponent"),a\x3de("./ReactCompositeComponent"),i\x3de("./ReactEmptyComponent"),s\x3de("./ReactBrowserEventEmitter"),u\x3de("./ReactNativeComponent"),c\x3de("./ReactPerf"),l\x3de("./ReactRootIndex"),p\x3de("./ReactUpdates"),d\x3d{Component:o.injection,CompositeComponent:a.injection,DOMProperty:n.injection,EmptyComponent:i.injection,EventPluginHub:r.injection,EventEmitter:s.injection,NativeComponent:u.injection,Perf:c.injection,RootIndex:l.injection,Updates:p.injection};t.exports\x3dd},{"./DOMProperty":11,"./EventPluginHub":18,"./ReactBrowserEventEmitter":30,"./ReactComponent":32,"./ReactCompositeComponent":34,"./ReactEmptyComponent":52,"./ReactNativeComponent":64,"./ReactPerf":66,"./ReactRootIndex":73,"./ReactUpdates":77}],57:[function(e,t){"use strict";function n(e){return o(document.documentElement,e)}var r\x3de("./ReactDOMSelection"),o\x3de("./containsNode"),a\x3de("./focusNode"),i\x3de("./getActiveElement"),s\x3d{hasSelectionCapabilities:function(e){return e\x26\x26("INPUT"\x3d\x3d\x3de.nodeName\x26\x26"text"\x3d\x3d\x3de.type||"TEXTAREA"\x3d\x3d\x3de.nodeName||"true"\x3d\x3d\x3de.contentEditable)},getSelectionInformation:function(){var e\x3di();return{focusedElem:e,selectionRange:s.hasSelectionCapabilities(e)?s.getSelection(e):null}},restoreSelection:function(e){var t\x3di(),r\x3de.focusedElem,o\x3de.selectionRange;t!\x3d\x3dr\x26\x26n(r)\x26\x26(s.hasSelectionCapabilities(r)\x26\x26s.setSelection(r,o),a(r))},getSelection:function(e){var t;if("selectionStart"in e)t\x3d{start:e.selectionStart,end:e.selectionEnd};else if(document.selection\x26\x26"INPUT"\x3d\x3d\x3de.nodeName){var n\x3ddocument.selection.createRange();n.parentElement()\x3d\x3d\x3de\x26\x26(t\x3d{start:-n.moveStart("character",-e.value.length),end:-n.moveEnd("character",-e.value.length)})}else t\x3dr.getOffsets(e);return t||{start:0,end:0}},setSelection:function(e,t){var n\x3dt.start,o\x3dt.end;if("undefined"\x3d\x3dtypeof o\x26\x26(o\x3dn),"selectionStart"in e)e.selectionStart\x3dn,e.selectionEnd\x3dMath.min(o,e.value.length);else if(document.selection\x26\x26"INPUT"\x3d\x3d\x3de.nodeName){var a\x3de.createTextRange();a.collapse(!0),a.moveStart("character",n),a.moveEnd("character",o-n),a.select()}else r.setOffsets(e,t)}};t.exports\x3ds},{"./ReactDOMSelection":46,"./containsNode":99,"./focusNode":109,"./getActiveElement":111}],58:[function(e,t){"use strict";function n(e){return d+e.toString(36)}function r(e,t){return e.charAt(t)\x3d\x3d\x3dd||t\x3d\x3d\x3de.length}function o(e){return""\x3d\x3d\x3de||e.charAt(0)\x3d\x3d\x3dd\x26\x26e.charAt(e.length-1)!\x3d\x3dd}function a(e,t){return 0\x3d\x3d\x3dt.indexOf(e)\x26\x26r(t,e.length)}function i(e){return e?e.substr(0,e.lastIndexOf(d)):""}function s(e,t){if(p(o(e)\x26\x26o(t)),p(a(e,t)),e\x3d\x3d\x3dt)return e;for(var n\x3de.length+f,i\x3dn;i\x3ct.length\x26\x26!r(t,i);i++);return t.substr(0,i)}function u(e,t){var n\x3dMath.min(e.length,t.length);if(0\x3d\x3d\x3dn)return"";for(var a\x3d0,i\x3d0;n\x3e\x3di;i++)if(r(e,i)\x26\x26r(t,i))a\x3di;else if(e.charAt(i)!\x3d\x3dt.charAt(i))break;var s\x3de.substr(0,a);return p(o(s)),s}function c(e,t,n,r,o,u){e\x3de||"",t\x3dt||"",p(e!\x3d\x3dt);var c\x3da(t,e);p(c||a(e,t));for(var l\x3d0,d\x3dc?i:s,f\x3de;;f\x3dd(f,t)){var m;if(o\x26\x26f\x3d\x3d\x3de||u\x26\x26f\x3d\x3d\x3dt||(m\x3dn(f,c,r)),m\x3d\x3d\x3d!1||f\x3d\x3d\x3dt)break;p(l++\x3ch)}}var l\x3de("./ReactRootIndex"),p\x3de("./invariant"),d\x3d".",f\x3dd.length,h\x3d100,m\x3d{createReactRootID:function(){return n(l.createReactRootIndex())},createReactID:function(e,t){return e+t},getReactRootIDFromNodeID:function(e){if(e\x26\x26e.charAt(0)\x3d\x3d\x3dd\x26\x26e.length\x3e1){var t\x3de.indexOf(d,1);return t\x3e-1?e.substr(0,t):e}return null},traverseEnterLeave:function(e,t,n,r,o){var a\x3du(e,t);a!\x3d\x3de\x26\x26c(e,a,n,r,!1,!0),a!\x3d\x3dt\x26\x26c(a,t,n,o,!0,!1)},traverseTwoPhase:function(e,t,n){e\x26\x26(c("",e,t,n,!0,!1),c(e,"",t,n,!1,!0))},traverseAncestors:function(e,t,n){c("",e,t,n,!0,!1)},_getFirstCommonAncestorID:u,_getNextDescendantID:s,isAncestorIDOf:a,SEPARATOR:d};t.exports\x3dm},{"./ReactRootIndex":73,"./invariant":124}],59:[function(e,t){"use strict";function n(e,t){if("function"\x3d\x3dtypeof t)for(var n in t)if(t.hasOwnProperty(n)){var r\x3dt[n];if("function"\x3d\x3dtypeof r){var o\x3dr.bind(t);for(var a in r)r.hasOwnProperty(a)\x26\x26(o[a]\x3dr[a]);e[n]\x3do}else e[n]\x3dr}}var r\x3d(e("./ReactCurrentOwner"),e("./invariant")),o\x3d(e("./monitorCodeUse"),e("./warning"),{}),a\x3d{},i\x3d{};i.wrapCreateFactory\x3dfunction(e){var t\x3dfunction(t){return"function"!\x3dtypeof t?e(t):t.isReactNonLegacyFactory?e(t.type):t.isReactLegacyFactory?e(t.type):t};return t},i.wrapCreateElement\x3dfunction(e){var t\x3dfunction(t){if("function"!\x3dtypeof t)return e.apply(this,arguments);var n;return t.isReactNonLegacyFactory?(n\x3dArray.prototype.slice.call(arguments,0),n[0]\x3dt.type,e.apply(this,n)):t.isReactLegacyFactory?(t._isMockFunction\x26\x26(t.type._mockedReactClassConstructor\x3dt),n\x3dArray.prototype.slice.call(arguments,0),n[0]\x3dt.type,e.apply(this,n)):t.apply(null,Array.prototype.slice.call(arguments,1))};return t},i.wrapFactory\x3dfunction(e){r("function"\x3d\x3dtypeof e);var t\x3dfunction(){return e.apply(this,arguments)};return n(t,e.type),t.isReactLegacyFactory\x3do,t.type\x3de.type,t},i.markNonLegacyFactory\x3dfunction(e){return e.isReactNonLegacyFactory\x3da,e},i.isValidFactory\x3dfunction(e){return"function"\x3d\x3dtypeof e\x26\x26e.isReactLegacyFactory\x3d\x3d\x3do},i.isValidClass\x3dfunction(e){return i.isValidFactory(e)},i._isLegacyCallWarningEnabled\x3d!0,t.exports\x3di},{"./ReactCurrentOwner":36,"./invariant":124,"./monitorCodeUse":134,"./warning":141}],60:[function(e,t){"use strict";var n\x3de("./adler32"),r\x3d{CHECKSUM_ATTR_NAME:"data-react-checksum",addChecksumToMarkup:function(e){var t\x3dn(e);return e.replace("\x3e"," "+r.CHECKSUM_ATTR_NAME+\'\x3d"\'+t+\'"\x3e\')},canReuseMarkup:function(e,t){var o\x3dt.getAttribute(r.CHECKSUM_ATTR_NAME);o\x3do\x26\x26parseInt(o,10);var a\x3dn(e);return a\x3d\x3d\x3do}};t.exports\x3dr},{"./adler32":96}],61:[function(e,t){"use strict";function n(e){var t\x3dE(e);return t\x26\x26S.getID(t)}function r(e){var t\x3do(e);if(t)if(x.hasOwnProperty(t)){var n\x3dx[t];n!\x3d\x3de\x26\x26(R(!s(n,t)),x[t]\x3de)}else x[t]\x3de;return t}function o(e){return e\x26\x26e.getAttribute\x26\x26e.getAttribute(D)||""}function a(e,t){var n\x3do(e);n!\x3d\x3dt\x26\x26delete x[n],e.setAttribute(D,t),x[t]\x3de}function i(e){return x.hasOwnProperty(e)\x26\x26s(x[e],e)||(x[e]\x3dS.findReactNodeByID(e)),x[e]}function s(e,t){if(e){R(o(e)\x3d\x3d\x3dt);var n\x3dS.findReactContainerForID(t);if(n\x26\x26g(n,e))return!0}return!1}function u(e){delete x[e]}function c(e){var t\x3dx[e];return t\x26\x26s(t,e)?void(I\x3dt):!1}function l(e){I\x3dnull,m.traverseAncestors(e,c);var t\x3dI;return I\x3dnull,t}var p\x3de("./DOMProperty"),d\x3de("./ReactBrowserEventEmitter"),f\x3d(e("./ReactCurrentOwner"),e("./ReactElement")),h\x3de("./ReactLegacyElement"),m\x3de("./ReactInstanceHandles"),v\x3de("./ReactPerf"),g\x3de("./containsNode"),y\x3de("./deprecated"),E\x3de("./getReactRootElementInContainer"),C\x3de("./instantiateReactComponent"),R\x3de("./invariant"),M\x3de("./shouldUpdateReactComponent"),b\x3d(e("./warning"),h.wrapCreateElement(f.createElement)),O\x3dm.SEPARATOR,D\x3dp.ID_ATTRIBUTE_NAME,x\x3d{},P\x3d1,_\x3d9,w\x3d{},T\x3d{},N\x3d[],I\x3dnull,S\x3d{_instancesByReactRootID:w,scrollMonitor:function(e,t){t()},_updateRootComponent:function(e,t,n,r){var o\x3dt.props;return S.scrollMonitor(n,function(){e.replaceProps(o,r)}),e},_registerComponent:function(e,t){R(t\x26\x26(t.nodeType\x3d\x3d\x3dP||t.nodeType\x3d\x3d\x3d_)),d.ensureScrollValueMonitoring();var n\x3dS.registerContainer(t);return w[n]\x3de,n},_renderNewRootComponent:v.measure("ReactMount","_renderNewRootComponent",function(e,t,n){var r\x3dC(e,null),o\x3dS._registerComponent(r,t);return r.mountComponentIntoNode(o,t,n),r}),render:function(e,t,r){R(f.isValidElement(e));var o\x3dw[n(t)];if(o){var a\x3do._currentElement;if(M(a,e))return S._updateRootComponent(o,e,t,r);S.unmountComponentAtNode(t)}var i\x3dE(t),s\x3di\x26\x26S.isRenderedByReact(i),u\x3ds\x26\x26!o,c\x3dS._renderNewRootComponent(e,t,u);return r\x26\x26r.call(c),c},constructAndRenderComponent:function(e,t,n){var r\x3db(e,t);return S.render(r,n)},constructAndRenderComponentByID:function(e,t,n){var r\x3ddocument.getElementById(n);return R(r),S.constructAndRenderComponent(e,t,r)},registerContainer:function(e){var t\x3dn(e);return t\x26\x26(t\x3dm.getReactRootIDFromNodeID(t)),t||(t\x3dm.createReactRootID()),T[t]\x3de,t},unmountComponentAtNode:function(e){var t\x3dn(e),r\x3dw[t];return r?(S.unmountComponentFromNode(r,e),delete w[t],delete T[t],!0):!1},unmountComponentFromNode:function(e,t){for(e.unmountComponent(),t.nodeType\x3d\x3d\x3d_\x26\x26(t\x3dt.documentElement);t.lastChild;)t.removeChild(t.lastChild)},findReactContainerForID:function(e){var t\x3dm.getReactRootIDFromNodeID(e),n\x3dT[t];return n},findReactNodeByID:function(e){var t\x3dS.findReactContainerForID(e);return S.findComponentRoot(t,e)},isRenderedByReact:function(e){if(1!\x3d\x3de.nodeType)return!1;var t\x3dS.getID(e);return t?t.charAt(0)\x3d\x3d\x3dO:!1},getFirstReactDOM:function(e){for(var t\x3de;t\x26\x26t.parentNode!\x3d\x3dt;){if(S.isRenderedByReact(t))return t;t\x3dt.parentNode}return null},findComponentRoot:function(e,t){var n\x3dN,r\x3d0,o\x3dl(t)||e;for(n[0]\x3do.firstChild,n.length\x3d1;r\x3cn.length;){for(var a,i\x3dn[r++];i;){var s\x3dS.getID(i);s?t\x3d\x3d\x3ds?a\x3di:m.isAncestorIDOf(s,t)\x26\x26(n.length\x3dr\x3d0,n.push(i.firstChild)):n.push(i.firstChild),i\x3di.nextSibling}if(a)return n.length\x3d0,a}n.length\x3d0,R(!1)},getReactRootID:n,getID:r,setID:a,getNode:i,purgeID:u};S.renderComponent\x3dy("ReactMount","renderComponent","render",this,S.render),t.exports\x3dS},{"./DOMProperty":11,"./ReactBrowserEventEmitter":30,"./ReactCurrentOwner":36,"./ReactElement":50,"./ReactInstanceHandles":58,"./ReactLegacyElement":59,"./ReactPerf":66,"./containsNode":99,"./deprecated":104,"./getReactRootElementInContainer":118,"./instantiateReactComponent":123,"./invariant":124,"./shouldUpdateReactComponent":138,"./warning":141}],62:[function(e,t){"use strict";function n(e,t,n){h.push({parentID:e,parentNode:null,type:c.INSERT_MARKUP,markupIndex:m.push(t)-1,textContent:null,fromIndex:null,toIndex:n})}function r(e,t,n){h.push({parentID:e,parentNode:null,type:c.MOVE_EXISTING,markupIndex:null,textContent:null,fromIndex:t,toIndex:n})}function o(e,t){h.push({parentID:e,parentNode:null,type:c.REMOVE_NODE,markupIndex:null,textContent:null,fromIndex:t,toIndex:null})}function a(e,t){h.push({parentID:e,parentNode:null,type:c.TEXT_CONTENT,markupIndex:null,textContent:t,fromIndex:null,toIndex:null})}function i(){h.length\x26\x26(u.BackendIDOperations.dangerouslyProcessChildrenUpdates(h,m),s())}function s(){h.length\x3d0,m.length\x3d0}var u\x3de("./ReactComponent"),c\x3de("./ReactMultiChildUpdateTypes"),l\x3de("./flattenChildren"),p\x3de("./instantiateReactComponent"),d\x3de("./shouldUpdateReactComponent"),f\x3d0,h\x3d[],m\x3d[],v\x3d{Mixin:{mountChildren:function(e,t){var n\x3dl(e),r\x3d[],o\x3d0;this._renderedChildren\x3dn;for(var a in n){var i\x3dn[a];if(n.hasOwnProperty(a)){var s\x3dp(i,null);n[a]\x3ds;var u\x3dthis._rootNodeID+a,c\x3ds.mountComponent(u,t,this._mountDepth+1);s._mountIndex\x3do,r.push(c),o++}}return r},updateTextContent:function(e){f++;var t\x3d!0;try{var n\x3dthis._renderedChildren;for(var r in n)n.hasOwnProperty(r)\x26\x26this._unmountChildByName(n[r],r);this.setTextContent(e),t\x3d!1}finally{f--,f||(t?s():i())}},updateChildren:function(e,t){f++;var n\x3d!0;try{this._updateChildren(e,t),n\x3d!1}finally{f--,f||(n?s():i())}},_updateChildren:function(e,t){var n\x3dl(e),r\x3dthis._renderedChildren;if(n||r){var o,a\x3d0,i\x3d0;for(o in n)if(n.hasOwnProperty(o)){var s\x3dr\x26\x26r[o],u\x3ds\x26\x26s._currentElement,c\x3dn[o];if(d(u,c))this.moveChild(s,i,a),a\x3dMath.max(s._mountIndex,a),s.receiveComponent(c,t),s._mountIndex\x3di;else{s\x26\x26(a\x3dMath.max(s._mountIndex,a),this._unmountChildByName(s,o));var f\x3dp(c,null);this._mountChildByNameAtIndex(f,o,i,t)}i++}for(o in r)!r.hasOwnProperty(o)||n\x26\x26n[o]||this._unmountChildByName(r[o],o)}},unmountChildren:function(){var e\x3dthis._renderedChildren;for(var t in e){var n\x3de[t];n.unmountComponent\x26\x26n.unmountComponent()}this._renderedChildren\x3dnull},moveChild:function(e,t,n){e._mountIndex\x3cn\x26\x26r(this._rootNodeID,e._mountIndex,t)},createChild:function(e,t){n(this._rootNodeID,t,e._mountIndex)},removeChild:function(e){o(this._rootNodeID,e._mountIndex)},setTextContent:function(e){a(this._rootNodeID,e)},_mountChildByNameAtIndex:function(e,t,n,r){var o\x3dthis._rootNodeID+t,a\x3de.mountComponent(o,r,this._mountDepth+1);e._mountIndex\x3dn,this.createChild(e,a),this._renderedChildren\x3dthis._renderedChildren||{},this._renderedChildren[t]\x3de},_unmountChildByName:function(e,t){this.removeChild(e),e._mountIndex\x3dnull,e.unmountComponent(),delete this._renderedChildren[t]}}};t.exports\x3dv},{"./ReactComponent":32,"./ReactMultiChildUpdateTypes":63,"./flattenChildren":108,"./instantiateReactComponent":123,"./shouldUpdateReactComponent":138}],63:[function(e,t){"use strict";var n\x3de("./keyMirror"),r\x3dn({INSERT_MARKUP:null,MOVE_EXISTING:null,REMOVE_NODE:null,TEXT_CONTENT:null});t.exports\x3dr},{"./keyMirror":130}],64:[function(e,t){"use strict";function n(e,t,n){var r\x3di[e];return null\x3d\x3dr?(o(a),new a(e,t)):n\x3d\x3d\x3de?(o(a),new a(e,t)):new r.type(t)}var r\x3de("./Object.assign"),o\x3de("./invariant"),a\x3dnull,i\x3d{},s\x3d{injectGenericComponentClass:function(e){a\x3de},injectComponentClasses:function(e){r(i,e)}},u\x3d{createInstanceForTag:n,injection:s};t.exports\x3du},{"./Object.assign":27,"./invariant":124}],65:[function(e,t){"use strict";var n\x3de("./emptyObject"),r\x3de("./invariant"),o\x3d{isValidOwner:function(e){return!(!e||"function"!\x3dtypeof e.attachRef||"function"!\x3dtypeof e.detachRef)},addComponentAsRefTo:function(e,t,n){r(o.isValidOwner(n)),n.attachRef(t,e)},removeComponentAsRefFrom:function(e,t,n){r(o.isValidOwner(n)),n.refs[t]\x3d\x3d\x3de\x26\x26n.detachRef(t)},Mixin:{construct:function(){this.refs\x3dn},attachRef:function(e,t){r(t.isOwnedBy(this));var o\x3dthis.refs\x3d\x3d\x3dn?this.refs\x3d{}:this.refs;o[e]\x3dt},detachRef:function(e){delete this.refs[e]}}};t.exports\x3do},{"./emptyObject":106,"./invariant":124}],66:[function(e,t){"use strict";function n(e,t,n){return n}var r\x3d{enableMeasure:!1,storedMeasure:n,measure:function(e,t,n){return n},injection:{injectMeasure:function(e){r.storedMeasure\x3de}}};t.exports\x3dr},{}],67:[function(e,t){"use strict";function n(e){return function(t,n,r){t[n]\x3dt.hasOwnProperty(n)?e(t[n],r):r}}function r(e,t){for(var n in t)if(t.hasOwnProperty(n)){var r\x3dc[n];r\x26\x26c.hasOwnProperty(n)?r(e,n,t[n]):e.hasOwnProperty(n)||(e[n]\x3dt[n])}return e}var o\x3de("./Object.assign"),a\x3de("./emptyFunction"),i\x3de("./invariant"),s\x3de("./joinClasses"),u\x3d(e("./warning"),n(function(e,t){return o({},t,e)})),c\x3d{children:a,className:n(s),style:u},l\x3d{TransferStrategies:c,mergeProps:function(e,t){return r(o({},e),t)},Mixin:{transferPropsTo:function(e){return i(e._owner\x3d\x3d\x3dthis),r(e.props,this.props),e}}};t.exports\x3dl},{"./Object.assign":27,"./emptyFunction":105,"./invariant":124,"./joinClasses":129,"./warning":141}],68:[function(e,t){"use strict";var n\x3d{};t.exports\x3dn},{}],69:[function(e,t){"use strict";var n\x3de("./keyMirror"),r\x3dn({prop:null,context:null,childContext:null});t.exports\x3dr},{"./keyMirror":130}],70:[function(e,t){"use strict";function n(e){function t(t,n,r,o,a){if(o\x3do||C,null!\x3dn[r])return e(n,r,o,a);var i\x3dg[a];return t?new Error("Required "+i+" `"+r+"` was not specified in "+("`"+o+"`.")):void 0}var n\x3dt.bind(null,!1);return n.isRequired\x3dt.bind(null,!0),n}function r(e){function t(t,n,r,o){var a\x3dt[n],i\x3dh(a);if(i!\x3d\x3de){var s\x3dg[o],u\x3dm(a);return new Error("Invalid "+s+" `"+n+"` of type `"+u+"` "+("supplied to `"+r+"`, expected `"+e+"`."))}}return n(t)}function o(){return n(E.thatReturns())}function a(e){function t(t,n,r,o){var a\x3dt[n];if(!Array.isArray(a)){var i\x3dg[o],s\x3dh(a);return new Error("Invalid "+i+" `"+n+"` of type "+("`"+s+"` supplied to `"+r+"`, expected an array."))}for(var u\x3d0;u\x3ca.length;u++){var c\x3de(a,u,r,o);if(c instanceof Error)return c}}return n(t)}function i(){function e(e,t,n,r){if(!v.isValidElement(e[t])){var o\x3dg[r];return new Error("Invalid "+o+" `"+t+"` supplied to "+("`"+n+"`, expected a ReactElement."))}}return n(e)}function s(e){function t(t,n,r,o){if(!(t[n]instanceof e)){var a\x3dg[o],i\x3de.name||C;return new Error("Invalid "+a+" `"+n+"` supplied to "+("`"+r+"`, expected instance of `"+i+"`."))}}return n(t)}function u(e){function t(t,n,r,o){for(var a\x3dt[n],i\x3d0;i\x3ce.length;i++)if(a\x3d\x3d\x3de[i])return;var s\x3dg[o],u\x3dJSON.stringify(e);return new Error("Invalid "+s+" `"+n+"` of value `"+a+"` "+("supplied to `"+r+"`, expected one of "+u+"."))}return n(t)}function c(e){function t(t,n,r,o){var a\x3dt[n],i\x3dh(a);if("object"!\x3d\x3di){var s\x3dg[o];return new Error("Invalid "+s+" `"+n+"` of type "+("`"+i+"` supplied to `"+r+"`, expected an object."))}for(var u in a)if(a.hasOwnProperty(u)){var c\x3de(a,u,r,o);if(c instanceof Error)return c}}return n(t)}function l(e){function t(t,n,r,o){for(var a\x3d0;a\x3ce.length;a++){var i\x3de[a];if(null\x3d\x3di(t,n,r,o))return}var s\x3dg[o];return new Error("Invalid "+s+" `"+n+"` supplied to "+("`"+r+"`."))}return n(t)}function p(){function e(e,t,n,r){if(!f(e[t])){var o\x3dg[r];return new Error("Invalid "+o+" `"+t+"` supplied to "+("`"+n+"`, expected a ReactNode."))}}return n(e)}function d(e){function t(t,n,r,o){var a\x3dt[n],i\x3dh(a);if("object"!\x3d\x3di){var s\x3dg[o];return new Error("Invalid "+s+" `"+n+"` of type `"+i+"` "+("supplied to `"+r+"`, expected `object`."))}for(var u in e){var c\x3de[u];if(c){var l\x3dc(a,u,r,o);if(l)return l}}}return n(t,"expected `object`")}function f(e){switch(typeof e){case"number":case"string":return!0;case"boolean":return!e;case"object":if(Array.isArray(e))return e.every(f);if(v.isValidElement(e))return!0;for(var t in e)if(!f(e[t]))return!1;return!0;default:return!1}}function h(e){var t\x3dtypeof e;return Array.isArray(e)?"array":e instanceof RegExp?"object":t}function m(e){var t\x3dh(e);if("object"\x3d\x3d\x3dt){if(e instanceof Date)return"date";if(e instanceof RegExp)return"regexp"}return t}var v\x3de("./ReactElement"),g\x3de("./ReactPropTypeLocationNames"),y\x3de("./deprecated"),E\x3de("./emptyFunction"),C\x3d"\x3c\x3canonymous\x3e\x3e",R\x3di(),M\x3dp(),b\x3d{array:r("array"),bool:r("boolean"),func:r("function"),number:r("number"),object:r("object"),string:r("string"),any:o(),arrayOf:a,element:R,instanceOf:s,node:M,objectOf:c,oneOf:u,oneOfType:l,shape:d,component:y("React.PropTypes","component","element",this,R),renderable:y("React.PropTypes","renderable","node",this,M)};t.exports\x3db},{"./ReactElement":50,"./ReactPropTypeLocationNames":68,"./deprecated":104,"./emptyFunction":105}],71:[function(e,t){"use strict";function n(){this.listenersToPut\x3d[]}var r\x3de("./PooledClass"),o\x3de("./ReactBrowserEventEmitter"),a\x3de("./Object.assign");a(n.prototype,{enqueuePutListener:function(e,t,n){this.listenersToPut.push({rootNodeID:e,propKey:t,propValue:n})},putListeners:function(){for(var e\x3d0;e\x3cthis.listenersToPut.length;e++){var t\x3dthis.listenersToPut[e];o.putListener(t.rootNodeID,t.propKey,t.propValue)}},reset:function(){this.listenersToPut.length\x3d0},destructor:function(){this.reset()}}),r.addPoolingTo(n),t.exports\x3dn},{"./Object.assign":27,"./PooledClass":28,"./ReactBrowserEventEmitter":30}],72:[function(e,t){"use strict";function n(){this.reinitializeTransaction(),this.renderToStaticMarkup\x3d!1,this.reactMountReady\x3dr.getPooled(null),this.putListenerQueue\x3ds.getPooled()}var r\x3de("./CallbackQueue"),o\x3de("./PooledClass"),a\x3de("./ReactBrowserEventEmitter"),i\x3de("./ReactInputSelection"),s\x3de("./ReactPutListenerQueue"),u\x3de("./Transaction"),c\x3de("./Object.assign"),l\x3d{initialize:i.getSelectionInformation,close:i.restoreSelection},p\x3d{initialize:function(){var e\x3da.isEnabled();return a.setEnabled(!1),e},close:function(e){a.setEnabled(e)}},d\x3d{initialize:function(){this.reactMountReady.reset()},close:function(){this.reactMountReady.notifyAll()}},f\x3d{initialize:function(){this.putListenerQueue.reset()},close:function(){this.putListenerQueue.putListeners()}},h\x3d[f,l,p,d],m\x3d{getTransactionWrappers:function(){return h},getReactMountReady:function(){return this.reactMountReady},getPutListenerQueue:function(){return this.putListenerQueue},destructor:function(){r.release(this.reactMountReady),this.reactMountReady\x3dnull,s.release(this.putListenerQueue),this.putListenerQueue\x3dnull}};c(n.prototype,u.Mixin,m),o.addPoolingTo(n),t.exports\x3dn},{"./CallbackQueue":6,"./Object.assign":27,"./PooledClass":28,"./ReactBrowserEventEmitter":30,"./ReactInputSelection":57,"./ReactPutListenerQueue":71,"./Transaction":93}],73:[function(e,t){"use strict";\nvar n\x3d{injectCreateReactRootIndex:function(e){r.createReactRootIndex\x3de}},r\x3d{createReactRootIndex:null,injection:n};t.exports\x3dr},{}],74:[function(e,t){"use strict";function n(e){c(o.isValidElement(e));var t;try{var n\x3da.createReactRootID();return t\x3ds.getPooled(!1),t.perform(function(){var r\x3du(e,null),o\x3dr.mountComponent(n,t,0);return i.addChecksumToMarkup(o)},null)}finally{s.release(t)}}function r(e){c(o.isValidElement(e));var t;try{var n\x3da.createReactRootID();return t\x3ds.getPooled(!0),t.perform(function(){var r\x3du(e,null);return r.mountComponent(n,t,0)},null)}finally{s.release(t)}}var o\x3de("./ReactElement"),a\x3de("./ReactInstanceHandles"),i\x3de("./ReactMarkupChecksum"),s\x3de("./ReactServerRenderingTransaction"),u\x3de("./instantiateReactComponent"),c\x3de("./invariant");t.exports\x3d{renderToString:n,renderToStaticMarkup:r}},{"./ReactElement":50,"./ReactInstanceHandles":58,"./ReactMarkupChecksum":60,"./ReactServerRenderingTransaction":75,"./instantiateReactComponent":123,"./invariant":124}],75:[function(e,t){"use strict";function n(e){this.reinitializeTransaction(),this.renderToStaticMarkup\x3de,this.reactMountReady\x3do.getPooled(null),this.putListenerQueue\x3da.getPooled()}var r\x3de("./PooledClass"),o\x3de("./CallbackQueue"),a\x3de("./ReactPutListenerQueue"),i\x3de("./Transaction"),s\x3de("./Object.assign"),u\x3de("./emptyFunction"),c\x3d{initialize:function(){this.reactMountReady.reset()},close:u},l\x3d{initialize:function(){this.putListenerQueue.reset()},close:u},p\x3d[l,c],d\x3d{getTransactionWrappers:function(){return p},getReactMountReady:function(){return this.reactMountReady},getPutListenerQueue:function(){return this.putListenerQueue},destructor:function(){o.release(this.reactMountReady),this.reactMountReady\x3dnull,a.release(this.putListenerQueue),this.putListenerQueue\x3dnull}};s(n.prototype,i.Mixin,d),r.addPoolingTo(n),t.exports\x3dn},{"./CallbackQueue":6,"./Object.assign":27,"./PooledClass":28,"./ReactPutListenerQueue":71,"./Transaction":93,"./emptyFunction":105}],76:[function(e,t){"use strict";var n\x3de("./DOMPropertyOperations"),r\x3de("./ReactComponent"),o\x3de("./ReactElement"),a\x3de("./Object.assign"),i\x3de("./escapeTextForBrowser"),s\x3dfunction(){};a(s.prototype,r.Mixin,{mountComponent:function(e,t,o){r.Mixin.mountComponent.call(this,e,t,o);var a\x3di(this.props);return t.renderToStaticMarkup?a:"\x3cspan "+n.createMarkupForID(e)+"\x3e"+a+"\x3c/span\x3e"},receiveComponent:function(e){var t\x3de.props;t!\x3d\x3dthis.props\x26\x26(this.props\x3dt,r.BackendIDOperations.updateTextContentByID(this._rootNodeID,t))}});var u\x3dfunction(e){return new o(s,null,null,null,null,e)};u.type\x3ds,t.exports\x3du},{"./DOMPropertyOperations":12,"./Object.assign":27,"./ReactComponent":32,"./ReactElement":50,"./escapeTextForBrowser":107}],77:[function(e,t){"use strict";function n(){h(O.ReactReconcileTransaction\x26\x26y)}function r(){this.reinitializeTransaction(),this.dirtyComponentsLength\x3dnull,this.callbackQueue\x3dc.getPooled(),this.reconcileTransaction\x3dO.ReactReconcileTransaction.getPooled()}function o(e,t,r){n(),y.batchedUpdates(e,t,r)}function a(e,t){return e._mountDepth-t._mountDepth}function i(e){var t\x3de.dirtyComponentsLength;h(t\x3d\x3d\x3dm.length),m.sort(a);for(var n\x3d0;t\x3en;n++){var r\x3dm[n];if(r.isMounted()){var o\x3dr._pendingCallbacks;if(r._pendingCallbacks\x3dnull,r.performUpdateIfNecessary(e.reconcileTransaction),o)for(var i\x3d0;i\x3co.length;i++)e.callbackQueue.enqueue(o[i],r)}}}function s(e,t){return h(!t||"function"\x3d\x3dtypeof t),n(),y.isBatchingUpdates?(m.push(e),void(t\x26\x26(e._pendingCallbacks?e._pendingCallbacks.push(t):e._pendingCallbacks\x3d[t]))):void y.batchedUpdates(s,e,t)}function u(e,t){h(y.isBatchingUpdates),v.enqueue(e,t),g\x3d!0}var c\x3de("./CallbackQueue"),l\x3de("./PooledClass"),p\x3d(e("./ReactCurrentOwner"),e("./ReactPerf")),d\x3de("./Transaction"),f\x3de("./Object.assign"),h\x3de("./invariant"),m\x3d(e("./warning"),[]),v\x3dc.getPooled(),g\x3d!1,y\x3dnull,E\x3d{initialize:function(){this.dirtyComponentsLength\x3dm.length},close:function(){this.dirtyComponentsLength!\x3d\x3dm.length?(m.splice(0,this.dirtyComponentsLength),M()):m.length\x3d0}},C\x3d{initialize:function(){this.callbackQueue.reset()},close:function(){this.callbackQueue.notifyAll()}},R\x3d[E,C];f(r.prototype,d.Mixin,{getTransactionWrappers:function(){return R},destructor:function(){this.dirtyComponentsLength\x3dnull,c.release(this.callbackQueue),this.callbackQueue\x3dnull,O.ReactReconcileTransaction.release(this.reconcileTransaction),this.reconcileTransaction\x3dnull},perform:function(e,t,n){return d.Mixin.perform.call(this,this.reconcileTransaction.perform,this.reconcileTransaction,e,t,n)}}),l.addPoolingTo(r);var M\x3dp.measure("ReactUpdates","flushBatchedUpdates",function(){for(;m.length||g;){if(m.length){var e\x3dr.getPooled();e.perform(i,null,e),r.release(e)}if(g){g\x3d!1;var t\x3dv;v\x3dc.getPooled(),t.notifyAll(),c.release(t)}}}),b\x3d{injectReconcileTransaction:function(e){h(e),O.ReactReconcileTransaction\x3de},injectBatchingStrategy:function(e){h(e),h("function"\x3d\x3dtypeof e.batchedUpdates),h("boolean"\x3d\x3dtypeof e.isBatchingUpdates),y\x3de}},O\x3d{ReactReconcileTransaction:null,batchedUpdates:o,enqueueUpdate:s,flushBatchedUpdates:M,injection:b,asap:u};t.exports\x3dO},{"./CallbackQueue":6,"./Object.assign":27,"./PooledClass":28,"./ReactCurrentOwner":36,"./ReactPerf":66,"./Transaction":93,"./invariant":124,"./warning":141}],78:[function(e,t){"use strict";var n\x3de("./DOMProperty"),r\x3dn.injection.MUST_USE_ATTRIBUTE,o\x3d{Properties:{cx:r,cy:r,d:r,dx:r,dy:r,fill:r,fillOpacity:r,fontFamily:r,fontSize:r,fx:r,fy:r,gradientTransform:r,gradientUnits:r,markerEnd:r,markerMid:r,markerStart:r,offset:r,opacity:r,patternContentUnits:r,patternUnits:r,points:r,preserveAspectRatio:r,r:r,rx:r,ry:r,spreadMethod:r,stopColor:r,stopOpacity:r,stroke:r,strokeDasharray:r,strokeLinecap:r,strokeOpacity:r,strokeWidth:r,textAnchor:r,transform:r,version:r,viewBox:r,x1:r,x2:r,x:r,y1:r,y2:r,y:r},DOMAttributeNames:{fillOpacity:"fill-opacity",fontFamily:"font-family",fontSize:"font-size",gradientTransform:"gradientTransform",gradientUnits:"gradientUnits",markerEnd:"marker-end",markerMid:"marker-mid",markerStart:"marker-start",patternContentUnits:"patternContentUnits",patternUnits:"patternUnits",preserveAspectRatio:"preserveAspectRatio",spreadMethod:"spreadMethod",stopColor:"stop-color",stopOpacity:"stop-opacity",strokeDasharray:"stroke-dasharray",strokeLinecap:"stroke-linecap",strokeOpacity:"stroke-opacity",strokeWidth:"stroke-width",textAnchor:"text-anchor",viewBox:"viewBox"}};t.exports\x3do},{"./DOMProperty":11}],79:[function(e,t){"use strict";function n(e){if("selectionStart"in e\x26\x26i.hasSelectionCapabilities(e))return{start:e.selectionStart,end:e.selectionEnd};if(window.getSelection){var t\x3dwindow.getSelection();return{anchorNode:t.anchorNode,anchorOffset:t.anchorOffset,focusNode:t.focusNode,focusOffset:t.focusOffset}}if(document.selection){var n\x3ddocument.selection.createRange();return{parentElement:n.parentElement(),text:n.text,top:n.boundingTop,left:n.boundingLeft}}}function r(e){if(!g\x26\x26null!\x3dh\x26\x26h\x3d\x3du()){var t\x3dn(h);if(!v||!p(v,t)){v\x3dt;var r\x3ds.getPooled(f.select,m,e);return r.type\x3d"select",r.target\x3dh,a.accumulateTwoPhaseDispatches(r),r}}}var o\x3de("./EventConstants"),a\x3de("./EventPropagators"),i\x3de("./ReactInputSelection"),s\x3de("./SyntheticEvent"),u\x3de("./getActiveElement"),c\x3de("./isTextInputElement"),l\x3de("./keyOf"),p\x3de("./shallowEqual"),d\x3do.topLevelTypes,f\x3d{select:{phasedRegistrationNames:{bubbled:l({onSelect:null}),captured:l({onSelectCapture:null})},dependencies:[d.topBlur,d.topContextMenu,d.topFocus,d.topKeyDown,d.topMouseDown,d.topMouseUp,d.topSelectionChange]}},h\x3dnull,m\x3dnull,v\x3dnull,g\x3d!1,y\x3d{eventTypes:f,extractEvents:function(e,t,n,o){switch(e){case d.topFocus:(c(t)||"true"\x3d\x3d\x3dt.contentEditable)\x26\x26(h\x3dt,m\x3dn,v\x3dnull);break;case d.topBlur:h\x3dnull,m\x3dnull,v\x3dnull;break;case d.topMouseDown:g\x3d!0;break;case d.topContextMenu:case d.topMouseUp:return g\x3d!1,r(o);case d.topSelectionChange:case d.topKeyDown:case d.topKeyUp:return r(o)}}};t.exports\x3dy},{"./EventConstants":16,"./EventPropagators":21,"./ReactInputSelection":57,"./SyntheticEvent":85,"./getActiveElement":111,"./isTextInputElement":127,"./keyOf":131,"./shallowEqual":137}],80:[function(e,t){"use strict";var n\x3dMath.pow(2,53),r\x3d{createReactRootIndex:function(){return Math.ceil(Math.random()*n)}};t.exports\x3dr},{}],81:[function(e,t){"use strict";var n\x3de("./EventConstants"),r\x3de("./EventPluginUtils"),o\x3de("./EventPropagators"),a\x3de("./SyntheticClipboardEvent"),i\x3de("./SyntheticEvent"),s\x3de("./SyntheticFocusEvent"),u\x3de("./SyntheticKeyboardEvent"),c\x3de("./SyntheticMouseEvent"),l\x3de("./SyntheticDragEvent"),p\x3de("./SyntheticTouchEvent"),d\x3de("./SyntheticUIEvent"),f\x3de("./SyntheticWheelEvent"),h\x3de("./getEventCharCode"),m\x3de("./invariant"),v\x3de("./keyOf"),g\x3d(e("./warning"),n.topLevelTypes),y\x3d{blur:{phasedRegistrationNames:{bubbled:v({onBlur:!0}),captured:v({onBlurCapture:!0})}},click:{phasedRegistrationNames:{bubbled:v({onClick:!0}),captured:v({onClickCapture:!0})}},contextMenu:{phasedRegistrationNames:{bubbled:v({onContextMenu:!0}),captured:v({onContextMenuCapture:!0})}},copy:{phasedRegistrationNames:{bubbled:v({onCopy:!0}),captured:v({onCopyCapture:!0})}},cut:{phasedRegistrationNames:{bubbled:v({onCut:!0}),captured:v({onCutCapture:!0})}},doubleClick:{phasedRegistrationNames:{bubbled:v({onDoubleClick:!0}),captured:v({onDoubleClickCapture:!0})}},drag:{phasedRegistrationNames:{bubbled:v({onDrag:!0}),captured:v({onDragCapture:!0})}},dragEnd:{phasedRegistrationNames:{bubbled:v({onDragEnd:!0}),captured:v({onDragEndCapture:!0})}},dragEnter:{phasedRegistrationNames:{bubbled:v({onDragEnter:!0}),captured:v({onDragEnterCapture:!0})}},dragExit:{phasedRegistrationNames:{bubbled:v({onDragExit:!0}),captured:v({onDragExitCapture:!0})}},dragLeave:{phasedRegistrationNames:{bubbled:v({onDragLeave:!0}),captured:v({onDragLeaveCapture:!0})}},dragOver:{phasedRegistrationNames:{bubbled:v({onDragOver:!0}),captured:v({onDragOverCapture:!0})}},dragStart:{phasedRegistrationNames:{bubbled:v({onDragStart:!0}),captured:v({onDragStartCapture:!0})}},drop:{phasedRegistrationNames:{bubbled:v({onDrop:!0}),captured:v({onDropCapture:!0})}},focus:{phasedRegistrationNames:{bubbled:v({onFocus:!0}),captured:v({onFocusCapture:!0})}},input:{phasedRegistrationNames:{bubbled:v({onInput:!0}),captured:v({onInputCapture:!0})}},keyDown:{phasedRegistrationNames:{bubbled:v({onKeyDown:!0}),captured:v({onKeyDownCapture:!0})}},keyPress:{phasedRegistrationNames:{bubbled:v({onKeyPress:!0}),captured:v({onKeyPressCapture:!0})}},keyUp:{phasedRegistrationNames:{bubbled:v({onKeyUp:!0}),captured:v({onKeyUpCapture:!0})}},load:{phasedRegistrationNames:{bubbled:v({onLoad:!0}),captured:v({onLoadCapture:!0})}},error:{phasedRegistrationNames:{bubbled:v({onError:!0}),captured:v({onErrorCapture:!0})}},mouseDown:{phasedRegistrationNames:{bubbled:v({onMouseDown:!0}),captured:v({onMouseDownCapture:!0})}},mouseMove:{phasedRegistrationNames:{bubbled:v({onMouseMove:!0}),captured:v({onMouseMoveCapture:!0})}},mouseOut:{phasedRegistrationNames:{bubbled:v({onMouseOut:!0}),captured:v({onMouseOutCapture:!0})}},mouseOver:{phasedRegistrationNames:{bubbled:v({onMouseOver:!0}),captured:v({onMouseOverCapture:!0})}},mouseUp:{phasedRegistrationNames:{bubbled:v({onMouseUp:!0}),captured:v({onMouseUpCapture:!0})}},paste:{phasedRegistrationNames:{bubbled:v({onPaste:!0}),captured:v({onPasteCapture:!0})}},reset:{phasedRegistrationNames:{bubbled:v({onReset:!0}),captured:v({onResetCapture:!0})}},scroll:{phasedRegistrationNames:{bubbled:v({onScroll:!0}),captured:v({onScrollCapture:!0})}},submit:{phasedRegistrationNames:{bubbled:v({onSubmit:!0}),captured:v({onSubmitCapture:!0})}},touchCancel:{phasedRegistrationNames:{bubbled:v({onTouchCancel:!0}),captured:v({onTouchCancelCapture:!0})}},touchEnd:{phasedRegistrationNames:{bubbled:v({onTouchEnd:!0}),captured:v({onTouchEndCapture:!0})}},touchMove:{phasedRegistrationNames:{bubbled:v({onTouchMove:!0}),captured:v({onTouchMoveCapture:!0})}},touchStart:{phasedRegistrationNames:{bubbled:v({onTouchStart:!0}),captured:v({onTouchStartCapture:!0})}},wheel:{phasedRegistrationNames:{bubbled:v({onWheel:!0}),captured:v({onWheelCapture:!0})}}},E\x3d{topBlur:y.blur,topClick:y.click,topContextMenu:y.contextMenu,topCopy:y.copy,topCut:y.cut,topDoubleClick:y.doubleClick,topDrag:y.drag,topDragEnd:y.dragEnd,topDragEnter:y.dragEnter,topDragExit:y.dragExit,topDragLeave:y.dragLeave,topDragOver:y.dragOver,topDragStart:y.dragStart,topDrop:y.drop,topError:y.error,topFocus:y.focus,topInput:y.input,topKeyDown:y.keyDown,topKeyPress:y.keyPress,topKeyUp:y.keyUp,topLoad:y.load,topMouseDown:y.mouseDown,topMouseMove:y.mouseMove,topMouseOut:y.mouseOut,topMouseOver:y.mouseOver,topMouseUp:y.mouseUp,topPaste:y.paste,topReset:y.reset,topScroll:y.scroll,topSubmit:y.submit,topTouchCancel:y.touchCancel,topTouchEnd:y.touchEnd,topTouchMove:y.touchMove,topTouchStart:y.touchStart,topWheel:y.wheel};for(var C in E)E[C].dependencies\x3d[C];var R\x3d{eventTypes:y,executeDispatch:function(e,t,n){var o\x3dr.executeDispatch(e,t,n);o\x3d\x3d\x3d!1\x26\x26(e.stopPropagation(),e.preventDefault())},extractEvents:function(e,t,n,r){var v\x3dE[e];if(!v)return null;var y;switch(e){case g.topInput:case g.topLoad:case g.topError:case g.topReset:case g.topSubmit:y\x3di;break;case g.topKeyPress:if(0\x3d\x3d\x3dh(r))return null;case g.topKeyDown:case g.topKeyUp:y\x3du;break;case g.topBlur:case g.topFocus:y\x3ds;break;case g.topClick:if(2\x3d\x3d\x3dr.button)return null;case g.topContextMenu:case g.topDoubleClick:case g.topMouseDown:case g.topMouseMove:case g.topMouseOut:case g.topMouseOver:case g.topMouseUp:y\x3dc;break;case g.topDrag:case g.topDragEnd:case g.topDragEnter:case g.topDragExit:case g.topDragLeave:case g.topDragOver:case g.topDragStart:case g.topDrop:y\x3dl;break;case g.topTouchCancel:case g.topTouchEnd:case g.topTouchMove:case g.topTouchStart:y\x3dp;break;case g.topScroll:y\x3dd;break;case g.topWheel:y\x3df;break;case g.topCopy:case g.topCut:case g.topPaste:y\x3da}m(y);var C\x3dy.getPooled(v,n,r);return o.accumulateTwoPhaseDispatches(C),C}};t.exports\x3dR},{"./EventConstants":16,"./EventPluginUtils":20,"./EventPropagators":21,"./SyntheticClipboardEvent":82,"./SyntheticDragEvent":84,"./SyntheticEvent":85,"./SyntheticFocusEvent":86,"./SyntheticKeyboardEvent":88,"./SyntheticMouseEvent":89,"./SyntheticTouchEvent":90,"./SyntheticUIEvent":91,"./SyntheticWheelEvent":92,"./getEventCharCode":112,"./invariant":124,"./keyOf":131,"./warning":141}],82:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticEvent"),o\x3d{clipboardData:function(e){return"clipboardData"in e?e.clipboardData:window.clipboardData}};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticEvent":85}],83:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticEvent"),o\x3d{data:null};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticEvent":85}],84:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticMouseEvent"),o\x3d{dataTransfer:null};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticMouseEvent":89}],85:[function(e,t){"use strict";function n(e,t,n){this.dispatchConfig\x3de,this.dispatchMarker\x3dt,this.nativeEvent\x3dn;var r\x3dthis.constructor.Interface;for(var o in r)if(r.hasOwnProperty(o)){var i\x3dr[o];this[o]\x3di?i(n):n[o]}var s\x3dnull!\x3dn.defaultPrevented?n.defaultPrevented:n.returnValue\x3d\x3d\x3d!1;this.isDefaultPrevented\x3ds?a.thatReturnsTrue:a.thatReturnsFalse,this.isPropagationStopped\x3da.thatReturnsFalse}var r\x3de("./PooledClass"),o\x3de("./Object.assign"),a\x3de("./emptyFunction"),i\x3de("./getEventTarget"),s\x3d{type:null,target:i,currentTarget:a.thatReturnsNull,eventPhase:null,bubbles:null,cancelable:null,timeStamp:function(e){return e.timeStamp||Date.now()},defaultPrevented:null,isTrusted:null};o(n.prototype,{preventDefault:function(){this.defaultPrevented\x3d!0;var e\x3dthis.nativeEvent;e.preventDefault?e.preventDefault():e.returnValue\x3d!1,this.isDefaultPrevented\x3da.thatReturnsTrue},stopPropagation:function(){var e\x3dthis.nativeEvent;e.stopPropagation?e.stopPropagation():e.cancelBubble\x3d!0,this.isPropagationStopped\x3da.thatReturnsTrue},persist:function(){this.isPersistent\x3da.thatReturnsTrue},isPersistent:a.thatReturnsFalse,destructor:function(){var e\x3dthis.constructor.Interface;for(var t in e)this[t]\x3dnull;this.dispatchConfig\x3dnull,this.dispatchMarker\x3dnull,this.nativeEvent\x3dnull}}),n.Interface\x3ds,n.augmentClass\x3dfunction(e,t){var n\x3dthis,a\x3dObject.create(n.prototype);o(a,e.prototype),e.prototype\x3da,e.prototype.constructor\x3de,e.Interface\x3do({},n.Interface,t),e.augmentClass\x3dn.augmentClass,r.addPoolingTo(e,r.threeArgumentPooler)},r.addPoolingTo(n,r.threeArgumentPooler),t.exports\x3dn},{"./Object.assign":27,"./PooledClass":28,"./emptyFunction":105,"./getEventTarget":115}],86:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticUIEvent"),o\x3d{relatedTarget:null};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticUIEvent":91}],87:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticEvent"),o\x3d{data:null};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticEvent":85}],88:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticUIEvent"),o\x3de("./getEventCharCode"),a\x3de("./getEventKey"),i\x3de("./getEventModifierState"),s\x3d{key:a,location:null,ctrlKey:null,shiftKey:null,altKey:null,metaKey:null,repeat:null,locale:null,getModifierState:i,charCode:function(e){return"keypress"\x3d\x3d\x3de.type?o(e):0},keyCode:function(e){return"keydown"\x3d\x3d\x3de.type||"keyup"\x3d\x3d\x3de.type?e.keyCode:0},which:function(e){return"keypress"\x3d\x3d\x3de.type?o(e):"keydown"\x3d\x3d\x3de.type||"keyup"\x3d\x3d\x3de.type?e.keyCode:0}};r.augmentClass(n,s),t.exports\x3dn},{"./SyntheticUIEvent":91,"./getEventCharCode":112,"./getEventKey":113,"./getEventModifierState":114}],89:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticUIEvent"),o\x3de("./ViewportMetrics"),a\x3de("./getEventModifierState"),i\x3d{screenX:null,screenY:null,clientX:null,clientY:null,ctrlKey:null,shiftKey:null,altKey:null,metaKey:null,getModifierState:a,button:function(e){var t\x3de.button;return"which"in e?t:2\x3d\x3d\x3dt?2:4\x3d\x3d\x3dt?1:0},buttons:null,relatedTarget:function(e){return e.relatedTarget||(e.fromElement\x3d\x3d\x3de.srcElement?e.toElement:e.fromElement)},pageX:function(e){return"pageX"in e?e.pageX:e.clientX+o.currentScrollLeft},pageY:function(e){return"pageY"in e?e.pageY:e.clientY+o.currentScrollTop}};r.augmentClass(n,i),t.exports\x3dn},{"./SyntheticUIEvent":91,"./ViewportMetrics":94,"./getEventModifierState":114}],90:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticUIEvent"),o\x3de("./getEventModifierState"),a\x3d{touches:null,targetTouches:null,changedTouches:null,altKey:null,metaKey:null,ctrlKey:null,shiftKey:null,getModifierState:o};r.augmentClass(n,a),t.exports\x3dn},{"./SyntheticUIEvent":91,"./getEventModifierState":114}],91:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticEvent"),o\x3de("./getEventTarget"),a\x3d{view:function(e){if(e.view)return e.view;var t\x3do(e);if(null!\x3dt\x26\x26t.window\x3d\x3d\x3dt)return t;var n\x3dt.ownerDocument;return n?n.defaultView||n.parentWindow:window},detail:function(e){return e.detail||0}};r.augmentClass(n,a),t.exports\x3dn},{"./SyntheticEvent":85,"./getEventTarget":115}],92:[function(e,t){"use strict";function n(e,t,n){r.call(this,e,t,n)}var r\x3de("./SyntheticMouseEvent"),o\x3d{deltaX:function(e){return"deltaX"in e?e.deltaX:"wheelDeltaX"in e?-e.wheelDeltaX:0},deltaY:function(e){return"deltaY"in e?e.deltaY:"wheelDeltaY"in e?-e.wheelDeltaY:"wheelDelta"in e?-e.wheelDelta:0},deltaZ:null,deltaMode:null};r.augmentClass(n,o),t.exports\x3dn},{"./SyntheticMouseEvent":89}],93:[function(e,t){"use strict";var n\x3de("./invariant"),r\x3d{reinitializeTransaction:function(){this.transactionWrappers\x3dthis.getTransactionWrappers(),this.wrapperInitData?this.wrapperInitData.length\x3d0:this.wrapperInitData\x3d[],this._isInTransaction\x3d!1},_isInTransaction:!1,getTransactionWrappers:null,isInTransaction:function(){return!!this._isInTransaction},perform:function(e,t,r,o,a,i,s,u){n(!this.isInTransaction());var c,l;try{this._isInTransaction\x3d!0,c\x3d!0,this.initializeAll(0),l\x3de.call(t,r,o,a,i,s,u),c\x3d!1}finally{try{if(c)try{this.closeAll(0)}catch(p){}else this.closeAll(0)}finally{this._isInTransaction\x3d!1}}return l},initializeAll:function(e){for(var t\x3dthis.transactionWrappers,n\x3de;n\x3ct.length;n++){var r\x3dt[n];try{this.wrapperInitData[n]\x3do.OBSERVED_ERROR,this.wrapperInitData[n]\x3dr.initialize?r.initialize.call(this):null}finally{if(this.wrapperInitData[n]\x3d\x3d\x3do.OBSERVED_ERROR)try{this.initializeAll(n+1)}catch(a){}}}},closeAll:function(e){n(this.isInTransaction());for(var t\x3dthis.transactionWrappers,r\x3de;r\x3ct.length;r++){var a,i\x3dt[r],s\x3dthis.wrapperInitData[r];try{a\x3d!0,s!\x3d\x3do.OBSERVED_ERROR\x26\x26i.close\x26\x26i.close.call(this,s),a\x3d!1}finally{if(a)try{this.closeAll(r+1)}catch(u){}}}this.wrapperInitData.length\x3d0}},o\x3d{Mixin:r,OBSERVED_ERROR:{}};t.exports\x3do},{"./invariant":124}],94:[function(e,t){"use strict";var n\x3de("./getUnboundedScrollPosition"),r\x3d{currentScrollLeft:0,currentScrollTop:0,refreshScrollValues:function(){var e\x3dn(window);r.currentScrollLeft\x3de.x,r.currentScrollTop\x3de.y}};t.exports\x3dr},{"./getUnboundedScrollPosition":120}],95:[function(e,t){"use strict";function n(e,t){if(r(null!\x3dt),null\x3d\x3de)return t;var n\x3dArray.isArray(e),o\x3dArray.isArray(t);return n\x26\x26o?(e.push.apply(e,t),e):n?(e.push(t),e):o?[e].concat(t):[e,t]}var r\x3de("./invariant");t.exports\x3dn},{"./invariant":124}],96:[function(e,t){"use strict";function n(e){for(var t\x3d1,n\x3d0,o\x3d0;o\x3ce.length;o++)t\x3d(t+e.charCodeAt(o))%r,n\x3d(n+t)%r;return t|n\x3c\x3c16}var r\x3d65521;t.exports\x3dn},{}],97:[function(e,t){function n(e){return e.replace(r,function(e,t){return t.toUpperCase()})}var r\x3d/-(.)/g;t.exports\x3dn},{}],98:[function(e,t){"use strict";function n(e){return r(e.replace(o,"ms-"))}var r\x3de("./camelize"),o\x3d/^-ms-/;t.exports\x3dn},{"./camelize":97}],99:[function(e,t){function n(e,t){return e\x26\x26t?e\x3d\x3d\x3dt?!0:r(e)?!1:r(t)?n(e,t.parentNode):e.contains?e.contains(t):e.compareDocumentPosition?!!(16\x26e.compareDocumentPosition(t)):!1:!1}var r\x3de("./isTextNode");t.exports\x3dn},{"./isTextNode":128}],100:[function(e,t){function n(e){return!!e\x26\x26("object"\x3d\x3dtypeof e||"function"\x3d\x3dtypeof e)\x26\x26"length"in e\x26\x26!("setInterval"in e)\x26\x26"number"!\x3dtypeof e.nodeType\x26\x26(Array.isArray(e)||"callee"in e||"item"in e)}function r(e){return n(e)?Array.isArray(e)?e.slice():o(e):[e]}var o\x3de("./toArray");t.exports\x3dr},{"./toArray":139}],101:[function(e,t){"use strict";function n(e){var t\x3do.createFactory(e),n\x3dr.createClass({displayName:"ReactFullPageComponent"+e,componentWillUnmount:function(){a(!1)},render:function(){return t(this.props)}});return n}var r\x3de("./ReactCompositeComponent"),o\x3de("./ReactElement"),a\x3de("./invariant");t.exports\x3dn},{"./ReactCompositeComponent":34,"./ReactElement":50,"./invariant":124}],102:[function(e,t){function n(e){var t\x3de.match(c);return t\x26\x26t[1].toLowerCase()}function r(e,t){var r\x3du;s(!!u);var o\x3dn(e),c\x3do\x26\x26i(o);if(c){r.innerHTML\x3dc[1]+e+c[2];for(var l\x3dc[0];l--;)r\x3dr.lastChild}else r.innerHTML\x3de;var p\x3dr.getElementsByTagName("script");p.length\x26\x26(s(t),a(p).forEach(t));for(var d\x3da(r.childNodes);r.lastChild;)r.removeChild(r.lastChild);return d}var o\x3de("./ExecutionEnvironment"),a\x3de("./createArrayFrom"),i\x3de("./getMarkupWrap"),s\x3de("./invariant"),u\x3do.canUseDOM?document.createElement("div"):null,c\x3d/^\\s*\x3c(\\w+)/;t.exports\x3dr},{"./ExecutionEnvironment":22,"./createArrayFrom":100,"./getMarkupWrap":116,"./invariant":124}],103:[function(e,t){"use strict";function n(e,t){var n\x3dnull\x3d\x3dt||"boolean"\x3d\x3dtypeof t||""\x3d\x3d\x3dt;if(n)return"";var r\x3disNaN(t);return r||0\x3d\x3d\x3dt||o.hasOwnProperty(e)\x26\x26o[e]?""+t:("string"\x3d\x3dtypeof t\x26\x26(t\x3dt.trim()),t+"px")}var r\x3de("./CSSProperty"),o\x3dr.isUnitlessNumber;t.exports\x3dn},{"./CSSProperty":4}],104:[function(e,t){function n(e,t,n,r,o){return o}e("./Object.assign"),e("./warning");t.exports\x3dn},{"./Object.assign":27,"./warning":141}],105:[function(e,t){function n(e){return function(){return e}}function r(){}r.thatReturns\x3dn,r.thatReturnsFalse\x3dn(!1),r.thatReturnsTrue\x3dn(!0),r.thatReturnsNull\x3dn(null),r.thatReturnsThis\x3dfunction(){return this},r.thatReturnsArgument\x3dfunction(e){return e},t.exports\x3dr},{}],106:[function(e,t){"use strict";var n\x3d{};t.exports\x3dn},{}],107:[function(e,t){"use strict";function n(e){return o[e]}function r(e){return(""+e).replace(a,n)}var o\x3d{"\x26":"\x26amp;","\x3e":"\x26gt;","\x3c":"\x26lt;",\'"\':"\x26quot;","\'":"\x26#x27;"},a\x3d/[\x26\x3e\x3c"\']/g;t.exports\x3dr},{}],108:[function(e,t){"use strict";function n(e,t,n){var r\x3de,a\x3d!r.hasOwnProperty(n);if(a\x26\x26null!\x3dt){var i,s\x3dtypeof t;i\x3d"string"\x3d\x3d\x3ds?o(t):"number"\x3d\x3d\x3ds?o(""+t):t,r[n]\x3di}}function r(e){if(null\x3d\x3de)return e;var t\x3d{};return a(e,n,t),t}{var o\x3de("./ReactTextComponent"),a\x3de("./traverseAllChildren");e("./warning")}t.exports\x3dr},{"./ReactTextComponent":76,"./traverseAllChildren":140,"./warning":141}],109:[function(e,t){"use strict";function n(e){try{e.focus()}catch(t){}}t.exports\x3dn},{}],110:[function(e,t){"use strict";var n\x3dfunction(e,t,n){Array.isArray(e)?e.forEach(t,n):e\x26\x26t.call(n,e)};t.exports\x3dn},{}],111:[function(e,t){function n(){try{return document.activeElement||document.body}catch(e){return document.body}}t.exports\x3dn},{}],112:[function(e,t){"use strict";function n(e){var t,n\x3de.keyCode;return"charCode"in e?(t\x3de.charCode,0\x3d\x3d\x3dt\x26\x2613\x3d\x3d\x3dn\x26\x26(t\x3d13)):t\x3dn,t\x3e\x3d32||13\x3d\x3d\x3dt?t:0}t.exports\x3dn},{}],113:[function(e,t){"use strict";function n(e){if(e.key){var t\x3do[e.key]||e.key;if("Unidentified"!\x3d\x3dt)return t}if("keypress"\x3d\x3d\x3de.type){var n\x3dr(e);return 13\x3d\x3d\x3dn?"Enter":String.fromCharCode(n)}return"keydown"\x3d\x3d\x3de.type||"keyup"\x3d\x3d\x3de.type?a[e.keyCode]||"Unidentified":""}var r\x3de("./getEventCharCode"),o\x3d{Esc:"Escape",Spacebar:" ",Left:"ArrowLeft",Up:"ArrowUp",Right:"ArrowRight",Down:"ArrowDown",Del:"Delete",Win:"OS",Menu:"ContextMenu",Apps:"ContextMenu",Scroll:"ScrollLock",MozPrintableKey:"Unidentified"},a\x3d{8:"Backspace",9:"Tab",12:"Clear",13:"Enter",16:"Shift",17:"Control",18:"Alt",19:"Pause",20:"CapsLock",27:"Escape",32:" ",33:"PageUp",34:"PageDown",35:"End",36:"Home",37:"ArrowLeft",38:"ArrowUp",39:"ArrowRight",40:"ArrowDown",45:"Insert",46:"Delete",112:"F1",113:"F2",114:"F3",115:"F4",116:"F5",117:"F6",118:"F7",119:"F8",120:"F9",121:"F10",122:"F11",123:"F12",144:"NumLock",145:"ScrollLock",224:"Meta"};t.exports\x3dn},{"./getEventCharCode":112}],114:[function(e,t){"use strict";function n(e){var t\x3dthis,n\x3dt.nativeEvent;if(n.getModifierState)return n.getModifierState(e);var r\x3do[e];return r?!!n[r]:!1}function r(){return n}var o\x3d{Alt:"altKey",Control:"ctrlKey",Meta:"metaKey",Shift:"shiftKey"};t.exports\x3dr},{}],115:[function(e,t){"use strict";function n(e){var t\x3de.target||e.srcElement||window;return 3\x3d\x3d\x3dt.nodeType?t.parentNode:t}t.exports\x3dn},{}],116:[function(e,t){function n(e){return o(!!a),p.hasOwnProperty(e)||(e\x3d"*"),i.hasOwnProperty(e)||(a.innerHTML\x3d"*"\x3d\x3d\x3de?"\x3clink /\x3e":"\x3c"+e+"\x3e\x3c/"+e+"\x3e",i[e]\x3d!a.firstChild),i[e]?p[e]:null}var r\x3de("./ExecutionEnvironment"),o\x3de("./invariant"),a\x3dr.canUseDOM?document.createElement("div"):null,i\x3d{circle:!0,defs:!0,ellipse:!0,g:!0,line:!0,linearGradient:!0,path:!0,polygon:!0,polyline:!0,radialGradient:!0,rect:!0,stop:!0,text:!0},s\x3d[1,\'\x3cselect multiple\x3d"true"\x3e\',"\x3c/select\x3e"],u\x3d[1,"\x3ctable\x3e","\x3c/table\x3e"],c\x3d[3,"\x3ctable\x3e\x3ctbody\x3e\x3ctr\x3e","\x3c/tr\x3e\x3c/tbody\x3e\x3c/table\x3e"],l\x3d[1,"\x3csvg\x3e","\x3c/svg\x3e"],p\x3d{"*":[1,"?\x3cdiv\x3e","\x3c/div\x3e"],area:[1,"\x3cmap\x3e","\x3c/map\x3e"],col:[2,"\x3ctable\x3e\x3ctbody\x3e\x3c/tbody\x3e\x3ccolgroup\x3e","\x3c/colgroup\x3e\x3c/table\x3e"],legend:[1,"\x3cfieldset\x3e","\x3c/fieldset\x3e"],param:[1,"\x3cobject\x3e","\x3c/object\x3e"],tr:[2,"\x3ctable\x3e\x3ctbody\x3e","\x3c/tbody\x3e\x3c/table\x3e"],optgroup:s,option:s,caption:u,colgroup:u,tbody:u,tfoot:u,thead:u,td:c,th:c,circle:l,defs:l,ellipse:l,g:l,line:l,linearGradient:l,path:l,polygon:l,polyline:l,radialGradient:l,rect:l,stop:l,text:l};t.exports\x3dn},{"./ExecutionEnvironment":22,"./invariant":124}],117:[function(e,t){"use strict";function n(e){for(;e\x26\x26e.firstChild;)e\x3de.firstChild;return e}function r(e){for(;e;){if(e.nextSibling)return e.nextSibling;e\x3de.parentNode}}function o(e,t){for(var o\x3dn(e),a\x3d0,i\x3d0;o;){if(3\x3d\x3do.nodeType){if(i\x3da+o.textContent.length,t\x3e\x3da\x26\x26i\x3e\x3dt)return{node:o,offset:t-a};a\x3di}o\x3dn(r(o))}}t.exports\x3do},{}],118:[function(e,t){"use strict";function n(e){return e?e.nodeType\x3d\x3d\x3dr?e.documentElement:e.firstChild:null}var r\x3d9;t.exports\x3dn},{}],119:[function(e,t){"use strict";function n(){return!o\x26\x26r.canUseDOM\x26\x26(o\x3d"textContent"in document.documentElement?"textContent":"innerText"),o}var r\x3de("./ExecutionEnvironment"),o\x3dnull;t.exports\x3dn},{"./ExecutionEnvironment":22}],120:[function(e,t){"use strict";function n(e){return e\x3d\x3d\x3dwindow?{x:window.pageXOffset||document.documentElement.scrollLeft,y:window.pageYOffset||document.documentElement.scrollTop}:{x:e.scrollLeft,y:e.scrollTop}}t.exports\x3dn},{}],121:[function(e,t){function n(e){return e.replace(r,"-$1").toLowerCase()}var r\x3d/([A-Z])/g;t.exports\x3dn},{}],122:[function(e,t){"use strict";function n(e){return r(e).replace(o,"-ms-")}var r\x3de("./hyphenate"),o\x3d/^ms-/;t.exports\x3dn},{"./hyphenate":121}],123:[function(e,t){"use strict";function n(e,t){var n;return n\x3d"string"\x3d\x3dtypeof e.type?r.createInstanceForTag(e.type,e.props,t):new e.type(e.props),n.construct(e),n}{var r\x3d(e("./warning"),e("./ReactElement"),e("./ReactLegacyElement"),e("./ReactNativeComponent"));e("./ReactEmptyComponent")}t.exports\x3dn},{"./ReactElement":50,"./ReactEmptyComponent":52,"./ReactLegacyElement":59,"./ReactNativeComponent":64,"./warning":141}],124:[function(e,t){"use strict";var n\x3dfunction(e,t,n,r,o,a,i,s){if(!e){var u;if(void 0\x3d\x3d\x3dt)u\x3dnew Error("Minified exception occurred; use the non-minified dev environment for the full error message and additional helpful warnings.");else{var c\x3d[n,r,o,a,i,s],l\x3d0;u\x3dnew Error("Invariant Violation: "+t.replace(/%s/g,function(){return c[l++]}))}throw u.framesToPop\x3d1,u}};t.exports\x3dn},{}],125:[function(e,t){"use strict";function n(e,t){if(!o.canUseDOM||t\x26\x26!("addEventListener"in document))return!1;var n\x3d"on"+e,a\x3dn in document;if(!a){var i\x3ddocument.createElement("div");i.setAttribute(n,"return;"),a\x3d"function"\x3d\x3dtypeof i[n]}return!a\x26\x26r\x26\x26"wheel"\x3d\x3d\x3de\x26\x26(a\x3ddocument.implementation.hasFeature("Events.wheel","3.0")),a}var r,o\x3de("./ExecutionEnvironment");o.canUseDOM\x26\x26(r\x3ddocument.implementation\x26\x26document.implementation.hasFeature\x26\x26document.implementation.hasFeature("","")!\x3d\x3d!0),t.exports\x3dn},{"./ExecutionEnvironment":22}],126:[function(e,t){function n(e){return!(!e||!("function"\x3d\x3dtypeof Node?e instanceof Node:"object"\x3d\x3dtypeof e\x26\x26"number"\x3d\x3dtypeof e.nodeType\x26\x26"string"\x3d\x3dtypeof e.nodeName))}t.exports\x3dn},{}],127:[function(e,t){"use strict";function n(e){return e\x26\x26("INPUT"\x3d\x3d\x3de.nodeName\x26\x26r[e.type]||"TEXTAREA"\x3d\x3d\x3de.nodeName)}var r\x3d{color:!0,date:!0,datetime:!0,"datetime-local":!0,email:!0,month:!0,number:!0,password:!0,range:!0,search:!0,tel:!0,text:!0,time:!0,url:!0,week:!0};t.exports\x3dn},{}],128:[function(e,t){function n(e){return r(e)\x26\x263\x3d\x3de.nodeType}var r\x3de("./isNode");t.exports\x3dn},{"./isNode":126}],129:[function(e,t){"use strict";function n(e){e||(e\x3d"");var t,n\x3darguments.length;if(n\x3e1)for(var r\x3d1;n\x3er;r++)t\x3darguments[r],t\x26\x26(e\x3d(e?e+" ":"")+t);return e}t.exports\x3dn},{}],130:[function(e,t){"use strict";var n\x3de("./invariant"),r\x3dfunction(e){var t,r\x3d{};n(e instanceof Object\x26\x26!Array.isArray(e));for(t in e)e.hasOwnProperty(t)\x26\x26(r[t]\x3dt);return r};t.exports\x3dr},{"./invariant":124}],131:[function(e,t){var n\x3dfunction(e){var t;for(t in e)if(e.hasOwnProperty(t))return t;return null};t.exports\x3dn},{}],132:[function(e,t){"use strict";function n(e,t,n){if(!e)return null;var o\x3d{};for(var a in e)r.call(e,a)\x26\x26(o[a]\x3dt.call(n,e[a],a,e));return o}var r\x3dObject.prototype.hasOwnProperty;t.exports\x3dn},{}],133:[function(e,t){"use strict";function n(e){var t\x3d{};return function(n){return t.hasOwnProperty(n)?t[n]:t[n]\x3de.call(this,n)}}t.exports\x3dn},{}],134:[function(e,t){"use strict";function n(e){r(e\x26\x26!/[^a-z0-9_]/.test(e))}var r\x3de("./invariant");t.exports\x3dn},{"./invariant":124}],135:[function(e,t){"use strict";function n(e){return o(r.isValidElement(e)),e}var r\x3de("./ReactElement"),o\x3de("./invariant");t.exports\x3dn},{"./ReactElement":50,"./invariant":124}],136:[function(e,t){"use strict";var n\x3de("./ExecutionEnvironment"),r\x3d/^[ \\r\\n\\t\\f]/,o\x3d/\x3c(!--|link|noscript|meta|script|style)[ \\r\\n\\t\\f\\/\x3e]/,a\x3dfunction(e,t){e.innerHTML\x3dt};if(n.canUseDOM){var i\x3ddocument.createElement("div");i.innerHTML\x3d" ",""\x3d\x3d\x3di.innerHTML\x26\x26(a\x3dfunction(e,t){if(e.parentNode\x26\x26e.parentNode.replaceChild(e,e),r.test(t)||"\x3c"\x3d\x3d\x3dt[0]\x26\x26o.test(t)){e.innerHTML\x3d""+t;var n\x3de.firstChild;1\x3d\x3d\x3dn.data.length?e.removeChild(n):n.deleteData(0,1)}else e.innerHTML\x3dt})}t.exports\x3da},{"./ExecutionEnvironment":22}],137:[function(e,t){"use strict";\nfunction n(e,t){if(e\x3d\x3d\x3dt)return!0;var n;for(n in e)if(e.hasOwnProperty(n)\x26\x26(!t.hasOwnProperty(n)||e[n]!\x3d\x3dt[n]))return!1;for(n in t)if(t.hasOwnProperty(n)\x26\x26!e.hasOwnProperty(n))return!1;return!0}t.exports\x3dn},{}],138:[function(e,t){"use strict";function n(e,t){return e\x26\x26t\x26\x26e.type\x3d\x3d\x3dt.type\x26\x26e.key\x3d\x3d\x3dt.key\x26\x26e._owner\x3d\x3d\x3dt._owner?!0:!1}t.exports\x3dn},{}],139:[function(e,t){function n(e){var t\x3de.length;if(r(!Array.isArray(e)\x26\x26("object"\x3d\x3dtypeof e||"function"\x3d\x3dtypeof e)),r("number"\x3d\x3dtypeof t),r(0\x3d\x3d\x3dt||t-1 in e),e.hasOwnProperty)try{return Array.prototype.slice.call(e)}catch(n){}for(var o\x3dArray(t),a\x3d0;t\x3ea;a++)o[a]\x3de[a];return o}var r\x3de("./invariant");t.exports\x3dn},{"./invariant":124}],140:[function(e,t){"use strict";function n(e){return d[e]}function r(e,t){return e\x26\x26null!\x3de.key?a(e.key):t.toString(36)}function o(e){return(""+e).replace(f,n)}function a(e){return"$"+o(e)}function i(e,t,n){return null\x3d\x3de?0:h(e,"",0,t,n)}var s\x3de("./ReactElement"),u\x3de("./ReactInstanceHandles"),c\x3de("./invariant"),l\x3du.SEPARATOR,p\x3d":",d\x3d{"\x3d":"\x3d0",".":"\x3d1",":":"\x3d2"},f\x3d/[\x3d.:]/g,h\x3dfunction(e,t,n,o,i){var u,d,f\x3d0;if(Array.isArray(e))for(var m\x3d0;m\x3ce.length;m++){var v\x3de[m];u\x3dt+(t?p:l)+r(v,m),d\x3dn+f,f+\x3dh(v,u,d,o,i)}else{var g\x3dtypeof e,y\x3d""\x3d\x3d\x3dt,E\x3dy?l+r(e,0):t;if(null\x3d\x3de||"boolean"\x3d\x3d\x3dg)o(i,null,E,n),f\x3d1;else if("string"\x3d\x3d\x3dg||"number"\x3d\x3d\x3dg||s.isValidElement(e))o(i,e,E,n),f\x3d1;else if("object"\x3d\x3d\x3dg){c(!e||1!\x3d\x3de.nodeType);for(var C in e)e.hasOwnProperty(C)\x26\x26(u\x3dt+(t?p:l)+a(C)+p+r(e[C],0),d\x3dn+f,f+\x3dh(e[C],u,d,o,i))}}return f};t.exports\x3di},{"./ReactElement":50,"./ReactInstanceHandles":58,"./invariant":124}],141:[function(e,t){"use strict";var n\x3de("./emptyFunction"),r\x3dn;t.exports\x3dr},{"./emptyFunction":105}]},{},[1])(1)});; \n\n                      if (typeof React \x3d\x3d\x3d \'undefined\' \x26\x26 typeof console !\x3d \'undefined\') { console.log(\'React not defined in reagent/react.min.js\');\n                      }\n                      if (typeof global !\x3d \'undefined\') {\n                      global.React \x3d React;\n                      } else if (typeof window !\x3d \'undefined\') {\n                      window.React \x3d React;\n                      } })(); } \n\n                      //@ sourceURL\x3dreagent/react.min.js\n');
var ir = Em.b(null), jr = new l(null, 1, [Ok, "https://github.com/reagent-project/reagent"], null);
function kr() {
  return new T(null, 3, 5, U, [Li, jr, new T(null, 2, 5, U, [Qk, new l(null, 3, [Qi, new l(null, 4, [Sj, "absolute", Fh, 0, Wk, 0, vk, 0], null), Ch, "Fork me on GitHub", si, "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png"], null)], null)], null);
}
fp.c("index.html", function() {
  return new T(null, 1, 5, U, [$p], null);
}, "Reagent: Minimalistic React for ClojureScript");
fp.c("news/index.html", function() {
  return new T(null, 1, 5, U, [hr], null);
}, "Reagent news");
function lr() {
  return new T(null, 5, 5, U, [Ri, new T(null, 2, 5, U, [Qh, new T(null, 5, 5, U, [Bj, new T(null, 2, 5, U, [pk, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "index.html"], null), "Reagent:"], null)], null), new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "index.html"], null), "Intro"], null)], null), new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [gp, new l(null, 1, [Ok, "news/index.html"], null), "News"], null)], null), new T(null, 2, 5, U, [Vh, new T(null, 3, 5, U, [Sk, 
  jr, "GitHub"], null)], null)], null)], null), q(G.b ? G.b(ir) : G.call(null, ir)) ? new T(null, 1, 5, U, [G.b ? G.b(ir) : G.call(null, ir)], null) : null, new T(null, 1, 5, U, [cp], null), new T(null, 1, 5, U, [kr], null)], null);
}
;(function(a) {
  var b = md(a) ? O.a(ee, a) : a, c = M.a(b, Nk);
  Q.a ? Q.a(ir, c) : Q.call(null, ir, c);
  return xp(new l(null, 2, [yk, function() {
    return function() {
      return new T(null, 1, 5, U, [lr], null);
    };
  }(a, b, c), Ng, new T(null, 2, 5, U, ["site/public/css/main.css", "site/public/css/examples.css"], null)], null));
})(null);
Fm(function() {
  return "undefined" !== typeof runtests ? runtests.ge() : null;
});

})();
