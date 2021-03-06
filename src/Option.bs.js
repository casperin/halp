// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");

function unwrapOr($$default, opt) {
  if (opt !== undefined) {
    return Js_primitive.valFromOption(opt);
  } else {
    return $$default;
  }
}

function isSome(opt) {
  return opt !== undefined;
}

function isNone(opt) {
  return !isSome(opt);
}

function map(fn, opt) {
  if (opt !== undefined) {
    return Js_primitive.some(Curry._1(fn, Js_primitive.valFromOption(opt)));
  }
  
}

function mapOr($$default, fn, opt) {
  if (opt !== undefined) {
    return Curry._1(fn, Js_primitive.valFromOption(opt));
  } else {
    return $$default;
  }
}

function next(opt1, opt2) {
  var match = isSome(opt1);
  if (match) {
    return opt2;
  }
  
}

function prev(opt1, opt2) {
  var match = isSome(opt2);
  if (match) {
    return opt1;
  }
  
}

function or_(opt1, opt2) {
  var match = isSome(opt1);
  if (match) {
    return opt1;
  } else {
    return opt2;
  }
}

function orLazy(fn, opt) {
  var match = isSome(opt);
  if (match) {
    return opt;
  } else {
    return Curry._1(fn, /* () */0);
  }
}

function filter(pred, opt) {
  if (opt !== undefined) {
    var match = Curry._1(pred, Js_primitive.valFromOption(opt));
    if (match) {
      return opt;
    } else {
      return undefined;
    }
  }
  
}

function values(opts) {
  var step = function (opt, xs) {
    if (opt !== undefined) {
      return /* :: */[
              Js_primitive.valFromOption(opt),
              xs
            ];
    } else {
      return xs;
    }
  };
  return List.fold_right(step, opts, /* [] */0);
}

function join(opt) {
  if (opt !== undefined) {
    return Js_primitive.valFromOption(opt);
  }
  
}

function andMap(optFn, opt) {
  if (optFn !== undefined) {
    return map(optFn, opt);
  }
  
}

function toList(opt) {
  if (opt !== undefined) {
    return /* :: */[
            Js_primitive.valFromOption(opt),
            /* [] */0
          ];
  } else {
    return /* [] */0;
  }
}

function traverse(fn, xs) {
  var step = function (x, acc) {
    var match = Curry._1(fn, x);
    if (match !== undefined) {
      var v = Js_primitive.valFromOption(match);
      return map((function (acc) {
                    return /* :: */[
                            v,
                            acc
                          ];
                  }), acc);
    }
    
  };
  return List.fold_right(step, xs, /* [] */0);
}

function combine(xs) {
  return traverse((function (x) {
                return x;
              }), xs);
}

exports.unwrapOr = unwrapOr;
exports.isSome = isSome;
exports.isNone = isNone;
exports.map = map;
exports.mapOr = mapOr;
exports.next = next;
exports.prev = prev;
exports.or_ = or_;
exports.orLazy = orLazy;
exports.filter = filter;
exports.values = values;
exports.join = join;
exports.andMap = andMap;
exports.toList = toList;
exports.traverse = traverse;
exports.combine = combine;
/* No side effect */
