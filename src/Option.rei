let unwrapOr: 'a => option('a) => 'a;
let isSome: option(_) => bool;
let isNone: option(_) => bool;
let map: ('a => 'b) => option('a) => option('b);
let mapOr: 'b => ('a => 'b) => option('a) => 'b;
let next: option('a) => option('b) => option('b);
let prev: option('a) => option('b) => option('a);
let or_: option('a) => option('a) => option('a);
let orLazy: ((unit) => option('a)) => option('a) => option('a);
let filter: ('a => bool) => option('a) => option('a);
let values: list(option('a)) => list('a);
let join: option(option('a)) => option('a);
let andMap: option(('a => 'b)) => option('a) => option('b);
let toList: option('a) => list('a);
let traverse: ('a => option('b)) => list('a) => option(list('b));
let combine: list(option('a)) => option(list('a));