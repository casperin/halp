let unwrapOr = (default, opt) =>
    switch(opt) {
    | Some(x) => x
    | None => default
    };

let isSome = opt =>
    switch(opt) {
    | Some(_) => true
    | None => false
    };

let isNone = opt =>
    !isSome(opt);

    let map = (fn, opt) =>
    switch(opt) {
        | Some(x) => Some(fn(x))
        | None => None
        };

let mapOr = (default, fn, opt) =>
    switch(opt) {
    | Some(x) => fn(x)
    | None => default
    };

let next = (opt1, opt2) =>
    isSome(opt1) ? opt2 : None;

let prev = (opt1, opt2) =>
    isSome(opt2) ? opt1 : None;

let or_ = (opt1, opt2) =>
    isSome(opt1) ? opt1 : opt2;

let orLazy = (fn, opt) =>
    isSome(opt) ? opt : fn();

let filter = (pred, opt) =>
    switch(opt) {
    | Some(x) => pred(x) ? opt : None
    | None => None
    };

let values = opts => {
    let step = (opt, xs) =>
        switch(opt) {
        | None => xs
        | Some(x) => [x, ...xs]
        };

    List.fold_right(step, opts, [])
};

let join = opt =>
    switch(opt) {
    | Some(x) => x
    | None => None
    };

let andMap = (optFn, opt) =>
    switch(optFn) {
    | Some(fn) => map(fn, opt)
    | None => None
    };

let toList = opt =>
    switch(opt) {
    | Some(x) => [x]
    | None => []
    };

let traverse = (fn, xs) => {
    let step = (x, acc) =>
        switch(fn(x)) {
        | Some(v) => map(acc => [v, ...acc], acc)
        | None => None
        };

    List.fold_right(step, xs, Some([]));
};

let combine = xs =>
    traverse(x => x, xs);
