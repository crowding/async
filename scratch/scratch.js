// So here I am writing some Javascript.
// I installed js-comint mode for now.
// I also have to install node, which makes for a whole THING.
// What I want to figure out is, what do JS and Python use for a
// protocol for async generators.

// the question I want to answer is about 

array = [
    new Promise(res => setTimeout(res, 1000, 1)),
    new Promise(res => setTimeout(res, 2000, 2)),
    new Promise(res => setTimeout(res, 3000, 3)),
    new Promise(res => setTimeout(res, 4000, 4)),
    5,
    6,
    7,
    8,
]

// question: when you "yield" must you yield a promise?
ticks = async function* (n) {
    for (var i = 0; i < n; i++) {
        console.log("emitting tick " + i);
        if (x <= 1) var x = new Promise(res => setTimeout(res, 5000, i));
        else var x = new Promise(res => i);
        yield x;
    }
    console.log("finished ticking");
}

// what happens if we call "next" several times without resolving the promises?
(async () => {
    var t = ticks(3);
    var a = t.next(); console.log(a);
    var b = t.next(); console.log(b);
    var c = t.next(); console.log(c);
    var d = t.next(); console.log(d);
    console.log("start");
    0
    console.log(await(d));
    console.log(await(c));
    console.log(await(b));
    console.log(await(a));
    console.log("end");    
})();

// emitting tick 0
// Promise { <pending> }
// Promise { <pending> }
// Promise { <pending> }
// Promise { <pending> }
// start
// Promise { <pending> }
// > emitting tick 1
// emitting tick 2
// finished ticking
// { value: undefined, done: true }
// { value: 2, done: false }
// { value: 1, done: false }
// { value: 0, done: false }
// end

// so what I've learned is: you can extract an unlimited chain of
// Promises from an async generator but each one will wait for the one
// logically preceding it to be fully resolved, before being evaluated itself. 

// Execution of the promise-generator is blocked on each promise it emits.
// At the same time, the promises returned from "next" are queued up.
// Fascinating.

// Oh, I get it: the next() method on an async generator returns a
// promise registered to resolve _afer_ the prev. promise does! Like
// each returned promise implicitly has await(prev) wrapped around it
// or in front of it.

// Actually, hold on, is a JS promise just a single-shot Publish-Subscribe sort of thing?
// Like a Promise has the slots:
// 
// resolved: bool
// result: NULL until resolved
// _subscribers_: tasks to "do next" when resolved. Which may involve resolving
// other promises, etc.
