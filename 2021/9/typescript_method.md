# TypeScript 4.4 changes how it calls a method when it’s imported separately

Imagine that you have a module `m` that exports an object directly.

m.ts
```
const m = {
    f() { console.log(this); }
};

export = m;
```

Then, you import it in various ways.

t.ts
```
import { f } from './m';
import m from './m';
import * as m2 from './m';

f();
m.f();
m2.f();
```

When you transpile this with TypeScript 4.3 and run `t.ts`, you'll get this.

```
{ f: [Function: f] }
{ f: [Function: f] }
{ f: [Getter], default: { f: [Function: f] } }
```

But when you transpile this with TypeScript 4.4, it prints this.

```
undefined
{ f: [Function: f] }
{ f: [Getter], default: { f: [Function: f] } }
```

This is because TypeScript 4.4 has changed how it calls a imported function as described [here](https://levelup.gitconnected.com/features-in-the-new-typescript-release-e5ef6ebba750#10e5).

> The new TS version discards the this value when calling imported functions for more compliance with ECMAScript modules’ specification on all available module systems (ESM, CommonJS, AMD, ...).

It's not very common to export an object directly in TypeScript, but it can happen when the module is written in JavaScript.
