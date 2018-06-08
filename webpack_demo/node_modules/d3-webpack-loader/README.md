# D3 Webpack Loader

[![npm version](https://badge.fury.io/js/d3-webpack-loader.svg)](https://badge.fury.io/js/d3-webpack-loader)


Automatically bundle D3 v4 modules under a single `d3` import with D3 Webpack Loader.

If you are using a subset of the modules provided by the default D3 build or are supplementing the default D3 build with additional modules, consider using D3 Webpack Loader to combine them under a single namespace. This lets you continue using D3 in your code in the familiar `d3.___` format.

### How do I use it?

After `npm install`ing a number of d3 modules, you can access them via `d3.___` in your code by importing `d3` with the loader. For instance, if we have installed d3-array, we can do:

```js
import d3 from 'd3!';

d3.sum([1, 2, 3, 4]); // === 10
```

**Important!** Don't forget to include the `!` at the end of the import. That tells webpack to use the `d3-webpack-loader`. If you prefer to be more verbose about it, you can also write the import as:

```js
import d3 from 'd3-webpack-loader!';
```

### Configuration

D3 Webpack Loader automatically figures out which D3 modules you have installed by scanning your module directories (e.g., `node_modules`) for packages that begin with `d3-` and combines them into a single object so you can access them under a single `d3` namespace.

If you'd like to exclude certain packages that begin with `d3-`, you can specify that in your webpack config under `d3Loader.exclude`:

```js
// webpack.config.js
module.exports = {
  ...
  d3Loader: {
    exclude: ['d3-dont-load-me', 'd3-i-mean-it']
  }
};
```

### How does it work?

It essentially does the equivalent of creating a local file that lists all your installed d3 modules and exports them as a single object. For instance, an equivalent manual set up would be if you had a local `d3-combined.js`:

```js
// d3-combined.js
import * as array from 'd3-array';
import * as scale from 'd3-scale';
import * as selection from 'd3-selection';

export default Object.assign({},
  array,
  scale,
  selection
);
```

And you used this in your code like:

```js
import d3 from './d3-combined';

d3.sum([1, 2, 3, 4]) // === 10
```

Doing it manually means you need to update your `d3-combined.js` file every time you install a new D3 module. Instead, you can just use D3 Webpack Loader and `import d3 from 'd3!'` and have it work automatically.


## Development

Code is written in old school ES5. No fancy new JS features. I don't know why I did this and I regret it. At least it is pretty brief.

### Testing

To run tests, you first need to install the test fixtures. Do this by running `npm run test:install`. Then you can run tests:

```
npm run test
```

### Linting

To lint files, run:

```
npm run lint
```

## License

MIT
