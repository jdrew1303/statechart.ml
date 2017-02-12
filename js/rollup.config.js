import nodeResolve from 'rollup-plugin-node-resolve';
import multiEntry from 'rollup-plugin-multi-entry';

export default {
  entry: [
    'lib/es6/src/lib/statechart.js',
    'lib/es6/src/engine/js/interpreter.js'
  ],
  plugins: [
    nodeResolve({
      jsnext: true
    }),
    multiEntry()
  ],
  sourceMap: true,
  moduleName: 'statechart',
  targets: [
    { dest: 'js/lib/index.js', format: 'cjs' },
    { dest: 'js/src/index.js', format: 'es' }
  ]
};
