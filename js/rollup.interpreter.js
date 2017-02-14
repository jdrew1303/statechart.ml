import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  entry: 'lib/es6/src/binding/bucklescript/interpreter.js',
  plugins: [
    nodeResolve({
      jsnext: true
    })
  ],
  sourceMap: true,
	exports: 'named',
  moduleName: 'interpreter',
  targets: [
    { dest: 'js/package/lib/interpreter.js', format: 'cjs' },
    { dest: 'js/package/src/interpreter.js', format: 'es' },
  ]
};
