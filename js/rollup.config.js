import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  entry: 'js/index.js',
  external: [
    'parse5/lib/sax'
  ],
  plugins: [
    nodeResolve({
      jsnext: true
    })
  ],
  sourceMap: true,
	exports: 'named',
  moduleName: 'statechart',
  targets: [
    { dest: 'js/package/lib/index.js', format: 'cjs' },
    { dest: 'js/package/src/index.js', format: 'es' },
  ]
};
