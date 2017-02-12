import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  entry: 'js/index.js',
  plugins: [
    nodeResolve({
      jsnext: true
    })
  ],
  sourceMap: true,
	exports: 'named',
  moduleName: 'statechart',
  targets: [
    { dest: 'lib/dist/statechart.js', format: 'cjs' },
  ]
};
