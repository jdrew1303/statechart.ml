import nodeResolve from 'rollup-plugin-node-resolve';

export default {
	// TODO
	entry: 'lib/es6/src/lib/statechart.js',
	plugins: [
		nodeResolve({
			jsnext: true
		}),
	],
	sourceMap: true,
	moduleName: 'statechart-compiler',
	targets: [
		{ dest: 'js/lib/compiler.js', format: 'cjs' },
		{ dest: 'js/src/compiler.js', format: 'es' }
	]
};
