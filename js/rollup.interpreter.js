import nodeResolve from 'rollup-plugin-node-resolve';

export default {
	entry: 'lib/es6/src/engine/js/interpreter.js',
	plugins: [
		nodeResolve({
			jsnext: true
		}),
	],
	sourceMap: true,
	moduleName: 'statechart-interpreter',
	targets: [
		{ dest: 'js/lib/interpreter.js', format: 'cjs' },
		{ dest: 'js/src/interpreter.js', format: 'es' }
	]
};
