export * from '../lib/es6/src/binding/bucklescript/main.js';
import * as Interpreter_ from '../lib/es6/src/binding/bucklescript/interpreter.js';
export const Interpreter = Interpreter_
import * as SCXML_ from '../lib/es6/src/binding/bucklescript/scxml.js';
export const SCXML = SCXML_
// import {parse as ecmascript} from '../lib/es6/src/ecmascript/statechart_ecmascript.js';
import {parse as nulldm} from '../lib/es6/src/null/statechart_null.js';

export var datamodels = {
  // TODO
  // ecmascript: ecmascript
  'null': nulldm
};
