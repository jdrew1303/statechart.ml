import {parse as scparse} from '../lib/es6/src/lib/statechart.js';
import * as Interpreter_ from '../lib/es6/src/binding/js/interpreter.js';
import * as SCXML_ from '../lib/es6/src/scxml/statechart_scxml.js';
import {parse as ecmascript} from '../lib/es6/src/ecmascript/statechart_ecmascript.js';

export default Interpreter_;
export var Interpreter = Interpreter;
export var datamodels = {
  ecmascript: ecmascript
};
export var SCXML = SCXML_;

export function parse(document, dms) {
  if (!dms) dms = datamodels;
  // turn the object into an array of tuples
  return scparse(document, Object.keys(dms).map(function(key) {
    return [key, dms[key]];
  }));
}

export {validate, translate} from '../lib/es6/src/lib/statechart.js';
