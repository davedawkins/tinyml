import * as monaco from 'monaco-editor';
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker"
import jsonWorker from "monaco-editor/esm/vs/language/json/json.worker?worker"
import cssWorker from "monaco-editor/esm/vs/language/css/css.worker?worker"
import htmlWorker from "monaco-editor/esm/vs/language/html/html.worker?worker"
import tsWorker from "monaco-editor/esm/vs/language/typescript/ts.worker?worker"
//import dsWorker from "./ds.worker?worker"

console.log("Initializing MonacoEnvironment", self);

self.MonacoEnvironment = {

	// getWorkerUrl: function (moduleId, label) {
	// 	if (label === 'json') {
	// 		return './json.worker.bundle.js';
	// 	}
	// 	if (label === 'css' || label === 'scss' || label === 'less') {
	// 		return './css.worker.bundle.js';
	// 	}
	// 	if (label === 'html' || label === 'handlebars' || label === 'razor') {
	// 		return './html.worker.bundle.js';
	// 	}
	// 	if (label === 'typescript' || label === 'javascript') {
	// 		return './ts.worker.bundle.js';
	// 	}
	// 	return './editor.worker.bundle.js';
	// },

	getWorker: function (workerId, label) {
		// const getWorkerModule = (moduleUrl, label) => {
		// 	return new Worker(self.MonacoEnvironment.getWorkerUrl(moduleUrl), {
		// 		name: label,
		// 		type: 'module'
		// 	});
		// };

		console.log("label: ", label)

		switch (label) {
			case 'json':
				return new jsonWorker();
			case 'css':
			case 'scss':
			case 'less':
				return new cssWorker();
			case 'html':
			case 'handlebars':
			case 'razor':
				return htmlWorker();
			case 'typescript':
			case 'javascript':
				let w = new tsWorker();
				console.log("tsworker ctor", tsWorker);
				console.log("tsworker", w);
				return w;
			default:
				return new editorWorker();
		}
	}
};