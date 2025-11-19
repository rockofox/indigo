import './style.css';
import { Elm } from './Main.elm';
import './editor-element.js';
import './terminal-element.js';
import { IndigoRunner } from './runner.js';

const root = document.querySelector('#app');
const app = Elm.Main.init({ node: root });

let runner = null;

// Wait for elements to be in DOM
setTimeout(() => {
    const termElement = document.querySelector('xterm-display');
    if (termElement) {
        runner = new IndigoRunner(termElement);
    }
}, 500); // Small delay to ensure Elm has rendered

app.ports.runCode.subscribe(async (code) => {
    if (!runner) {
        const termElement = document.querySelector('xterm-display');
        if (termElement) {
            runner = new IndigoRunner(termElement);
        } else {
            console.error("Terminal element not found");
            return;
        }
    }
    try {
        await runner.run(code);
    } catch (e) {
        console.error(e);
        runner.termElement.write(`\r\nError: ${e.message}\r\n`);
    }
});
