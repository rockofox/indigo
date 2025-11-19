import { LitElement, html, css, unsafeCSS } from 'lit';
import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import xtermCss from "@xterm/xterm/css/xterm.css?inline";

export class XTermDisplayElement extends LitElement {
    static styles = [
        css`${unsafeCSS(xtermCss)}`,
        css`
        :host {
          display: block;
          height: 100%;
          background: #191724;
        }
        #terminal {
          height: 100%;
          padding: 10px;
        }
      `
    ];

    firstUpdated() {
        this.term = new Terminal({
            theme: {
                background: '#191724',
                foreground: '#f8f8f2',
                cursor: '#f8f8f0',
                selectionBackground: '#44475a',
            },
            convertEol: true,
            fontFamily: 'monospace'
        });

        this.fitAddon = new FitAddon();
        this.term.loadAddon(this.fitAddon);
        this.term.open(this.renderRoot.querySelector('#terminal'));
        this.fitAddon.fit();

        window.addEventListener('resize', () => this.fitAddon.fit());

        // Expose terminal instance for the runner
        this.term.onData(data => {
            this.dispatchEvent(new CustomEvent('term-data', {
                detail: data,
                bubbles: true,
                composed: true
            }));
        });
    }

    write(data) {
        if (this.term) {
            this.term.write(data);
        }
    }

    clear() {
        if (this.term) {
            // Use RIS (Reset to Initial State) escape sequence
            this.term.write('\x1bc');
        }
    }

    render() {
        return html`<div id="terminal"></div>`;
    }
}

customElements.define('xterm-display', XTermDisplayElement);
