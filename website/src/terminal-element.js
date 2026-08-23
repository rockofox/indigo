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
          width: 100%;
          max-width: 100%;
          background: #191724;
          box-sizing: border-box;
          overflow: hidden;
        }
        #terminal {
          height: 100%;
          width: 100%;
          max-width: 100%;
          padding: 8px 12px;
          box-sizing: border-box;
        }
        .xterm {
          height: 100%;
          max-width: 100%;
        }
        .xterm-viewport {
          overflow-y: auto !important;
          -webkit-overflow-scrolling: touch;
        }
        @media (max-width: 600px) {
          #terminal {
            padding: 6px 8px;
          }
        }
      `
    ];

    connectedCallback() {
        super.connectedCallback();
        if (this.fitAddon && this.term) {
            requestAnimationFrame(() => this.fit());
        }
    }

    disconnectedCallback() {
        super.disconnectedCallback();
        if (this.resizeObserver) {
            this.resizeObserver.disconnect();
        }
    }

    firstUpdated() {
        this.term = new Terminal({
            theme: {
                background: '#191724',
                foreground: '#e0def4',
                cursor: '#ebbcba',
                cursorAccent: '#191724',
                selectionBackground: '#403d52',
                black: '#26233a',
                red: '#eb6f92',
                green: '#9ccfd8',
                yellow: '#f6c177',
                blue: '#31748f',
                magenta: '#c4a7e7',
                cyan: '#ebbcba',
                white: '#e0def4',
                brightBlack: '#6e6a86',
                brightRed: '#eb6f92',
                brightGreen: '#9ccfd8',
                brightYellow: '#f6c177',
                brightBlue: '#31748f',
                brightMagenta: '#c4a7e7',
                brightCyan: '#ebbcba',
                brightWhite: '#ffffff'
            },
            convertEol: true,
            fontFamily: "'JetBrains Mono', 'Fira Code', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace",
            fontSize: 13.5,
            lineHeight: 1.45,
            cursorBlink: true,
            scrollback: 5000,
            allowTransparency: true
        });

        this.fitAddon = new FitAddon();
        this.term.loadAddon(this.fitAddon);
        
        const container = this.renderRoot.querySelector('#terminal');
        this.term.open(container);

        this.resizeObserver = new ResizeObserver(() => {
            this.fit();
        });
        this.resizeObserver.observe(this);

        requestAnimationFrame(() => {
            this.fit();
            this.showWelcome();
        });

        this.term.onData(data => {
            this.dispatchEvent(new CustomEvent('term-data', {
                detail: data,
                bubbles: true,
                composed: true
            }));
        });
    }

    fit() {
        if (this.fitAddon && this.term && this.isConnected) {
            try {
                this.fitAddon.fit();
            } catch (_) {}
        }
    }

    showWelcome() {
        if (this.term) {
            this.term.write('\x1b[90mIndigo playground (WASI)\x1b[0m\r\n\x1b[90mPress Run or Ctrl+Enter to evaluate.\x1b[0m\r\n\r\n');
        }
    }

    write(data) {
        if (this.term) {
            this.term.write(data);
        }
    }

    clear() {
        if (this.term) {
            this.term.reset();
        }
    }

    render() {
        return html`<div id="terminal"></div>`;
    }
}

customElements.define('xterm-display', XTermDisplayElement);

