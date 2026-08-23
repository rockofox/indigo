import './style.css';
import { Elm } from './Main.elm';
import './editor-element.js';
import './terminal-element.js';
import { runnerInstance } from './runner.js';

function encodeCode(code) {
    try {
        return encodeURIComponent(btoa(unescape(encodeURIComponent(code))));
    } catch (_) {
        return encodeURIComponent(code);
    }
}

function decodeCode(str) {
    try {
        return decodeURIComponent(escape(atob(decodeURIComponent(str))));
    } catch (_) {
        try {
            return decodeURIComponent(str);
        } catch (_) {
            return "";
        }
    }
}

function getInitialCodeFromUrl() {
    const hash = window.location.hash;
    if (hash.includes('code=')) {
        const idx = hash.indexOf('code=');
        return decodeCode(hash.slice(idx + 5));
    }
    const params = new URLSearchParams(window.location.search);
    const codeParam = params.get('code');
    if (codeParam) {
        return decodeCode(codeParam);
    }
    return "";
}

function showToast(message, type = 'info') {
    let container = document.querySelector('.toast-container');
    if (!container) {
        container = document.createElement('div');
        container.className = 'toast-container';
        document.body.appendChild(container);
    }
    const toast = document.createElement('div');
    toast.className = `toast-message toast-${type}`;
    toast.textContent = message;
    container.appendChild(toast);
    setTimeout(() => {
        toast.classList.add('toast-fade-out');
        setTimeout(() => toast.remove(), 300);
    }, 2400);
}

const initialSharedCode = getInitialCodeFromUrl();
const root = document.querySelector('#app');

const app = Elm.Main.init({
    node: root,
    flags: {
        initialCode: initialSharedCode
    }
});

async function getActiveTerminal() {
    let termElement = document.querySelector('xterm-display');
    if (termElement) return termElement;
    
    // Wait one animation frame if just rendered
    await new Promise(resolve => requestAnimationFrame(resolve));
    return document.querySelector('xterm-display');
}

// Ports wiring
if (app.ports.runCode) {
    app.ports.runCode.subscribe(async (code) => {
        const termElement = await getActiveTerminal();
        if (!termElement) {
            console.error("Active terminal element not found");
            return;
        }

        await runnerInstance.run(code, termElement, {
            onStatus: (statusObj) => {
                if (app.ports.statusChanged) {
                    app.ports.statusChanged.send(statusObj.message);
                }
            }
        });
    });
}

if (app.ports.copyToClipboard) {
    app.ports.copyToClipboard.subscribe(async (text) => {
        try {
            await navigator.clipboard.writeText(text);
            showToast("Copied to clipboard!");
        } catch (e) {
            console.error("Failed to copy:", e);
            showToast("Failed to copy to clipboard", "error");
        }
    });
}

if (app.ports.shareCode) {
    app.ports.shareCode.subscribe(async (code) => {
        try {
            const encoded = encodeCode(code);
            const url = new URL(window.location.href);
            url.pathname = '/playground';
            url.hash = `code=${encoded}`;
            window.history.replaceState({}, '', url.toString());

            await navigator.clipboard.writeText(url.toString());
            showToast("Shareable link copied to clipboard!");
        } catch (e) {
            console.error("Failed to share:", e);
            showToast("Failed to generate share link", "error");
        }
    });
}

if (app.ports.scrollToTop) {
    app.ports.scrollToTop.subscribe(() => {
        window.scrollTo({ top: 0, behavior: 'smooth' });
    });
}

// Listen for custom editor run-code event (Ctrl+Enter / Cmd+Enter)
window.addEventListener('run-code', () => {
    const runBtn = document.querySelector('.btn-run') || document.querySelector('.pane-quick-run');
    if (runBtn) {
        runBtn.click();
    }
});

