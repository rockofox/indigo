import { LitElement, html, css } from 'lit';
import { basicSetup, EditorView } from "codemirror";
import { keymap } from "@codemirror/view";
import { Compartment, EditorState } from "@codemirror/state";
import { indentWithTab } from "@codemirror/commands";
import { indigo } from "codemirror-lang-indigo";
import { cpp } from "@codemirror/lang-cpp";
import { oneDark } from "@codemirror/theme-one-dark";

const editorTheme = EditorView.theme({
    "&": {
        height: "100%",
        fontSize: "13.5px",
        fontFamily: "'JetBrains Mono', 'Fira Code', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace",
        backgroundColor: "#1f1d2e",
        color: "#e0def4"
    },
    ".cm-content": {
        fontFamily: "'JetBrains Mono', 'Fira Code', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace",
        padding: "10px 0",
        lineHeight: "1.55",
        caretColor: "#ebbcba"
    },
    ".cm-cursor, .cm-dropCursor": {
        borderLeftColor: "#ebbcba"
    },
    "&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection": {
        backgroundColor: "#403d52 !important"
    },
    ".cm-gutters": {
        backgroundColor: "#191724",
        color: "#6e6a86",
        borderRight: "1px solid #26233a",
        paddingRight: "6px"
    },
    ".cm-activeLine": {
        backgroundColor: "#26233a55"
    },
    ".cm-activeLineGutter": {
        backgroundColor: "#26233a88",
        color: "#ebbcba"
    },
    ".cm-scroller": {
        overflow: "auto",
        fontFamily: "'JetBrains Mono', 'Fira Code', ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
    }
}, { dark: true });

export class CodeEditorElement extends LitElement {
    static properties = {
        code: { type: String },
        readOnly: { type: Boolean, attribute: 'read-only' },
        autoHeight: { type: Boolean, attribute: 'auto-height' },
        language: { type: String }
    };

    constructor() {
        super();
        this.code = "";
        this.readOnly = false;
        this.autoHeight = false;
        this.language = "indigo";
    }

    static styles = css`
    :host {
      display: block;
      height: 100%;
      width: 100%;
      overflow: hidden;
      box-sizing: border-box;
    }
    :host([auto-height]) {
      height: auto;
      overflow: visible;
    }
    .cm-editor {
      height: 100%;
      outline: none !important;
    }
    :host([auto-height]) .cm-editor {
      height: auto;
    }
    :host([auto-height]) .cm-scroller {
      overflow: visible;
    }
  `;

    firstUpdated() {
        const languageConf = new Compartment();
        this.readOnlyCompartment = new Compartment();

        let langExtension = indigo();
        if (this.language === 'c') {
            langExtension = cpp();
        }

        const runKeymap = keymap.of([
            {
                key: "Mod-Enter",
                run: () => {
                    this.dispatchEvent(new CustomEvent('run-code', {
                        bubbles: true,
                        composed: true
                    }));
                    return true;
                }
            },
            indentWithTab
        ]);

        this.editorView = new EditorView({
            doc: this.code,
            extensions: [
                basicSetup,
                runKeymap,
                languageConf.of(langExtension),
                oneDark,
                editorTheme,
                EditorView.updateListener.of((update) => {
                    if (update.docChanged) {
                        this.dispatchEvent(new CustomEvent('code-changed', {
                            detail: { code: update.state.doc.toString() },
                            bubbles: true,
                            composed: true
                        }));
                    }
                }),
                this.readOnlyCompartment.of(EditorState.readOnly.of(this.readOnly))
            ],
            parent: this.renderRoot
        });
    }

    updated(changedProperties) {
        if (changedProperties.has('code') && this.editorView) {
            const currentDoc = this.editorView.state.doc.toString();
            if (currentDoc !== this.code) {
                this.editorView.dispatch({
                    changes: { from: 0, to: currentDoc.length, insert: this.code || "" }
                });
            }
        }
        if (changedProperties.has('readOnly') && this.editorView) {
            this.editorView.dispatch({
                effects: this.readOnlyCompartment.reconfigure(EditorState.readOnly.of(this.readOnly))
            });
        }
    }

    render() {
        return html``;
    }
}

customElements.define('code-editor', CodeEditorElement);

