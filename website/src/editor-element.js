import { LitElement, html, css } from 'lit';
import { basicSetup, EditorView } from "codemirror"
import { keymap } from "@codemirror/view"
import { Compartment, EditorState } from "@codemirror/state"
import { indentWithTab } from "@codemirror/commands"
import { indigo } from "codemirror-lang-indigo"
import { cpp } from "@codemirror/lang-cpp"
import { oneDark } from "@codemirror/theme-one-dark"

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
      overflow: hidden;
    }
    :host([auto-height]) {
      height: auto;
      overflow: visible;
    }
    .cm-editor {
      height: 100%;
    }
    :host([auto-height]) .cm-editor {
      height: auto;
    }
  `;

    firstUpdated() {
        const languageConf = new Compartment();
        this.readOnlyCompartment = new Compartment();

        let langExtension = indigo();
        if (this.language === 'c') {
            langExtension = cpp();
        }

        this.editorView = new EditorView({
            doc: this.code,
            extensions: [
                basicSetup,
                keymap.of([indentWithTab]),
                languageConf.of(langExtension),
                oneDark,
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
                    changes: { from: 0, to: currentDoc.length, insert: this.code }
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
