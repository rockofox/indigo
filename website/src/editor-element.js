import { LitElement, html, css } from 'lit';
import { basicSetup, EditorView } from "codemirror"
import { keymap } from "@codemirror/view"
import { Compartment, EditorState } from "@codemirror/state"
import { indentWithTab } from "@codemirror/commands"
import { indigo } from "codemirror-lang-indigo"
import { oneDark } from "@codemirror/theme-one-dark"

export class CodeEditorElement extends LitElement {
    static properties = {
        code: { type: String },
        readOnly: { type: Boolean }
    };

    constructor() {
        super();
        this.code = "";
        this.readOnly = false;
    }

    static styles = css`
    :host {
      display: block;
      height: 100%;
      overflow: hidden;
    }
    .cm-editor {
      height: 100%;
    }
  `;

    firstUpdated() {
        const languageConf = new Compartment();

        this.editorView = new EditorView({
            doc: this.code,
            extensions: [
                basicSetup,
                keymap.of([indentWithTab]),
                languageConf.of(indigo()),
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
                EditorState.readOnly.of(this.readOnly)
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
    }

    render() {
        return html``;
    }
}

customElements.define('code-editor', CodeEditorElement);
