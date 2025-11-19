import { WASI, File, PreopenDirectory, Fd } from "@bjorn3/browser_wasi_shim";
import * as fflate from 'fflate';

const ZIP_URL = "https://storage.googleapis.com/indigolang/indigo-wasm-latest.zip";

class XTermStdio extends Fd {
    constructor(term) {
        super();
        this.term = term;
    }

    fd_write(view8, iovs) {
        let nwritten = 0;
        for (let iovec of iovs) {
            let buffer = view8.slice(iovec.buf, iovec.buf + iovec.buf_len);
            this.term.write(buffer);
            nwritten += iovec.buf_len;
        }
        return { ret: 0, nwritten };
    }

    fd_read(view8, iovs) {
        // Don't actually read from terminal - just return 0 bytes read
        // This prevents cursor position queries from getting garbage responses
        return { ret: 0, nread: 0 };
    }

    fd_close() {
        return 0; // Success
    }
}

export class IndigoRunner {
    constructor(termElement) {
        this.termElement = termElement;
        this.zipFetched = false;
        this.wasmBytes = null;
        this.preludeText = "";
        this.testText = "";
    }

    async fetchAndUnzip() {
        if (this.zipFetched) return;
        const resp = await fetch(ZIP_URL);
        const buf = new Uint8Array(await resp.arrayBuffer());
        const files = fflate.unzipSync(buf);
        this.wasmBytes = files["indigo-wasm-latest/indigo-init.wasm"];
        this.preludeText = new TextDecoder("utf-8").decode(files["indigo-wasm-latest/std/prelude.in"]);
        this.testText = new TextDecoder("utf-8").decode(files["indigo-wasm-latest/std/test.in"]);
        this.zipFetched = true;
    }

    async run(code, stdin = "") {
        await this.fetchAndUnzip();
        this.termElement.clear();

        const wasi = new WASI([], [], [
            new XTermStdio(this.termElement),
            new XTermStdio(this.termElement),
            new XTermStdio(this.termElement),
            new PreopenDirectory("/usr/local/lib/indigo/std", new Map([
                ["prelude.in", new File(new TextEncoder("utf-8").encode(this.preludeText))],
                ["test.in", new File(new TextEncoder("utf-8").encode(this.testText))]
            ]))
        ]);

        const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
        const wasm = await WebAssembly.instantiate(this.wasmBytes, wasiImportObj);
        wasi.inst = wasm.instance;
        const exports = wasm.instance.exports;
        const memory = exports.memory;

        const encoder = new TextEncoder();
        const decoder = new TextDecoder();

        const outputPtrPtr = exports.mallocPtr();
        const progLen = code.length;
        const progPtr = exports.mallocBytes(progLen);
        const progArr = new Uint8Array(memory.buffer, progPtr, progLen);
        encoder.encodeInto(code, progArr);
        const stdinLen = stdin.length;
        const stdinPtr = exports.mallocBytes(stdinLen);
        const stdinArr = new Uint8Array(memory.buffer, stdinPtr, stdinLen);
        encoder.encodeInto(stdin, stdinArr);
        const outputLen = exports.runProgramRawBuffered(progPtr, progLen, stdinPtr, stdinLen, outputPtrPtr);
        const outputPtrArr = new Uint32Array(memory.buffer, outputPtrPtr, 1);
        const outputPtr = outputPtrArr[0];
        const outputArr = new Uint8Array(memory.buffer, outputPtr, outputLen);

        let output = decoder.decode(outputArr);

        // Strip ANSI escape codes and control characters
        output = output.replace(/\x1b\[[0-9;]*[a-zA-Z]/g, ''); // ANSI codes
        output = output.replace(/[\x00-\x08\x0B-\x0C\x0E-\x1F\x7F]/g, ''); // Control chars except \n, \r, \t

        this.termElement.write(output);
        exports.free_(outputPtr);
    }
}
