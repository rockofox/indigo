import { WASI, File, PreopenDirectory, Fd } from "@bjorn3/browser_wasi_shim";
import * as fflate from 'fflate';

const BUILD_TIME = import.meta.env.VITE_BUILD_TIME || Date.now();
const ZIP_URL = `https://storage.googleapis.com/indigolang/indigo-wasm-latest.zip?v=${BUILD_TIME}`;

class XTermStdio extends Fd {
    constructor(termElement) {
        super();
        this.termElement = termElement;
    }

    fd_write(data) {
        if (this.termElement) {
            this.termElement.write(data);
        }
        return { ret: 0, nwritten: data.byteLength };
    }

    fd_read(len) {
        return { ret: 0, data: new Uint8Array(0) };
    }

    fd_close() {
        return 0;
    }
}

export class IndigoRunner {
    constructor() {
        this.zipPromise = null;
        this.wasmBytes = null;
        this.preludeText = "";
        this.testText = "";
        this.isRunning = false;
    }

    async ensureLoaded(onStatus) {
        if (this.wasmBytes) return;

        if (!this.zipPromise) {
            this.zipPromise = (async () => {
                if (onStatus) onStatus({ status: 'fetching', message: 'Downloading Indigo runtime...' });
                const resp = await fetch(ZIP_URL);
                if (!resp.ok) {
                    throw new Error(`Failed to download runtime (${resp.status} ${resp.statusText})`);
                }
                const buf = new Uint8Array(await resp.arrayBuffer());
                if (onStatus) onStatus({ status: 'extracting', message: 'Initializing runtime...' });
                const files = fflate.unzipSync(buf);
                this.wasmBytes = files["indigo-wasm-latest/indigo-init.wasm"];
                this.preludeText = new TextDecoder("utf-8").decode(files["indigo-wasm-latest/std/prelude.in"]);
                this.testText = new TextDecoder("utf-8").decode(files["indigo-wasm-latest/std/test.in"]);
            })();
        }

        await this.zipPromise;
    }

    async run(code, termElement, options = {}) {
        const { stdin = "", onStatus } = options;
        if (this.isRunning) {
            return;
        }

        this.isRunning = true;
        const startTime = performance.now();

        try {
            await this.ensureLoaded(onStatus);

            if (termElement) {
                termElement.clear();
            }

            if (onStatus) onStatus({ status: 'compiling', message: 'Compiling & running...' });

            const wasi = new WASI([], [], [
                new XTermStdio(termElement),
                new XTermStdio(termElement),
                new XTermStdio(termElement),
                new PreopenDirectory("/usr/local/lib/indigo/std", new Map([
                    ["prelude.in", new File(new TextEncoder().encode(this.preludeText))],
                    ["test.in", new File(new TextEncoder().encode(this.testText))]
                ]))
            ]);

            const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
            const wasm = await WebAssembly.instantiate(this.wasmBytes, wasiImportObj);
            wasi.inst = wasm.instance;
            const exports = wasm.instance.exports;
            const memory = exports.memory;

            const encoder = new TextEncoder();
            const decoder = new TextDecoder();

            const progBytes = encoder.encode(code);
            const stdinBytes = encoder.encode(stdin);

            const progLen = progBytes.length;
            const progPtr = exports.mallocBytes(progLen);
            const progArr = new Uint8Array(memory.buffer, progPtr, progLen);
            progArr.set(progBytes);

            const stdinLen = stdinBytes.length;
            const stdinPtr = exports.mallocBytes(stdinLen);
            const stdinArr = new Uint8Array(memory.buffer, stdinPtr, stdinLen);
            stdinArr.set(stdinBytes);

            const outputPtrPtr = exports.mallocPtr();
            let outputPtr = null;

            try {
                const outputLen = exports.runProgramRawBuffered(progPtr, progLen, stdinPtr, stdinLen, outputPtrPtr);
                const outputPtrArr = new Uint32Array(memory.buffer, outputPtrPtr, 1);
                outputPtr = outputPtrArr[0];

                if (outputLen > 0 && outputPtr) {
                    const outputArr = new Uint8Array(memory.buffer, outputPtr, outputLen);
                    const output = decoder.decode(outputArr);
                    if (termElement) {
                        termElement.write(output);
                    }
                }
            } finally {
                if (outputPtr) exports.free_(outputPtr);
                if (outputPtrPtr) exports.free_(outputPtrPtr);
                if (progPtr) exports.free_(progPtr);
                if (stdinPtr) exports.free_(stdinPtr);
            }

            const elapsed = Math.round(performance.now() - startTime);
            if (onStatus) onStatus({ status: 'done', message: `Executed in ${elapsed}ms`, elapsed });

        } catch (e) {
            const isProcExit = e?.message?.includes('proc_exit') ||
                               e?.name === 'ProcExit' ||
                               e?.message?.includes('unreachable');

            const elapsed = Math.round(performance.now() - startTime);

            if (!isProcExit) {
                console.error("Playground execution error:", e);
                if (termElement) {
                    termElement.write(`\r\n\x1b[31mError: ${e.message || e}\x1b[0m\r\n`);
                }
                if (onStatus) onStatus({ status: 'error', message: `Error: ${e.message || e}`, elapsed });
            } else {
                if (onStatus) onStatus({ status: 'done', message: `Finished with errors in ${elapsed}ms`, elapsed });
            }
        } finally {
            this.isRunning = false;
        }
    }
}

export const runnerInstance = new IndigoRunner();

