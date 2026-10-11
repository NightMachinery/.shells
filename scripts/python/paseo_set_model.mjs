#!/usr/bin/env node
// Set one Paseo agent's model through the installed Paseo CLI's own daemon
// client. `paseo agent update` has no model flag, but the daemon accepts a
// `set_agent_model_request`, which `DaemonClient.setAgentModel` sends.
// paseo_handoff.py runs this after an import; see docs/paseo.md.
//
//   node paseo_set_model.mjs --paseo <paseo executable> --home <paseo home> --check
//   node paseo_set_model.mjs --paseo <paseo executable> --home <paseo home> \
//       --agent-id <uuid> --model <model id>
//
// --check only connects and disconnects: a read-only smoke test.
//
// The daemon stores the model string without checking it, and a wrong one can
// break the conversation, so the caller must pass a model the source session
// really used. The shape check below only refuses garbage.

import { readFileSync, realpathSync, statSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { parseArgs } from "node:util";

const MODEL_RE = /^[A-Za-z0-9][A-Za-z0-9._:@-]{0,99}(?:\[[0-9]{1,4}[km]\])?$/;
const UUID_RE = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;
const ENTRY = "/@getpaseo/cli/bin/paseo";
const SHIM_ENTRY_RE = /"((?:\$basedir)?\/[^"\n]*\/@getpaseo\/cli\/bin\/paseo)"/g;

function fail(message, code = 1) {
    process.stderr.write("paseo-set-model: " + message + "\n", () => process.exit(code));
}

// Resolve the @getpaseo/cli package that the given `paseo` executable runs,
// at run time, so no version path is hard-coded. A pnpm shim names that entry
// relative to its own directory ($basedir), and the last one named is the one
// it executes; an npm install links to the entry directly.
function cliPackage(executable) {
    const real = realpathSync(executable);
    let entry = null;
    if (real.endsWith(ENTRY)) {
        entry = real;
    } else if (statSync(real).size <= 64 * 1024) {
        const named = [...readFileSync(real, "utf8").matchAll(SHIM_ENTRY_RE)].at(-1)?.[1];
        if (named) {
            entry = named.startsWith("$basedir/")
                ? path.join(path.dirname(real), named.slice("$basedir/".length))
                : named;
            entry = realpathSync(entry);
        }
    }
    if (!entry) {
        throw new Error("cannot find the @getpaseo/cli package behind the given paseo executable");
    }
    const directory = path.dirname(path.dirname(entry));
    const manifest = JSON.parse(readFileSync(path.join(directory, "package.json"), "utf8"));
    if (manifest.name !== "@getpaseo/cli") {
        throw new Error("the paseo executable does not run @getpaseo/cli");
    }
    return { directory, version: manifest.version ?? null };
}

async function cliModule(directory, file) {
    return import(pathToFileURL(path.join(directory, "dist", "utils", file)).href);
}

async function main() {
    let values;
    try {
        ({ values } = parseArgs({
            options: {
                paseo: { type: "string" },
                home: { type: "string" },
                "agent-id": { type: "string" },
                model: { type: "string" },
                check: { type: "boolean" },
            },
            strict: true,
            allowPositionals: false,
        }));
    } catch (error) {
        return fail(error.message, 64);
    }
    const check = values.check === true;
    if (!values.paseo || !values.home || !path.isAbsolute(values.home)) {
        return fail("--paseo and an absolute --home are required", 64);
    }
    if (check ? (values["agent-id"] || values.model)
        : !(UUID_RE.test(values["agent-id"] ?? "") && MODEL_RE.test(values.model ?? ""))) {
        return fail("use --check alone, or a UUID --agent-id with a well-formed --model", 64);
    }

    const cli = cliPackage(values.paseo);
    const { connectToDaemon } = await cliModule(cli.directory, "client.js");
    const { selectDaemonTarget } = await cliModule(cli.directory, "daemon-target.js");
    // The same target `paseo agent update --home <home>` connects to. An empty
    // environment keeps an inherited PASEO_HOST from choosing another daemon.
    const target = selectDaemonTarget({ home: values.home }, {}, false);
    const client = await connectToDaemon({ target });
    let result;
    try {
        if (check) {
            result = { ok: true, mode: "check", cliVersion: cli.version,
                       daemonVersion: client.getLastServerInfoMessage()?.version ?? null };
        } else {
            await client.setAgentModel(values["agent-id"], values.model);
            result = { ok: true, mode: "set", agentId: values["agent-id"], model: values.model };
        }
    } finally {
        await client.close().catch(() => {});
    }
    // Exit explicitly once the line is written, so a handle the client left
    // behind cannot keep this process alive until the caller's timeout.
    process.stdout.write(JSON.stringify(result) + "\n", () => process.exit(0));
}

main().catch((error) => {
    const message = error && typeof error === "object" && "message" in error ? error.message : String(error);
    fail(String(message));
});
