import fs from "node:fs";
import path from "node:path";

const [target, ...sources] = process.argv.slice(2);

if (!target || sources.length === 0) {
  console.error("usage: node merge-json.mjs TARGET SOURCE...");
  process.exit(2);
}

function merge(left, right) {
  if (
    left && right &&
    typeof left === "object" && typeof right === "object" &&
    !Array.isArray(left) && !Array.isArray(right)
  ) {
    const result = { ...left };
    for (const [key, value] of Object.entries(right)) {
      result[key] = key in result ? merge(result[key], value) : value;
    }
    return result;
  }
  return right;
}

let result = {};
for (const source of sources) {
  result = merge(result, JSON.parse(fs.readFileSync(source, "utf8")));
}

fs.mkdirSync(path.dirname(target), { recursive: true });
const temporary = `${target}.tmp.${process.pid}`;
fs.writeFileSync(temporary, `${JSON.stringify(result, null, 2)}\n`);
fs.renameSync(temporary, target);
