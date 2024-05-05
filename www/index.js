import * as wasm from "lugha";

const editor = document.getElementById("code-editor");
const output = document.getElementById("code-output");
const runner = document.getElementById("btn-runcode");
const ast = document.getElementById("ast");

const keywords = new Set(wasm.get_keywords());

function recurseText(element, result) {
  if (element.nodeName === "#text") {
    element.textContent
      .split(" ")
      .filter((w) => w.length > 0)
      .map((word) => {
        if (keywords.has(word)) {
          return `<b>${word}</b>`;
        } else {
          return word;
        }
      })
      .forEach((el) => result.push(el));
  }

  if (element.childNodes) {
    element.childNodes.forEach((child) => {
      recurseText(child, result);
      if (child.nodeName === "DIV") {
        result.push("\n");
      }
    });
  }
}

// Very ugly but quick and dirty and avoid stuff such as treesitter
function syntaxColorAndAST() {
  // TODO: color syntax
  try {
    ast.innerHTML = wasm.get_ast(editor.value);
  } catch (error) {
    ast.innerHTML = `<span class="error">Parsing error: ${error}</span>`;
  }
}

function runcode() {
  try {
    output.innerHTML = wasm.run(editor.value);
  } catch (error) {
    output.innerHTML = `<span class="error">Evaluation error: ${error}</span>`;
  }
}

editor.addEventListener("input", syntaxColorAndAST);
runner.addEventListener("click", runcode);
syntaxColorAndAST();
