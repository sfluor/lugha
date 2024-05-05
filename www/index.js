import * as wasm from "lugha";

const editor = document.getElementById("code-editor");
const output = document.getElementById("code-output");
const runner = document.getElementById("btn-runcode");

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
function syntaxColor(event) {
  // TODO: color syntax
  // console.log(event);
  // const result = [];
  // recurseText(editor, result);
  // console.log("collected", result);
  // console.log(result.join(" "));
  // editor.innerHTML = result.join(" ");
  // var range = document.createRange();
  //     var sel = window.getSelection();
  // const selection = window.getSelection();
  // range.setStart(editor, editor.childNodes.length);
  // range.collapse(true);
  // selection.removeAllRanges();
  // selection.addRange(range);
}

function runcode() {
  console.log("running code", editor.value);
  output.textContent = wasm.run(editor.value);
}

editor.addEventListener("input", syntaxColor);
runner.addEventListener("click", runcode);

console.log(wasm.run("print 5+3;"));
console.log("test", keywords);
