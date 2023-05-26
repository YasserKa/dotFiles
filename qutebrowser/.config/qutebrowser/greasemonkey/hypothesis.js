// ==UserScript==
// @name            hypothesis
// @author          Yasser Kaddoura
// @description     Embed hypothes.is in the browser https://web.hypothes.is/help/embedding-hypothesis-in-websites-and-platforms/#embedding-hypothesis

// @grant    				none
// @noframes
// @license         Apache License V2
// @match           *://*/*
// ==/UserScript==

/**
 ,* Repo: https://github.com/tim-hub/Hypothesis-Assistant
 ,*/
(function () {
  var doc = document;

  // https://h.readthedocs.io/projects/client/en/latest/publishers/config.html#config-settings
  var hypothesisConfig = doc.createElement("script");
  hypothesisConfig.setAttribute("type", "application/json");
  hypothesisConfig.setAttribute("class", "js-hypothesis-config");
  // "theme": "clean" (don't show the sidebar)
  hypothesisConfig.innerText = `{
"showHighlights": false,
"theme": "clean"
}`;
  doc.body.appendChild(hypothesisConfig);

  var script = doc.createElement("script");
  script.setAttribute("src", "https://hypothes.is/embed.js");
  script.setAttribute("async", "");
  doc.body.appendChild(script);
  const element = document.querySelector("[data-component='AdderToolbar']");

  // Remove UI that pops up after selecting text
  function addGlobalStyle(css) {
    var head, style;
    head = document.getElementsByTagName("head")[0];
    if (!head) return;
    style = document.createElement("style");
    style.type = "text/css";
    style.innerHTML = css;
    head.appendChild(style);
  }
  css = "hypothesis-adder { display: none }";
  addGlobalStyle(css);
})();
