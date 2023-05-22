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
  // theme: clean (don't show the sidebar)
  hypothesisConfig.innerText = `{
"showHighlights": false
}`;
  doc.body.appendChild(hypothesisConfig);

  var script = doc.createElement("script");
  script.setAttribute("src", "https://hypothes.is/embed.js");
  script.setAttribute("async", "");
  doc.body.appendChild(script);
})();
