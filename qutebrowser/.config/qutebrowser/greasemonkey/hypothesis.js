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
  // https://h.readthedocs.io/projects/client/en/latest/publishers/config.html#config-settings
  window.hypothesisConfig = function () {
    return {
      showHighlights: false,
      openSidebar: true,
    };
  };
  var doc = document;
  var script = doc.createElement("script");
  script.setAttribute("src", "https://hypothes.is/embed.js");
  script.setAttribute("async", "");
  doc.body.appendChild(script);
})();
