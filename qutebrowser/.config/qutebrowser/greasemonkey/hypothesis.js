// ==UserScript==
// @name            Hypothesis
// @author          Yasser Kaddoura
// @description     Embed hypothes.is in the browser https://web.hypothes.is/help/embedding-hypothesis-in-websites-and-platforms/#embedding-hypothesis

// @grant    				none
// @noframes
// @license         Apache License V2
// @match           *://*/*
// ==/UserScript==

(function () {
  var doc = document;

  // https://h.readthedocs.io/projects/client/en/latest/publishers/config.html#config-settings
  var hypothesisConfig = doc.createElement("script");
  hypothesisConfig.setAttribute("type", "application/json");
  hypothesisConfig.setAttribute("class", "js-hypothesis-config");
  // "theme": "clean" (don't show the sidebar)
  // Don't use that theme and hide them via JS, so qutebrowser can mimic the functionality by
  // clicking them.
  hypothesisConfig.innerText = `{
"showHighlights": false,
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
    console.log(style);
    head.appendChild(style);
  }
  css = "hypothesis-adder { display: none }";
  addGlobalStyle(css);

  function waitForElementToExist(selector) {
    return new Promise((resolve) => {
      if (document.querySelector(selector)) {
        return resolve(document.querySelector(selector));
      }

      const observer = new MutationObserver(() => {
        if (document.querySelector(selector)) {
          resolve(document.querySelector(selector));
          observer.disconnect();
        }
      });

      observer.observe(document.body, {
        subtree: true,
        childList: true,
      });
    });
  }

  waitForElementToExist("hypothesis-sidebar").then((element) => {
    // Remove the side bar
    function addStyleToSideBar(css) {
      var el, style;
      el = document.querySelector("hypothesis-sidebar").shadowRoot;
      if (!el) return;
      style = document.createElement("style");
      style.type = "text/css";
      style.innerHTML = css;
      console.log(style);
      el.appendChild(style);
    }

    css = "div.sidebar-container.sidebar-collapsed { display: none }";

    addStyleToSideBar(css);
  });
})();
