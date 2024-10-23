// ==UserScript==
// @name         Auto Skip YouTube Ads
// @description  Finish YouTube ads automatically
// @match        *://*.youtube.com/*
// ==/UserScript==

(function() {
  function callback(mutationList, observer) {
    if (document.querySelector(".ytp-ad-player-overlay-layout")) {
      document.querySelector("video").currentTime = document.querySelector("video").duration;
    }
  }

  const observerOptions = {
    childList: true,
    subtree: true,
  };

  const targetNode = document.querySelector("ytd-app");
  const observer = new MutationObserver(callback);
  observer.observe(targetNode, observerOptions);
})();
