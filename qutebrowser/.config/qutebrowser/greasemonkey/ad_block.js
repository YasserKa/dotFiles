// ==UserScript==
// @name         Auto Skip YouTube Ads
// @version      1.0.0
// @description  Speed up and skip YouTube ads automatically
// @author       jso8910
// @match        *://*.youtube.com/*
// @exclude      *://*.youtube.com/subscribe_embed?*
// ==/UserScript==
setInterval(() => {
  const btn = document.querySelector(
    ".videoAdUiSkipButton,.ytp-ad-skip-button",
  );
  if (btn) btn.click();

  const btn1 = document.querySelector(".ytp-ad-skip-button-modern.ytp-button");
  if (btn1) btn1.click();

  const ad = [...document.querySelectorAll(".ad-showing")][0];
  if (ad) {
    document.querySelector("video").playbackRate = 10;
    document.querySelector("video").muted = true;
  }
}, 50);
