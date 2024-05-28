// ==UserScript==
// @name         RelationFixer
// @namespace    https://myanimelist.net/profile/RobbiRobb
// @version      0.2
// @description  displays relations on anime and manga in a more classical layout because the official one sucks
// @author       RobbiRobb
// @match        https://myanimelist.net/anime/*
// @match        https://myanimelist.net/manga/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=myanimelist.net
// @grant        none
// @require      https://code.jquery.com/jquery-3.7.1.min.js
// @updateURL    https://pastebin.com/raw/MTcrX0uZ
// ==/UserScript==

(function() {
  "use strict";
  console.log("asdfsadfsa");

  function appendList(relations, listElement) {
    for (const key of Object.keys(relations).sort().reverse()) {
      const row = $("<tr></tr>");
      const typeTD = $(
        "<td class=\"ar fw-n borderClass nowrap\" valign=\"top\"></td>",
      ).text(key + ":");
      const listTD = $("<td class=\"borderClass\" width=\"100%\">");
      const entryList = $("<ul class=\"entries\"></ul>");

      for (const entry of relations[key]) {
        entryList.append($("<li></li>").html(entry.title.html() + entry.type));
      }

      listTD.append(entryList);
      row.append(typeTD);
      row.append(listTD);

      $(listElement).prepend(row);
    }
  }

  if ($(".entries-tile").length !== 0) {
    const relations = {};

    $(".entries-tile .entry").each((index, el) => {
      const type = $(el).find(".content .relation").text().trim();

      if (type == "") return;
      if (relations[type.split("\n")[0].trim()] == undefined) {
        relations[type.split("\n")[0].trim()] = [];
      }

      relations[type.split("\n")[0].trim()].push({
        title: $(el).find(".content .title"),
        type: type.split("\n")[1].trim(),
      });
    });

    if ($(".entries-table > tbody").length == 0) {
      const tbody = $("<tbody></tbody>");
      appendList(relations, tbody);
      const table = $("<table class=\"entries-table\"></table>").append(tbody);

      $(".related-entries").each((index, el) => {
        $(el).append(table);
      });
    } else {
      $(".entries-table > tbody").each((index, el) => {
        appendList(relations, el);
      });
    }
  }

  const style = $("<style></style>").text(`
		.related-entries .ar.mt4,
		.entries-tile {
			display: none !important;
		}
		
		.entries-table .hide-entry {
			display: table-row !important;
		}
	`);

  $(document.head).append(style);
})();
