/**
 * Bookmark identifiers.
 *
 * Bookmarks are keyed by the base arXiv id ("2403.01234"), so a bookmark
 * survives a new version of the paper. Earlier releases stored versioned ids
 * ("2403.01234v2"); migrateStorage() converts them once.
 *
 * Loaded in the browser (window.QEBookmarkIds) and by the Node test in
 * tests/js/test-bookmark-ids.js.
 */
(function (root) {
  'use strict';

  var STORAGE_VERSION = 4;

  function baseId(id) {
    return String(id).trim().replace(/v[0-9]+$/, '');
  }

  // Returns { data, changed }. `data` always has unique base ids and the
  // current version number; the order of the bookmarks is kept.
  function migrateStorage(data) {
    var input = (data && Array.isArray(data.bookmarks)) ? data.bookmarks : [];
    var seen = {};
    var bookmarks = [];
    input.forEach(function (id) {
      if (id === null || id === undefined || String(id).trim() === '') return;
      var base = baseId(id);
      if (!seen[base]) {
        seen[base] = true;
        bookmarks.push(base);
      }
    });
    var changed = !data || data.version !== STORAGE_VERSION ||
      bookmarks.length !== input.length ||
      bookmarks.some(function (id, i) { return id !== input[i]; });
    return { data: { bookmarks: bookmarks, version: STORAGE_VERSION }, changed: changed };
  }

  var api = { STORAGE_VERSION: STORAGE_VERSION, baseId: baseId, migrateStorage: migrateStorage };
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = api;
  } else {
    root.QEBookmarkIds = api;
  }
})(typeof window !== 'undefined' ? window : this);
