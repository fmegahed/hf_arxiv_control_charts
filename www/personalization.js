/**
 * QE ArXiv Watch - Personalization Module
 *
 * Client-side bookmark storage using localStorage. Bookmarks cover all
 * research tracks and are keyed by base arXiv id (see bookmark_ids.js).
 */

const QEPersonalization = (function() {
  'use strict';

  const STORAGE_KEY = 'qe_arxiv_watch_v3';

  /**
   * Get storage data from localStorage. Versioned ids written by earlier
   * releases are converted to base ids the first time they are read.
   */
  function getStorage() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      if (raw) {
        const migrated = QEBookmarkIds.migrateStorage(JSON.parse(raw));
        if (migrated.changed) {
          localStorage.setItem(STORAGE_KEY, JSON.stringify(migrated.data));
        }
        return migrated.data;
      }
    } catch (e) {
      console.warn('QEPersonalization: Error reading localStorage', e);
    }
    return { bookmarks: [], version: QEBookmarkIds.STORAGE_VERSION };
  }

  /**
   * Save storage data to localStorage and sync to Shiny
   */
  function saveStorage(data) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(data));
    } catch (e) {
      console.warn('QEPersonalization: Error saving to localStorage', e);
    }
    syncToShiny(data);
  }

  /**
   * Sync bookmarks to Shiny for server-side access
   */
  function syncToShiny(data) {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('personalization_bookmarks', (data || getStorage()).bookmarks, {priority: 'event'});
    }
  }

  /**
   * Toggle bookmark status for a paper. Returns true if now bookmarked.
   */
  function toggleBookmark(paperId) {
    const id = QEBookmarkIds.baseId(paperId);
    const data = getStorage();
    const index = data.bookmarks.indexOf(id);
    if (index === -1) {
      data.bookmarks.push(id);
      showToast('Paper bookmarked', 'success');
    } else {
      data.bookmarks.splice(index, 1);
      showToast('Bookmark removed', 'info');
    }
    saveStorage(data);
    updateAllBookmarkIcons(data.bookmarks);
    return index === -1;
  }

  function isBookmarked(paperId) {
    return getStorage().bookmarks.indexOf(QEBookmarkIds.baseId(paperId)) !== -1;
  }

  function getBookmarks() {
    return getStorage().bookmarks;
  }

  /**
   * Update all bookmark buttons on the page to match current state
   */
  function updateAllBookmarkIcons(bookmarks) {
    const marked = bookmarks || getBookmarks();
    document.querySelectorAll('.bookmark-icon').forEach(function(icon) {
      const paperId = icon.getAttribute('data-paper-id');
      if (!paperId) return;
      const isMarked = marked.indexOf(QEBookmarkIds.baseId(paperId)) !== -1;
      icon.classList.toggle('bookmarked', isMarked);
      icon.setAttribute('aria-pressed', isMarked ? 'true' : 'false');
      const label = isMarked ? 'Remove bookmark' : 'Bookmark this paper';
      icon.setAttribute('aria-label', label);
      icon.setAttribute('title', label);
      const glyph = icon.querySelector('.bookmark-glyph');
      if (glyph) glyph.textContent = isMarked ? '★' : '☆';
      const text = icon.querySelector('.bookmark-text');
      if (text) text.textContent = isMarked ? 'Bookmarked' : 'Bookmark';
    });
  }

  /**
   * Show toast notification
   */
  function showToast(message, type) {
    document.querySelectorAll('.toast-notification').forEach(function(el) { el.remove(); });
    const toast = document.createElement('div');
    toast.className = 'toast-notification toast-' + (type || 'info');
    toast.setAttribute('role', 'status');
    toast.appendChild(document.createTextNode(message));
    document.body.appendChild(toast);
    setTimeout(function() { toast.classList.add('show'); }, 10);
    setTimeout(function() {
      toast.classList.remove('show');
      setTimeout(function() { toast.remove(); }, 300);
    }, 2000);
  }

  function init() {
    $(document).on('shiny:connected', function() {
      syncToShiny();
      setTimeout(updateAllBookmarkIcons, 100);
    });

    // Bookmark buttons anywhere on the page
    document.addEventListener('click', function(e) {
      const icon = e.target.closest ? e.target.closest('.bookmark-icon') : null;
      if (!icon) return;
      e.preventDefault();
      toggleBookmark(icon.getAttribute('data-paper-id'));
    });

    // Buttons are redrawn by tables and by the paper page
    $(document).on('draw.dt shiny:value', function() {
      setTimeout(updateAllBookmarkIcons, 50);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  return {
    toggleBookmark: toggleBookmark,
    isBookmarked: isBookmarked,
    getBookmarks: getBookmarks,
    updateAllBookmarkIcons: updateAllBookmarkIcons,
    showToast: showToast,
    syncToShiny: syncToShiny
  };

})();
