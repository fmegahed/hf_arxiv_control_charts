/**
 * QE ArXiv Watch - Personalization Module
 *
 * Client-side bookmark storage and management using localStorage.
 * Works across all research tracks (SPC, DOE, Reliability).
 */

const QEPersonalization = (function() {
  'use strict';

  const STORAGE_KEY = 'qe_arxiv_watch_v3';

  /**
   * Get storage data from localStorage
   */
  function getStorage() {
    try {
      const data = localStorage.getItem(STORAGE_KEY);
      if (data) {
        return JSON.parse(data);
      }
    } catch (e) {
      console.warn('QEPersonalization: Error reading localStorage', e);
    }
    return { bookmarks: [], version: 3 };
  }

  /**
   * Save storage data to localStorage and sync to Shiny
   */
  function saveStorage(data) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(data));
      syncToShiny();
    } catch (e) {
      console.warn('QEPersonalization: Error saving to localStorage', e);
    }
  }

  /**
   * Sync bookmarks to Shiny for server-side access
   */
  function syncToShiny() {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      const data = getStorage();
      Shiny.setInputValue('personalization_bookmarks', data.bookmarks, {priority: 'event'});
    }
  }

  /**
   * Toggle bookmark status for a paper
   */
  function toggleBookmark(paperId) {
    const data = getStorage();
    const index = data.bookmarks.indexOf(paperId);

    if (index === -1) {
      // Add bookmark
      data.bookmarks.push(paperId);
      saveStorage(data);
      showToast('Paper bookmarked', 'success');
      updateBookmarkIcon(paperId, true);
    } else {
      // Remove bookmark
      data.bookmarks.splice(index, 1);
      saveStorage(data);
      showToast('Bookmark removed', 'info');
      updateBookmarkIcon(paperId, false);
    }

    return index === -1; // Returns true if bookmarked, false if removed
  }

  /**
   * Check if a paper is bookmarked
   */
  function isBookmarked(paperId) {
    const data = getStorage();
    return data.bookmarks.indexOf(paperId) !== -1;
  }

  /**
   * Get all bookmarked paper IDs
   */
  function getBookmarks() {
    const data = getStorage();
    return data.bookmarks || [];
  }

  /**
   * Update a single bookmark icon's visual state
   */
  function updateBookmarkIcon(paperId, bookmarked) {
    document.querySelectorAll('.bookmark-icon').forEach(function(icon) {
      if (icon.getAttribute('data-paper-id') === paperId) {
        icon.innerHTML = bookmarked ? '<i class="fa fa-star"></i>' : '<i class="fa fa-star-o"></i>';
        icon.className = bookmarked ? 'bookmark-icon bookmarked' : 'bookmark-icon';
      }
    });
  }

  /**
   * Update all bookmark icons on the page to match current state
   */
  function updateAllBookmarkIcons() {
    var bookmarks = getBookmarks();
    document.querySelectorAll('.bookmark-icon').forEach(function(icon) {
      var paperId = icon.getAttribute('data-paper-id');
      if (paperId) {
        var isMarked = bookmarks.indexOf(paperId) !== -1;
        icon.innerHTML = isMarked ? '<i class="fa fa-star"></i>' : '<i class="fa fa-star-o"></i>';
        icon.className = isMarked ? 'bookmark-icon bookmarked' : 'bookmark-icon';
      }
    });
  }

  /**
   * Show toast notification
   */
  function showToast(message, type) {
    // Remove existing toasts
    const existing = document.querySelectorAll('.toast-notification');
    existing.forEach(function(el) { el.remove(); });

    // Create toast element using safe DOM methods
    const toast = document.createElement('div');
    toast.className = 'toast-notification toast-' + (type || 'info');

    const iconEl = document.createElement('i');
    iconEl.className = 'fa fa-' + (type === 'success' ? 'check-circle' : 'info-circle');
    toast.appendChild(iconEl);

    const textNode = document.createTextNode(' ' + message);
    toast.appendChild(textNode);

    document.body.appendChild(toast);

    // Trigger animation
    setTimeout(function() {
      toast.classList.add('show');
    }, 10);

    // Auto-hide after 2 seconds
    setTimeout(function() {
      toast.classList.remove('show');
      setTimeout(function() {
        toast.remove();
      }, 300);
    }, 2000);
  }

  /**
   * Initialize on page load
   */
  function init() {
    // Initial sync to Shiny when it's ready
    // Always register the handler - jQuery will queue it even if Shiny isn't defined yet
    $(document).on('shiny:connected', function() {
      syncToShiny();
      setTimeout(updateAllBookmarkIcons, 100);
    });

    // Also run immediately if Shiny is already connected
    if (typeof Shiny !== 'undefined' && Shiny.shinyapp && Shiny.shinyapp.$socket) {
      syncToShiny();
      updateAllBookmarkIcons();
    }

    // Update icons after DataTables redraws
    $(document).on('draw.dt', function() {
      setTimeout(updateAllBookmarkIcons, 100);
    });

    // Handle dynamic content updates
    const observer = new MutationObserver(function(mutations) {
      let shouldUpdate = false;
      mutations.forEach(function(mutation) {
        if (mutation.addedNodes.length > 0) {
          mutation.addedNodes.forEach(function(node) {
            if (node.nodeType === 1 && (node.classList?.contains('bookmark-icon') || node.querySelector?.('.bookmark-icon'))) {
              shouldUpdate = true;
            }
          });
        }
      });
      if (shouldUpdate) {
        setTimeout(updateAllBookmarkIcons, 50);
      }
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  // Public API
  return {
    toggleBookmark: toggleBookmark,
    isBookmarked: isBookmarked,
    getBookmarks: getBookmarks,
    updateAllBookmarkIcons: updateAllBookmarkIcons,
    showToast: showToast,
    syncToShiny: syncToShiny
  };

})();

// RSS Tooltip positioning handler
document.addEventListener('mouseover', function(e) {
  if (e.target.closest('.rss-help-tooltip')) {
    const tooltip = e.target.closest('.rss-help-tooltip').querySelector('.tooltip-content');
    if (tooltip) {
      const rect = e.target.getBoundingClientRect();
      tooltip.style.top = (rect.bottom + 10) + 'px';
      tooltip.style.left = Math.min(rect.left, window.innerWidth - 320) + 'px';
    }
  }
});
