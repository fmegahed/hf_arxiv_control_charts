/**
 * QE ArXiv Watch - page behaviour
 *
 * - typesets math whenever new content appears (outputs, table draws, dialogs)
 * - forwards clicks on papers, tags, chips, help buttons and track cards to Shiny
 * - draws table cells (links, badges, relevance bars) from plain values
 */
const QEW = (function() {
  'use strict';

  let typesetTimer = null;
  const pending = new Set();
  let typesetChain = Promise.resolve();

  // Typeset math inside `node` (or the whole page). Calls are batched.
  function typeset(node) {
    pending.add(node || document.body);
    if (typesetTimer) clearTimeout(typesetTimer);
    typesetTimer = setTimeout(function() {
      // Keep nodes still on the page, and not those inside another pending node.
      const all = Array.from(pending).filter(function(n) { return document.body.contains(n); });
      const nodes = all.filter(function(n) {
        return !all.some(function(other) { return other !== n && other.contains(n); });
      });
      pending.clear();
      if (!nodes.length || !window.MathJax || !MathJax.typesetPromise) return;
      // One typesetting run at a time. MathJax's list of earlier math is
      // cleared first because outputs and table pages replace their content.
      typesetChain = typesetChain.then(function() {
        if (MathJax.typesetClear) MathJax.typesetClear();
        return MathJax.typesetPromise(nodes);
      }).catch(function(err) { console.log('MathJax: ' + err.message); });
    }, 60);
  }

  function send(name, value) {
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(name, value, {priority: 'event'});
  }

  // Text already escaped by DT stays as it is; these add only fixed markup.
  function attr(value) {
    return String(value === null || value === undefined ? '' : value)
      .replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function column(meta, name) {
    const columns = meta.settings.aoColumns;
    for (let i = 0; i < columns.length; i++) {
      if (columns[i].sTitle === name || columns[i].title === name) return i;
    }
    return -1;
  }

  // DT escapes cell text on the server, so `data` is safe to place in HTML.
  const render = {
    title: function(data, type, row, meta) {
      if (type !== 'display') return data;
      const status = row[column(meta, 'status')];
      const badge = status === 'out_of_scope' ? ' <span class="badge-screened">screened out</span>' : '';
      return '<a class="paper-link" href="' + attr(row[column(meta, 'href')]) + '" data-open-paper="' +
        attr(row[column(meta, 'paper_id')]) + '" data-paper-track="' + attr(row[column(meta, 'row_track')]) +
        '">' + data + '</a>' + badge;
    },
    bookmark: function(data, type) {
      if (type !== 'display') return data;
      return '<button type="button" class="bookmark-icon" data-paper-id="' + attr(data) +
        '" aria-pressed="false" aria-label="Bookmark this paper" title="Bookmark this paper">' +
        '<span class="bookmark-glyph" aria-hidden="true">☆</span></button>';
    },
    code: function(data, type) {
      if (type !== 'display' || !data) return data;
      const cls = data === 'Public' ? 'code-public' : (data === 'Not public' ? 'code-none' : 'code-unknown');
      return '<span class="code-badge ' + cls + '">' + data + '</span>';
    },
    relevance: function(data, type) {
      if (type !== 'display') return data;
      if (data === null || data === undefined || data === '') return '<span class="rel-none">not scored</span>';
      const value = Math.max(0, Math.min(1, Number(data)));
      return '<span class="rel-bar" role="img" aria-label="Relevance ' + value.toFixed(2) + '">' +
        '<span class="rel-fill" style="width:' + Math.round(value * 100) + '%"></span></span>' +
        '<span class="rel-num">' + value.toFixed(2) + '</span>';
    }
  };

  function afterDraw(container) {
    typeset(container);
  }

  function init() {
    $(document).on('shiny:value', function(e) {
      const target = e.target;
      setTimeout(function() { typeset(target); }, 0);
    });
    $(document).on('shown.bs.modal', function(e) { typeset(e.target); });

    // Chat answers arrive in pieces; typeset once a message has stopped changing.
    let chatTimer = null;
    new MutationObserver(function(mutations) {
      const chat = mutations.map(function(m) {
        return m.target.nodeType === 1 ? m.target.closest('.miami-chat-container') : null;
      }).find(function(node) { return node; });
      if (!chat || mutations.every(function(m) { return m.target.closest && m.target.closest('mjx-container'); })) return;
      if (chatTimer) clearTimeout(chatTimer);
      chatTimer = setTimeout(function() { typeset(chat); }, 700);
    }).observe(document.body, { childList: true, subtree: true });

    // Tables inside a closed <details> are measured as zero wide; fix on opening.
    document.addEventListener('toggle', function(e) {
      if (e.target.tagName === 'DETAILS' && e.target.open && $.fn.dataTable) {
        $(e.target).find('table.dataTable').each(function() {
          if ($.fn.dataTable.isDataTable(this)) $(this).DataTable().columns.adjust();
        });
      }
    }, true);

    document.addEventListener('click', function(e) {
      if (!e.target.closest) return;
      const plain = !(e.ctrlKey || e.metaKey || e.shiftKey || e.button);

      const paper = e.target.closest('[data-open-paper]');
      if (paper && plain) {
        e.preventDefault();
        send('qew_open_paper', {
          id: paper.getAttribute('data-open-paper'), track: paper.getAttribute('data-paper-track') || ''
        });
        window.scrollTo(0, 0);
        return;
      }
      const track = e.target.closest('[data-select-track]');
      if (track && plain) {
        e.preventDefault();
        send('qew_select_track', track.getAttribute('data-select-track'));
        window.scrollTo(0, 0);
        return;
      }
      const help = e.target.closest('[data-help]');
      if (help) {
        e.preventDefault();
        send('qew_help', help.getAttribute('data-help'));
        return;
      }
      const chip = e.target.closest('[data-chip-remove]');
      if (chip) {
        send('qew_chip_remove', chip.getAttribute('data-chip-remove'));
        return;
      }
      const tag = e.target.closest('button.tag-chip[data-field]');
      if (tag) {
        send('qew_tag_click', {
          field: tag.getAttribute('data-field'), value: tag.getAttribute('data-value'),
          track: tag.getAttribute('data-track'), role: tag.getAttribute('data-role')
        });
        window.scrollTo(0, 0);
        return;
      }
      const setter = e.target.closest('[data-set-input]');
      if (setter) {
        send(setter.getAttribute('data-set-input'), setter.getAttribute('data-value'));
        return;
      }
      const copy = e.target.closest('[data-copy-from]');
      if (copy) {
        const source = document.getElementById(copy.getAttribute('data-copy-from'));
        if (source && navigator.clipboard) {
          navigator.clipboard.writeText(source.textContent).then(function() {
            if (window.QEPersonalization) QEPersonalization.showToast('Address copied', 'success');
          });
        }
      }
    });

    // Enter in a question box presses its Go button.
    document.addEventListener('keydown', function(e) {
      if (e.key !== 'Enter' || !e.target.getAttribute) return;
      const button = e.target.getAttribute('data-enter-clicks');
      if (!button) return;
      e.preventDefault();
      $(e.target).trigger('change');
      const el = document.getElementById(button);
      if (el) setTimeout(function() { el.click(); }, 0);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  return { typeset: typeset, afterDraw: afterDraw, render: render };
})();
