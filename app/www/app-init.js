
(function () {
  'use strict';

  // --------------------------
  // Logging helpers
  // --------------------------
  const LOG_PREFIX = '[FWI25:init]';
  const LS_KEY     = 'fwi25_debug';
  const debugEnabled =
    (new URLSearchParams(window.location.search).get('debug') === '1') ||
    (typeof localStorage !== 'undefined' && localStorage.getItem(LS_KEY) === '1');

  const log = {
    info: (...args) => console.info(LOG_PREFIX, ...args),
    warn: (...args) => console.warn(LOG_PREFIX, ...args),
    error: (...args) => console.error(LOG_PREFIX, ...args),
    debug: (...args) => { if (debugEnabled) console.debug(LOG_PREFIX, ...args); }
  };

  log.info('Bootstrap starting', { debugEnabled });

  // --------------------------
  // Gate: run only after Shiny is initialized
  // --------------------------
  function whenShinyReady(fn) {
    Promise.resolve(window.Shiny?.initializedPromise)
      .then(() => { try { fn(); } catch (e) { log.error('gated fn error:', e); } });
  }

  // --------------------------
  // Safety: do not rewrite inside `.no-i18n`
  // If any mutation occurs there, re-bind Shiny safely.
  // --------------------------
  const noI18nObserver = new MutationObserver((mutations) => {
    for (const m of mutations) {
      const host = m.target.closest?.('.no-i18n');
      if (host && m.type === 'childList') {
        log.warn('Prevent rewriting inside .no-i18n; Shiny bindings may break');
        if (window.Shiny?.bindAll) Shiny.bindAll(host);
      }
    }
  });

  function watchNoI18n() {
    document.querySelectorAll('.no-i18n').forEach((el) => {
      noI18nObserver.observe(el, { childList: true, subtree: true });
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', watchNoI18n, { once: true });
  } else {
    watchNoI18n();
  }

  // --------------------------
  // Busy state toggles for GC cards (optional)
  // --------------------------
  document.addEventListener('shiny:recalculating', function (ev) {
    const card = ev.target?.closest?.('.gc-card');
    if (card) { card.classList.add('busy'); card.setAttribute('aria-busy', 'true'); }
  });
  document.addEventListener('shiny:value', function (ev) {
    const card = ev.target?.closest?.('.gc-card');
    if (card) { card.classList.remove('busy'); card.setAttribute('aria-busy', 'false'); }
  });

  // --------------------------
  // Register Shiny message handlers (after Shiny is ready)
  // --------------------------
  whenShinyReady(function () {
    // Title
    Shiny.addCustomMessageHandler('set-title', function (msg) {
      try { document.title = msg; } catch (e) { log.warn('set-title error', e); }
    });

    // ARIA labels
    Shiny.addCustomMessageHandler('set-aria-labels', function (msg) {
      try {
        if (msg?.app)  document.getElementById('main-content')?.setAttribute('aria-label', msg.app);
        if (msg?.tabs) document.getElementById('tabs-region')?.setAttribute('aria-label', msg.tabs);
      } catch (e) { log.warn('set-aria-labels error', e); }
    });

    // Enable/disable mapping fieldset
    Shiny.addCustomMessageHandler('mappingSetDisabled', function (msg) {
      const el = document.getElementById(msg.id);
      if (!el) return;
      const disabled = !!msg.disabled;
      el.setAttribute('aria-disabled', disabled ? 'true' : 'false');
      el.style.pointerEvents = disabled ? 'none' : 'auto';
      el.style.opacity = disabled ? '0.6' : '1';
      log.info('mappingSetDisabled applied', { id: msg.id, disabled });
    });

    // Form reset
    Shiny.addCustomMessageHandler('form-reset', function (id) {
      const el = document.getElementById(id);
      if (el && typeof el.reset === 'function') el.reset();
      log.info('Form reset', { id });
    });

    // Numeric inputs: clear visually
    Shiny.addCustomMessageHandler('numeric-blank', function (msg) {
      try {
        const el = document.getElementById(msg.id);
        if (!el) return;
        el.value = '';
        el.dispatchEvent(new Event('input', { bubbles: true }));
      } catch (e) { log.warn('numeric-blank error', e); }
    });

    // ---- File input labels (LEAN) ----
    // Only placeholder and accessibility; do NOT touch button children (icon-only is rendered in R)
    Shiny.addCustomMessageHandler('updateFileInputLabels', function (message) {
      try {
        if (!message?.id) return;
        const hiddenInput = document.getElementById(message.id);
        if (!hiddenInput) return;

        // The visible controls live in the nearest .input-group
        const container = hiddenInput.closest('.input-group') || hiddenInput.parentElement;
        if (!container) return;

        // Placeholder on the readonly text input (filename box)
        const textInput = container.querySelector('input.form-control[readonly]');
        if (textInput && typeof message.placeholder === 'string') {
          textInput.setAttribute('placeholder', message.placeholder);
          textInput.value = '';
          textInput.setAttribute('aria-label', message.placeholder);
        }

        // Accessibility on the file input itself
        if (typeof message.buttonLabel === 'string') {
          hiddenInput.setAttribute('title', message.buttonLabel);
          hiddenInput.setAttribute('aria-label', message.buttonLabel);
        }

        if (window.Shiny?.bindAll) Shiny.bindAll(container);
        log.debug('updateFileInputLabels (lean) applied', { id: message.id });
      } catch (e) {
        log.warn('updateFileInputLabels handler error', e);
      }
    });
  });

  // --------------------------
  // Time zone helpers
  // --------------------------
  whenShinyReady(function () {
    // A) Push browser IANA zone once Shiny is ready
    try {
      const tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
      Shiny.setInputValue('tz_browser', tz, { priority: 'event' });
      log.info('tz_browser set', { tz });
    } catch (e) {
      log.warn('tz_browser push failed', e);
    }

    // B) Server -> client request: tz_lookup(lat, lon)
    Shiny.addCustomMessageHandler('tz_lookup', function (msg) {
      try {
        const tz = (typeof tzlookup === 'function') ? tzlookup(msg.lat, msg.lon) : null; // from tz.js
        Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
        log.info('tz_lookup_result set (server message)', { tz });
      } catch (e) {
        log.error('tz_lookup handler error', e);
        Shiny.setInputValue('tz_lookup_result', null, { priority: 'event' });
      }
    });
  });

  // C) Client watcher: whenever manual_lat/lon inputs change, compute tz
  (function () {
    let lastLat = null, lastLon = null;
    const isFiniteNum = (x) => typeof x === 'number' && isFinite(x);

    document.addEventListener('shiny:inputchanged', function (ev) {
      if (!ev?.name) return;

      if (/-manual_lat$/.test(ev.name)) {
        lastLat = (typeof ev.value === 'number') ? ev.value : Number(ev.value);
      } else if (/-manual_lon$/.test(ev.name)) {
        lastLon = (typeof ev.value === 'number') ? ev.value : Number(ev.value);
      } else {
        return;
      }

      if (isFiniteNum(lastLat) && isFiniteNum(lastLon)) {
        try {
          const tz = (typeof tzlookup === 'function') ? tzlookup(lastLat, lastLon) : null;
          Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
          log.info('tz_lookup_result set via inputchanged', { lat: lastLat, lon: lastLon, tz });
        } catch (e) {
          log.warn('tz_lookup_result set failed via inputchanged', e);
          Shiny.setInputValue('tz_lookup_result', null, { priority: 'event' });
        }
      }
    });

    // Lat/lon poller: sniff DOM for a few seconds and push tz (starts only when Shiny is ready)
    function startLatLonPoller(seconds = 6, everyMs = 250) {
      const end = Date.now() + seconds * 1000;
      let lastKey = null;

      function tryOnce() {
        const latEl = document.querySelector("[id$='manual_lat']");
        const lonEl = document.querySelector("[id$='manual_lon']");
        if (latEl && lonEl) {
          const lat = Number(latEl.value);
          const lon = Number(lonEl.value);
          const ok  = isFinite(lat) && isFinite(lon);
          const key = ok ? lat.toFixed(6) + '/' + lon.toFixed(6) : null;

          if (ok && key !== lastKey) {
            try {
              const tz = (typeof tzlookup === 'function') ? tzlookup(lat, lon) : null;
              Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
              log.info('tz_lookup_result set via poller', { lat, lon, tz });
              lastKey = key;
            } catch (e) {
              log.warn('poller tz push failed', e);
            }
          }
        }
        if (Date.now() < end) setTimeout(tryOnce, everyMs);
      }

      tryOnce();
    }

    // Start the poller only once Shiny is ready (avoid early setInputValue errors)
    whenShinyReady(() => startLatLonPoller(6, 250));
  })();

  // --------------------------
  // Plotly meta helpers (optional; keep if you read layout/traces in R)
  // --------------------------
  whenShinyReady(function () {
    function pushPlotlyMeta(id, maxAttempts = 20, delayMs = 150) {
      let n = 0;
      function tryOnce() {
        const host = document.getElementById(id);
        if (!host) {
          n++;
          if (n <= maxAttempts) return setTimeout(tryOnce, delayMs);
          return;
        }
        const target = (host && (host.layout || host.data)) ? host :
                       (host.querySelector ? host.querySelector('.plotly') : null);

        const hasLayout = !!(target && target.layout);
        const hasData   = !!(target && target.data && Array.isArray(target.data));

        if (hasLayout)
          Shiny.setInputValue(id + '_layout', target.layout, { priority: 'event' });

        if (hasData) {
          const meta = { indices: [], id: [], srcType: [], yaxis: [], showlegend: [] };
          for (let i = 0; i < target.data.length; i++) {
            const tr = target.data[i];
            meta.indices.push(i);
            const cd0 = (tr.customdata && tr.customdata[0]) ? tr.customdata[0] : null;
            meta.id.push(cd0 ? cd0[1] : null);
            meta.srcType.push((tr.line && tr.line.dash === 'dash') ? 'fwi87' : 'fwi25');
            meta.yaxis.push(tr.yaxis || 'y');
            meta.showlegend.push(!!tr.showlegend);
          }
          Shiny.setInputValue(id + '_traces', meta, { priority: 'event' });
        }

        if (!hasLayout || !hasData) {
          n++;
          if (n <= maxAttempts) setTimeout(tryOnce, delayMs);
        }
      }
      tryOnce();
    }

    Shiny.addCustomMessageHandler('getPlotlyLayout', (m) => { if (m?.id) pushPlotlyMeta(m.id); });
    Shiny.addCustomMessageHandler('getPlotlyTraces', (m) => { if (m?.id) pushPlotlyMeta(m.id); });
  });
  

  // --------------------------
  // Minimal instrumentation
   // --------------------------
  document.addEventListener('DOMContentLoaded', () => { log.info('DOMContentLoaded'); });
  Promise.resolve(window.Shiny?.initializedPromise).then(() => { log.info('Shiny.initializedPromise resolved'); });

  log.info('Bootstrap complete');
})();