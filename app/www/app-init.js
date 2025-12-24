
(function () {
  'use strict';

  // -------------------------------------------------
  // Logging helpers
  // -------------------------------------------------
  const LOG_PREFIX = '[FWI25:init]';
  const LS_KEY = 'fwi25_debug';
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

  // -------------------------------------------------
  // Safety: do not rewrite inside `.no-i18n`
  // If any mutation occurs there, re-bind Shiny safely.
  // -------------------------------------------------
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

  // -------------------------------------------------
  // Busy state toggles for GC cards (optional)
  // -------------------------------------------------
  document.addEventListener('shiny:recalculating', function (ev) {
    const card = ev.target?.closest?.('.gc-card');
    if (card) { card.classList.add('busy'); card.setAttribute('aria-busy', 'true'); }
  });
  document.addEventListener('shiny:value', function (ev) {
    const card = ev.target?.closest?.('.gc-card');
    if (card) { card.classList.remove('busy'); card.setAttribute('aria-busy', 'false'); }
  });

  // -------------------------------------------------
  // Register Shiny message handlers (after Shiny is ready)
  // -------------------------------------------------
  Shiny.initializedPromise.then(() => {
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
  });

  // -------------------------------------------------
  // Time zone helpers
  // -------------------------------------------------
  Shiny.initializedPromise.then(() => {
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
          const ok = isFinite(lat) && isFinite(lon);
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
    Shiny.initializedPromise.then(() => startLatLonPoller(6, 250));
  })();

  // -------------------------------------------------
  // Plotly meta helpers (optional; keep if you read layout/traces in R)
  // -------------------------------------------------
  Shiny.initializedPromise.then(() => {
    function getGraphDiv(host) {
      if (!host) return null;
      // Sometimes the host itself is the GraphDiv (R htmlwidget sets classes on it)
      if (host.classList.contains('js-plotly-plot') || host.layout || host.data) return host;
      return host.querySelector('.js-plotly-plot, .plotly');
    }

    // --- push meta from a GraphDiv (gd) ---
    function pushMetaFromGraphDiv(id, gd) {
      const hasLayout = !!(gd && gd.layout);
      const hasData = !!(gd && gd.data && Array.isArray(gd.data));
      log.info('pushMetaFromGraphDiv: state', { id, hasLayout, hasData });

      if (hasLayout) {
        Shiny.setInputValue(id + '_layout', gd.layout, { priority: 'event' });
        log.info('pushPlotlyMeta: layout pushed', { id });
      }
      if (hasData) {
        const meta = { indices: [], id: [], srcType: [], yaxis: [], showlegend: [] };
        for (let i = 0; i < gd.data.length; i++) {
          const tr = gd.data[i];
          meta.indices.push(i);
          const cd0 = (tr.customdata && tr.customdata[0]) ? tr.customdata[0] : null; // list-of-lists [seriesLabel, stationId]
          meta.id.push(cd0 ? cd0[1] : null);
          meta.srcType.push((tr.line && tr.line.dash === 'dash') ? 'fwi87' : 'fwi25');
          meta.yaxis.push(tr.yaxis || 'y');
          meta.showlegend.push(!!tr.showlegend);
        }
        Shiny.setInputValue(id + '_traces', meta, { priority: 'event' });
        log.info('pushPlotlyMeta: traces pushed', { id, count: meta.indices.length });
      }
    }

    // --- wait until GraphDiv exists, then push meta after first render ---
    function pushMetaWhenGraphReady(id, timeoutMs = 10000) {
      const host = document.getElementById(id);
      if (!host) { log.warn('pushMetaWhenGraphReady: host not found', { id }); return; }
      let gd = getGraphDiv(host);

      // If already ready, push immediately
      if (gd && gd.layout && gd.data) {
        log.info('pushMetaWhenGraphReady: gd ready immediately', { id });
        pushMetaFromGraphDiv(id, gd);
        return;
      }

      // Watch for GraphDiv to appear
      const obs = new MutationObserver(() => {
        gd = getGraphDiv(host);
        if (gd && gd.addEventListener) {
          obs.disconnect();
          log.info('pushMetaWhenGraphReady: GraphDiv detected', { id });
          gd.addEventListener('plotly_afterplot', () => {
            log.info('pushMetaWhenGraphReady: afterplot fired', { id });
            pushMetaFromGraphDiv(id, gd);
          }, { once: true });
        }
      });
      obs.observe(host, { childList: true, subtree: true });

      // Safety timeout
      setTimeout(() => { try { obs.disconnect(); } catch (e) { /* noop */ } }, timeoutMs);
    }

    // --- robust handler registration ---
    function registerHandlers() {
      if (!window.Shiny || typeof Shiny.addCustomMessageHandler !== 'function') return false;
      if (!Shiny.customMessageHandlers) { Shiny.customMessageHandlers = {}; log.info('Created Shiny.customMessageHandlers'); }
      log.info('Registering custom message handlers');
      Shiny.addCustomMessageHandler('getPlotlyLayout', (m) => { if (m?.id) { log.info('getPlotlyLayout received', { id: m.id }); pushMetaWhenGraphReady(m.id); } });
      Shiny.addCustomMessageHandler('getPlotlyTraces', (m) => { if (m?.id) { log.info('getPlotlyTraces received', { id: m.id }); pushMetaWhenGraphReady(m.id); } });
      return true;
    }

    if (!registerHandlers()) {
      log.info('Shiny not ready; deferring handler registration');
      document.addEventListener('shiny:connected', () => { log.info('shiny:connected -> registering'); registerHandlers(); }, { once: true });
      let attempts = 0;
      const t = setInterval(() => { attempts++; if (registerHandlers() || attempts >= 20) clearInterval(t); }, 250);
    }
  });

  // -------------------------------------------------
  // Minimal instrumentation
  // -------------------------------------------------
  document.addEventListener('DOMContentLoaded', () => { log.info('DOMContentLoaded'); });
  Promise.resolve(window.Shiny?.initializedPromise).then(() => { log.info('Shiny.initializedPromise resolved'); });
  log.info('Bootstrap complete');

})();

// app-init.js (append)
// Viewport height fix: sets CSS --vh-page to the real innerHeight (mobile-friendly)
(function () {
  function setViewportVars() {
    const vh = window.innerHeight * 0.01;
    document.documentElement.style.setProperty('--vh-page', `${vh * 100}px`);
    // Derive your other tokens from the true page height if desired
    document.documentElement.style.setProperty('--vh-input-card', `${Math.round(vh * 60)}vh`);
    document.documentElement.style.setProperty('--vh-output-card', `${Math.round(vh * 40)}vh`);
  }
  window.addEventListener('resize', setViewportVars, { passive: true });
  window.addEventListener('orientationchange', setViewportVars);
  setViewportVars();
})();
