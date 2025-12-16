
// www/app-init.js
(function () {
  'use strict';

  // ---- Logging -----------------------------------------------------------
  const LOG_PREFIX = '[FWI25:init]';
  const LS_KEY = 'fwi25_debug';
  const debugEnabled =
    (new URLSearchParams(window.location.search).get('debug') === '1') ||
    (typeof localStorage !== 'undefined' && localStorage.getItem(LS_KEY) === '1');

  const log = {
    info:  (...args) => console.info(LOG_PREFIX, ...args),
    warn:  (...args) => console.warn(LOG_PREFIX, ...args),
    error: (...args) => console.error(LOG_PREFIX, ...args),
    debug: (...args) => { if (debugEnabled) console.debug(LOG_PREFIX, ...args); }
  };

  log.info('Bootstrap starting', { debugEnabled });

  // ---- Utilities ---------------------------------------------------------
  function whenShinyReady(fn) {
    try {
      const ready = window.Shiny &&
        typeof Shiny.setInputValue === 'function' &&
        typeof Shiny.addCustomMessageHandler === 'function';
      if (ready) { fn(); return; }
      document.addEventListener('shiny:connected', () => {
        try { fn(); } catch (e) { log.error('gated function error:', e); }
      }, { once: true });
    } catch (e) { log.error('whenShinyReady error:', e); }
  }

  function once(fn) {
    let ran = false;
    return function () { if (!ran) { ran = true; try { fn(); } catch (e) { log.error('once error:', e); } } };
  }

  // ---- Busy state toggles (GC cards) ------------------------------------
  document.addEventListener('shiny:recalculating', function (ev) {
    const card = ev.target && ev.target.closest && ev.target.closest('.gc-card');
    if (card) { card.classList.add('busy'); card.setAttribute('aria-busy', 'true'); log.debug('Card busy ON', { id: card.id || '(no id)' }); }
  });
  document.addEventListener('shiny:value', function (ev) {
    const card = ev.target && ev.target.closest && ev.target.closest('.gc-card');
    if (card) { card.classList.remove('busy'); card.setAttribute('aria-busy', 'false'); log.debug('Card busy OFF', { id: card.id || '(no id)' }); }
  });

  // ---- Title / ARIA / mapping disabled / form reset / numeric-blank -----
  whenShinyReady(function () {
    Shiny.addCustomMessageHandler('set-title', function (msg) {
      try {
        document.title = msg;
        log.info('Title set', { title: msg });
        if (window.parent && window.parent !== window) {
          window.parent.postMessage({ type: 'set-title', title: msg }, '*');
          log.debug('Posted set-title to parent');
        }
      } catch (e) { log.warn('set-title handler error', e); }
    });

    Shiny.addCustomMessageHandler('set-aria-labels', function (msg) {
      try {
        if (msg && msg.app) {
          const el = document.getElementById('main-content');
          if (el) el.setAttribute('aria-label', msg.app);
          log.debug('ARIA label set: main-content', { label: msg.app });
        }
        if (msg && msg.tabs) {
          const el = document.getElementById('tabs-region');
          if (el) el.setAttribute('aria-label', msg.tabs);
          log.debug('ARIA label set: tabs-region', { label: msg.tabs });
        }
      } catch (e) { log.warn('set-aria-labels handler error', e); }
    });

    Shiny.addCustomMessageHandler('mappingSetDisabled', function (msg) {
      const el = document.getElementById(msg.id);
      if (!el) { log.warn('mappingSetDisabled: element not found', msg.id); return; }
      const disabled = !!msg.disabled;
      el.setAttribute('aria-disabled', disabled ? 'true' : 'false');
      el.style.pointerEvents = disabled ? 'none' : 'auto';
      el.style.opacity = disabled ? '0.6' : '1';
      log.info('mappingSetDisabled applied', { id: msg.id, disabled });
    });

    Shiny.addCustomMessageHandler('form-reset', function (id) {
      try { const el = document.getElementById(id); if (el && typeof el.reset === 'function') el.reset(); log.info('Form reset', { id }); }
      catch (e) { log.warn('form-reset handler error', e); }
    });

    // Visually blank numeric inputs without sending "NA" (HTML number won’t accept "NA")
    Shiny.addCustomMessageHandler('numeric-blank', function (msg) {
      try {
        const el = document.getElementById(msg.id);
        if (!el) return;
        el.value = '';
        el.dispatchEvent(new Event('input', { bubbles: true }));  // inform Shiny
        log.debug('numeric-blank applied', { id: msg.id });
      } catch (e) { log.warn('numeric-blank handler error', e); }
    });
  });

  // ---- Timezone initialization / re-send --------------------------------
  const initTZ = once(function () {
    let tz = null;
    try { tz = Intl.DateTimeFormat().resolvedOptions().timeZone; log.info('Browser time zone resolved', { tz }); }
    catch (e) { log.warn('Intl timeZone resolution failed', e); }

    function pushTZ() {
      if (!tz) { log.warn('No browser time zone available; skipping push'); return; }
      try {
        Shiny.setInputValue('tz_browser', tz, { priority: 'event' });
        log.info('tz_browser set', { tz });
      } catch (e) {
        log.warn('tz_browser push failed (Shiny not ready yet)', e);
      }
    }

    // Try immediately
    try { pushTZ(); } catch (_) {}

    // Poll for Shiny every 200ms up to ~10s
    let attempts = 0, iv = setInterval(function () {
      attempts++;
      if (window.Shiny && typeof Shiny.setInputValue === 'function') {
        clearInterval(iv);
        pushTZ();
      } else if (attempts >= 50) {
        clearInterval(iv);
        log.warn('Gave up waiting for Shiny API to appear');
      }
    }, 200);

    // Also re-send on reconnect
    document.addEventListener('shiny:connected', function () {
      try { pushTZ(); log.debug('tz_browser re-sent on reconnect', { tz }); }
      catch (e) { log.warn('tz_browser re-send failed on reconnect', e); }
    });
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initTZ, { once: true });
  } else {
    initTZ();
  }

  // ---- Timezone lookup via tzlookup() (server message & local watcher) ---
  whenShinyReady(function () {
    // A) Server -> Client custom message path (keep for parity)
    Shiny.addCustomMessageHandler('tz_lookup', function (msg) {
      try {
        log.info('tz_lookup handler received', msg);
        const tz = (typeof tzlookup === 'function') ? tzlookup(msg.lat, msg.lon) : null;
        Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
        log.info('tz_lookup_result set (server message)', { tz });
      } catch (e) {
        log.error('tz_lookup handler error', e);
        try { Shiny.setInputValue('tz_lookup_result', null, { priority: 'event' }); } catch (_) {}
      }
    });

    // B) Client-side local watcher: when lat/lon inputs change, compute TZ and push it
    log.info('tz_lookup input watcher registering');
    let lastLat = null, lastLon = null;
    const isFiniteNum = (x) => typeof x === 'number' && isFinite(x);

    document.addEventListener('shiny:inputchanged', function (ev) {
      if (!ev || !ev.name) return;

      if (ev.name === 'mapping-manual_lat') {
        lastLat = (typeof ev.value === 'number') ? ev.value : Number(ev.value);
      } else if (ev.name === 'mapping-manual_lon') {
        lastLon = (typeof ev.value === 'number') ? ev.value : Number(ev.value);
      } else {
        return; // ignore other inputs
      }

      if (isFiniteNum(lastLat) && isFiniteNum(lastLon)) {
        try {
          const tz = (typeof tzlookup === 'function') ? tzlookup(lastLat, lastLon) : null;
          Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
          log.info('tz_lookup_result set via inputchanged', { lat: lastLat, lon: lastLon, tz });
        } catch (e) {
          log.warn('tz_lookup_result set failed via inputchanged', e);
          try { Shiny.setInputValue('tz_lookup_result', null, { priority: 'event' }); } catch (_) {}
        }
      }
    });

    // C) Post-upload / initial poller: sniff lat/lon values directly from DOM for a few seconds
    function startLatLonPoller(seconds = 6, everyMs = 250) {
      const end = Date.now() + seconds * 1000;
      const LAT_ID = 'mapping-manual_lat';
      const LON_ID = 'mapping-manual_lon';
      let lastSent = null;

      function tryOnce() {
        const latEl = document.getElementById(LAT_ID);
        const lonEl = document.getElementById(LON_ID);
        if (latEl && lonEl) {
          const lat = Number(latEl.value);
          const lon = Number(lonEl.value);
          const ok = isFinite(lat) && isFinite(lon);
          const key = ok ? lat.toFixed(6) + '/' + lon.toFixed(6) : null;

          if (ok && key !== lastSent) {
            try {
              const tz = (typeof tzlookup === 'function') ? tzlookup(lat, lon) : null;
              Shiny.setInputValue('tz_lookup_result', tz, { priority: 'event' });
              log.info('tz_lookup_result set via poller', { lat, lon, tz });
              lastSent = key;
            } catch (e) {
              log.warn('tz_lookup_result set failed via poller', e);
            }
          }
        }
        if (Date.now() < end) setTimeout(tryOnce, everyMs);
      }
      tryOnce();
    }

    // Kick the poller after DOM ready & after reconnect (covers upload flows)
    document.addEventListener('DOMContentLoaded', function () { startLatLonPoller(6, 250); }, { once: true });
    document.addEventListener('shiny:connected', function () { startLatLonPoller(6, 250); });
  });

  // ---- Parent/iframe awareness ------------------------------------------
  (function iframeAwareness() {
    try {
      const inIframe = window.parent && window.parent !== window;
      log.debug('Iframe context', { inIframe });
      window.addEventListener('message', function (ev) {
        const data = ev && ev.data;
        if (data && data.type === 'tz-browser' && data.tz) {
          log.info('Received TZ from parent', { tz: data.tz });
          whenShinyReady(function () {
            try { Shiny.setInputValue('tz_browser', data.tz, { priority: 'event' }); log.info('tz_browser set from parent', { tz: data.tz }); }
            catch (e) { log.error('tz_browser set from parent failed', e); }
          });
        }
      });
    } catch (e) { log.warn('iframe awareness error', e); }
  })();

  // ---- Plotly helpers ----------------------------------------------------
  whenShinyReady(function () {
    function pushPlotlyMeta(id, maxAttempts = 20, delayMs = 150) {
      let n = 0;
      function tryOnce() {
        const host = document.getElementById(id);
        if (!host) { n++; if (n <= maxAttempts) return setTimeout(tryOnce, delayMs); return; }
        const target = (host && (host.layout || host.data)) ? host :
                       (host.querySelector ? host.querySelector('.plotly') : null);
        const hasLayout = !!(target && target.layout);
        const hasData   = !!(target && target.data && Array.isArray(target.data));
        if (hasLayout) Shiny.setInputValue(id + '_layout', target.layout, { priority: 'event' });
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
        if (!hasLayout || !hasData) { n++; if (n <= maxAttempts) setTimeout(tryOnce, delayMs); }
      }
      tryOnce();
    }

    Shiny.addCustomMessageHandler('getPlotlyLayout', function (message) {
      if (!message || !message.id) return;
      pushPlotlyMeta(message.id);
    });
    Shiny.addCustomMessageHandler('getPlotlyTraces', function (message) {
      if (!message || !message.id) return;
      pushPlotlyMeta(message.id);
    });
  });

  log.info('Bootstrap complete');
})();
``
