/* app-init.js — FWI25 bootstrap for Shiny & Shinylive (WebR)
   - Robust readiness gating for both server Shiny and Shinylive
   - One-time timezone initialization with re-send on reconnect
   - Busy state toggles for GC cards
   - Title/ARIA/message handlers
   - Structured console logging (enable via ?debug=1 or localStorage)
*/
(function () {
  "use strict";
  const LOG_PREFIX = "[FWI25:init]";
  const LS_KEY = "fwi25_debug";
  const debugEnabled = (new URLSearchParams(window.location.search).get("debug") === "1") ||
                       (typeof localStorage !== "undefined" && localStorage.getItem(LS_KEY) === "1");
  const log = {
    info: (...args) => console.info(LOG_PREFIX, ...args),
    warn: (...args) => console.warn(LOG_PREFIX, ...args),
    error: (...args) => console.error(LOG_PREFIX, ...args),
    debug: (...args) => { if (debugEnabled) console.debug(LOG_PREFIX, ...args); },
  };
  log.info("Bootstrap starting", { debugEnabled });

  // Utilities
  function once(fn) { let ran = false; return function() { if (!ran) { ran = true; try { fn(); } catch (e) { log.error("once wrapper error:", e); } } }; }
  function whenShinyReady(fn) {
    try {
      if (window.Shiny && typeof Shiny.setInputValue === "function" && typeof Shiny.addCustomMessageHandler === "function") {
        log.debug("Shiny API present; executing immediately"); fn(); return;
      }
      const onConnected = () => { log.debug("Received shiny:connected; executing gated function"); try { fn(); } catch (e) { log.error("gated function error:", e); } };
      document.addEventListener("shiny:connected", onConnected, { once: true });
      let tries = 0, maxTries = 200; // ~10s @50ms
      const iv = setInterval(() => {
        tries++;
        if (window.Shiny && typeof Shiny.setInputValue === "function" && typeof Shiny.addCustomMessageHandler === "function") {
          clearInterval(iv); log.debug("Shiny API appeared after polling; executing gated function"); try { fn(); } catch (e) { log.error("gated (poll) function error:", e); }
        } else if (tries % 40 === 0) { log.debug("Waiting for Shiny API...", { tries }); }
        if (tries >= maxTries) { clearInterval(iv); log.warn("Gave up waiting for Shiny API after polling"); }
      }, 50);
    } catch (e) { log.error("whenShinyReady error:", e); }
  }

  const sendInitEvent = once(function () {
    whenShinyReady(function () { const token = Math.random(); log.info("Sending __init__ seed event", { token }); Shiny.setInputValue("__init__", token, { priority: "event" }); });
  });
  if (document.readyState === "loading") { document.addEventListener("DOMContentLoaded", sendInitEvent, { once: true }); } else { sendInitEvent(); }

  // Busy state toggles (GC cards)
  document.addEventListener("shiny:recalculating", function (ev) { const el = ev.target; const card = el && el.closest(".gc-card"); if (card) { card.classList.add("busy"); card.setAttribute("aria-busy", "true"); log.debug("Card busy ON", { id: card.id || "(no id)" }); } });
  document.addEventListener("shiny:value", function (ev) { const el = ev.target; const card = el && el.closest(".gc-card"); if (card) { card.classList.remove("busy"); card.setAttribute("aria-busy", "false"); log.debug("Card busy OFF", { id: card.id || "(no id)" }); } });

  // Message handlers (title, ARIA, form reset, mapping disabled)
  whenShinyReady(function () {
    Shiny.addCustomMessageHandler("set-title", function (msg) { try { document.title = msg; log.info("Title set", { title: msg }); if (window.parent && window.parent !== window) { window.parent.postMessage({ type: "set-title", title: msg }, "*"); log.debug("Posted set-title to parent"); } } catch (e) { log.warn("set-title handler error", e); } });
    Shiny.addCustomMessageHandler("set-aria-labels", function (msg) { try { if (msg && msg.app) { const el = document.getElementById("main-content"); el?.setAttribute("aria-label", msg.app); log.debug("ARIA label set: main-content", { label: msg.app }); } if (msg && msg.tabs) { const el = document.getElementById("tabs-region"); el?.setAttribute("aria-label", msg.tabs); log.debug("ARIA label set: tabs-region", { label: msg.tabs }); } } catch (e) { log.warn("set-aria-labels handler error", e); } });
    Shiny.addCustomMessageHandler("mappingSetDisabled", function (msg) { const el = document.getElementById(msg.id); if (!el) { log.warn("mappingSetDisabled: element not found", msg.id); return; } const disabled = !!msg.disabled; el.setAttribute("aria-disabled", disabled ? "true" : "false"); el.style.pointerEvents = disabled ? "none" : "auto"; el.style.opacity = disabled ? "0.6" : "1"; log.info("mappingSetDisabled applied", { id: msg.id, disabled }); });
    Shiny.addCustomMessageHandler("form-reset", function (id) { try { document.getElementById(id)?.reset(); log.info("Form reset", { id }); } catch (e) { log.warn("form-reset handler error", e); } });
  });

  // Timezone initialization
  const initTZ = once(function () {
    let tz = null; try { tz = Intl.DateTimeFormat().resolvedOptions().timeZone; log.info("Browser time zone resolved", { tz }); } catch (e) { log.warn("Intl timeZone resolution failed", e); }
    if (!tz) { log.warn("No browser time zone available; skipping push"); return; }
    const pushTZ = () => { if (window.Shiny && typeof Shiny.setInputValue === "function") { Shiny.setInputValue("tz_browser", tz, { priority: "event" }); log.info("tz_browser set", { tz }); } else { log.debug("Shiny not ready; waiting to push tz_browser"); document.addEventListener("shiny:connected", function () { try { Shiny.setInputValue("tz_browser", tz, { priority: "event" }); log.info("tz_browser set after shiny:connected", { tz }); } catch (e) { log.error("Failed pushing tz_browser after connect", e); } }, { once: true }); } };
    pushTZ();
    document.addEventListener("shiny:connected", function () { try { Shiny.setInputValue("tz_browser", tz, { priority: "event" }); log.debug("tz_browser re-sent on reconnect", { tz }); } catch (e) { log.warn("tz_browser re-send failed on reconnect", e); } });
  });
  if (document.readyState === "loading") { document.addEventListener("DOMContentLoaded", initTZ, { once: true }); } else { initTZ(); }

  // Timezone lookup via tzlookup() (if present)
  whenShinyReady(function () {
    Shiny.addCustomMessageHandler("tz_lookup", function (msg) {
      try { if (typeof tzlookup !== "function") { log.warn("tz_lookup requested but tzlookup() is undefined"); Shiny.setInputValue("tz_lookup_result", null, { priority: "event" }); return; }
        const tz = tzlookup(msg.lat, msg.lon); Shiny.setInputValue("tz_lookup_result", tz, { priority: "event" }); log.info("tz_lookup_result set", { lat: msg.lat, lon: msg.lon, tz });
      } catch (e) { log.error("tz_lookup handler error", e); try { Shiny.setInputValue("tz_lookup_result", null, { priority: "event" }); } catch (_) {}
      }
    });
  });

  // Parent/iframe awareness (Shinylive embeds)
  (function iframeAwareness() { try { const inIframe = window.parent && window.parent !== window; log.debug("Iframe context", { inIframe }); window.addEventListener("message", function (ev) { const data = ev?.data; if (data && data.type === "tz-browser" && data.tz) { log.info("Received TZ from parent", { tz: data.tz }); whenShinyReady(function () { Shiny.setInputValue("tz_browser", data.tz, { priority: "event" }); log.info("tz_browser set from parent message", { tz: data.tz }); }); } }); } catch (e) { log.warn("iframe awareness error", e); } })();

  // ---- Plotly helpers moved from mod_plot.R (robust, namespaced, retry) ----
  whenShinyReady(function () {
    function pushPlotlyMeta(id, maxAttempts = 20, delayMs = 150) {
      let n = 0;
      function tryOnce() {
        const host = document.getElementById(id);
        if (!host) { n++; if (n <= maxAttempts) return setTimeout(tryOnce, delayMs); return; }
        // target: either the host itself has layout/data, or its inner '.plotly' node
        const target = (host && (host.layout || host.data)) ? host : (host.querySelector ? host.querySelector('.plotly') : null);
        const hasLayout = !!(target && target.layout);
        const hasData   = !!(target && target.data && Array.isArray(target.data));
        if (hasLayout) { Shiny.setInputValue(id + '_layout', target.layout, { priority: 'event' }); }
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
    Shiny.addCustomMessageHandler('getPlotlyLayout', function(message) { if (!message || !message.id) return; pushPlotlyMeta(message.id); });
    Shiny.addCustomMessageHandler('getPlotlyTraces', function(message) { if (!message || !message.id) return; pushPlotlyMeta(message.id); });
    // Also auto-push after output updates (Shiny & Shinylive)
    document.addEventListener('shiny:value', function(ev) { const el = ev.target; if (!el || !el.id) return; if (el.querySelector && el.querySelector('.plotly')) { pushPlotlyMeta(el.id); } });
  });

  log.info("Bootstrap complete");
})();
