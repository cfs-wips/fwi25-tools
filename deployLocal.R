setwd(".")

shinylive::export(
  "app", "docs",
  wasm_packages = TRUE,
  template_params = list(
    title = "Hourly FWI (NG‑CFFDRS)",
    include_in_head = paste0(
      "<script>",
      "window.addEventListener('message',function(e){",
      "  if(e && e.data && e.data.type==='set-title' && typeof e.data.title==='string'){",
      "    try{ document.title = e.data.title; }catch(err){}",
      "  }",
      "}, false);",
      "</script>"
    )
  )
)
  
httpuv::runStaticServer("docs")
