setwd(".")
shinylive::export("app", "docs", wasm_packages = TRUE, max_filesize = "200M")
httpuv::runStaticServer("docs")
