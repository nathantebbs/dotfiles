-- ols, the Odin language server. Not installed on this machine yet; init.lua
-- skips any server whose binary is missing, so this stays inert until it is.
return {
  cmd = { "ols" },
  filetypes = { "odin" },
  root_markers = { "ols.json", "odinfmt.json", ".git" },
}
