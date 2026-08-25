-- ols, the Odin language server. init.lua skips any server
-- whose binary is missing, so this stays inert on a machine without it.
return {
  cmd = { "ols" },
  filetypes = { "odin" },
  root_markers = { "ols.json", "odinfmt.json", ".git" },
}
