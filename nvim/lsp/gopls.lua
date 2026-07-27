-- gopls. Installed by the Brewfile's `go "golang.org/x/tools/gopls"` line.
return {
  cmd = { "gopls" },
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_markers = { "go.work", "go.mod", ".git" },
}
