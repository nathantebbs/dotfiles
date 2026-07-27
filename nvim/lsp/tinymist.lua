-- tinymist. Supersedes typst-lsp, which upstream has deprecated.
-- Neovim detects the typst filetype on its own; typst.vim only adds syntax.
return {
  cmd = { "tinymist" },
  filetypes = { "typst" },
  root_markers = { "typst.toml", ".git" },
  settings = {
    formatterMode = "typstyle",
  },
}
