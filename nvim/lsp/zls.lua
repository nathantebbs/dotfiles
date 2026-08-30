-- zls, the Zig language server. Neovim provides the mode and compiler files.
return {
  cmd = { "zls" },
  filetypes = { "zig" },
  root_markers = { "build.zig.zon", "build.zig", ".git" },
}
