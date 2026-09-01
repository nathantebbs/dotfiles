-- ~/.config/nvim/ftplugin/odin.lua
-- Runs after Neovim's own ftplugin/odin.vim, which sets commentstring and
-- suffixesadd. Indentation, :make and format on save are what it leaves out.
--
-- The Emacs half of this is .emacs.d/configs/rc-odin.el. Both editors use ols
-- and odinfmt, so the two agree on formatting and on where a project starts.

-- Tabs, against the spaces init.lua sets everywhere else: the core library and
-- odinfmt both use them. The width has to match odinfmt.json's tabs_width or a
-- level indents to a tab plus padding spaces rather than to one tab.
vim.bo.expandtab = false
vim.bo.tabstop = 4
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4

-- odin reports a diagnostic as path(line:column) Error:, then echoes the
-- offending line and a caret line under it. %-G discards those two.
vim.bo.errorformat = table.concat({
  [[%f(%l:%c) Syntax %trror: %m]],
  [[%f(%l:%c) %trror: %m]],
  [[%f(%l:%c) %tarning: %m]],
  [[%f(%l:%c) %m]],
  [[%-G%.%#]],
}, ",")

-- A package in Odin is a directory holding .odin files, so a root that holds
-- only subdirectories is not one. The buffer's own directory always is, which
-- is what makes the fallback safe. Mirrors rc-odin--package.
local function holds_odin(dir)
  if not dir or not vim.uv.fs_stat(dir) then
    return false
  end
  for name, kind in vim.fs.dir(dir) do
    if kind == "file" and name:sub(-5) == ".odin" then
      return true
    end
  end
  return false
end

-- A Makefile at the root wins, since it already carries the flags and the
-- -out: path a bare odin build would have to invent.
local buffer_dir = vim.fn.expand("%:p:h")
local root = vim.fs.root(0, { ".git", "ols.json", "odinfmt.json" }) or buffer_dir

if vim.uv.fs_stat(root .. "/Makefile") then
  vim.bo.makeprg = "make -C " .. vim.fn.fnameescape(root)
else
  local src = root .. "/src"
  local package = (holds_odin(src) and src)
    or (holds_odin(root) and root)
    or buffer_dir
  -- cd first: odin writes the binary into the process working directory, so
  -- without this :make drops it wherever Neovim happens to have been started.
  vim.bo.makeprg = "cd " .. vim.fn.fnameescape(root) .. " && odin build "
    .. vim.fn.fnameescape(package) .. " -vet -strict-style -debug"
end

-- Format on save, which is what apheleia does for Odin in Emacs. Routed
-- through ols rather than running odinfmt directly, so the buffer edit is the
-- server's workspace edit and point survives it. A machine without ols saves
-- unformatted rather than erroring.
local group = vim.api.nvim_create_augroup("rc_odin", { clear = false })
vim.api.nvim_clear_autocmds({ group = group, buffer = 0 })
vim.api.nvim_create_autocmd("BufWritePre", {
  group = group,
  buffer = 0,
  desc = "Format Odin through ols",
  callback = function(ev)
    if next(vim.lsp.get_clients({ bufnr = ev.buf, method = "textDocument/formatting" })) then
      vim.lsp.buf.format({ bufnr = ev.buf, timeout_ms = 2000 })
    end
  end,
})

vim.b.undo_ftplugin = (vim.b.undo_ftplugin or "")
  .. " | setlocal expandtab< tabstop< shiftwidth< softtabstop<"
  .. " errorformat< makeprg<"
