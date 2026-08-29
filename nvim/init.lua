-- ~/.config/nvim/init.lua
-- Minimal Neovim config based ../.vimrc for desktop environment usage
-- Author: Nathan Tebbs
-- Created: 2026-01-24

-- =====================
-- Options
-- =====================

-- Everything of mine hangs off the leader, so Neovim keeps its own Ctrl keys.
-- Must precede the mappings and lazy.nvim, which reads it at setup.
vim.g.mapleader = " "

vim.opt.clipboard = "unnamedplus"

vim.opt.relativenumber = true
vim.opt.number = true

-- Spaces everywhere; clang-format is UseTab: Never and gofmt is handled by
-- Neovim's own ftplugin/go.vim, which sets noexpandtab.
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.autoindent = true

vim.opt.scrolloff = 10

vim.opt.textwidth = 79

-- Prompt instead of failing on :q with unsaved changes. `hidden' is already
-- Neovim's default, so only this half of the .vimrc pair is needed.
vim.opt.confirm = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Neovim keeps undo files under stdpath("state"), so unlike the .vimrc there
-- is no directory to build here.
vim.opt.undofile = true
vim.opt.undolevels = 10000

-- Finding files
vim.opt.path:append("**")
vim.opt.wildmenu = true
vim.opt.wildmode = "longest:full,full"
vim.opt.wildignorecase = true

-- Keeps :find and path=** usable in a repo with vendored dependencies
vim.opt.wildignore:append({ "*.o", "*.obj", "*.a", "*.so", "*.dylib", "*.pyc", "*.class" })
vim.opt.wildignore:append({ ".git/**", "node_modules/**", "target/**", "build/**", "dist/**" })

-- True color (good default in Neovim)
vim.opt.termguicolors = true

-- =====================
-- Files
-- =====================
local rc_files = vim.api.nvim_create_augroup("rc_files", { clear = true })

-- Pick up changes made outside Neovim
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
  group = rc_files,
  command = "silent! checktime",
})

vim.api.nvim_create_autocmd("BufReadPost", {
  group = rc_files,
  desc = "Reopen a file where it was left, except for commit messages",
  callback = function(ev)
    if vim.bo[ev.buf].filetype:match("commit") then
      return
    end
    local mark = vim.api.nvim_buf_get_mark(ev.buf, '"')
    if mark[1] >= 1 and mark[1] <= vim.api.nvim_buf_line_count(ev.buf) then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- =====================
-- netrw tweaks
-- =====================
vim.g.netrw_banner = 0
vim.g.netrw_browse_split = 2
vim.g.netrw_altv = 1
vim.g.netrw_liststyle = 3

-- =====================
-- Keymaps
-- =====================
-- The same bindings as the .vimrc, on the same keys. Telescope stands in for
-- fzf.vim, which is the only reason any of these differ in what they call.
local map = vim.keymap.set

map("n", "<Space>", "<Nop>")

map("i", "<C-h>", "<C-w>")
map("i", "<C-BS>", "<C-w>")
map("i", "<C-Backspace>", "<C-w>")

-- Finding things
map("n", "<leader>f", "<cmd>Telescope find_files<cr>", { desc = "Files" })
map("n", "<leader>b", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })
map("n", "<leader>l", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Lines in buffer" })
map("n", "<leader>/", "<cmd>Telescope live_grep<cr>", { desc = "Grep the project" })
map("n", "<leader>m", "<cmd>Telescope keymaps<cr>", { desc = "Maps" })

-- Files and buffers
map("n", "<leader>e", "<cmd>Ex<cr>", { desc = "netrw" })
map("n", "<leader>k", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>w", "<cmd>write<cr>", { desc = "Write" })

-- Undotree
map("n", "<leader>u", "<cmd>UndotreeToggle<cr>", { desc = "Toggle undotree" })

-- Keep the selection after shifting it
map("x", "<", "<gv")
map("x", ">", ">gv")

-- No .vimrc counterpart: vim-plug is driven by :PlugInstall, not a mapping.
map("n", "<C-c><C-p>i", "<cmd>Lazy sync<cr>", { desc = "Plugins: install/update" })
map("n", "<C-c><C-p>c", "<cmd>Lazy clean<cr>", { desc = "Plugins: clean" })

-- =====================
-- Plugins
-- =====================
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    {
      'nvim-telescope/telescope.nvim', version = '*',
      dependencies = {
        'nvim-lua/plenary.nvim',
        -- Native fzf sorter. Building it requires make.
        { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
      }
    },

    { "tpope/vim-surround" },
    { "itchyny/lightline.vim" },

    { "windwp/nvim-autopairs" },

    {
      "folke/todo-comments.nvim",
      dependencies = { "nvim-lua/plenary.nvim" },
      opts = {},
    },

    -- Presenter
    {
      "sotte/presenting.nvim",
      opts = {},
      cmd = { "Presenting" },
    },
    { "mbbill/undotree" },

    -- Colors
    { "c9rgreen/vim-colors-modus" },

    -- Language Support
    { "kaarmu/typst.vim", ft = 'typst', lazy=false },
  })

-- =====================
-- LSP
-- =====================
-- Neovim 0.11 moved the client into core, so there is no lspconfig plugin.
-- Each server's table lives in lsp/<name>.lua, found on the runtimepath.

-- Indexing vim.lsp.config resolves those files; require() only searches lua/.
-- Absent binary means skip, like the PATH guards in config.bash.
local servers = { "clangd", "gopls", "pyright", "tinymist", "ols" }
for _, name in ipairs(servers) do
  local cmd = vim.lsp.config[name].cmd
  if vim.fn.executable(cmd[1]) == 1 then
    vim.lsp.enable(name)
  end
end

vim.diagnostic.config({
  virtual_text = { prefix = "*" },
  severity_sort = true,
  float = { border = "single", source = true },
})

-- Core already binds grn, gra, grr, gri, gO and K. These are the gaps.
vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP keymaps, bound per buffer so they exist only where a server ran",
  callback = function(ev)
    local opts = function(desc) return { buffer = ev.buf, desc = desc } end
    map("n", "gd", vim.lsp.buf.definition, opts("LSP: definition"))
    map("n", "gD", vim.lsp.buf.declaration, opts("LSP: declaration"))
    map("n", "gy", vim.lsp.buf.type_definition, opts("LSP: type definition"))
    map("n", "<C-c><C-d>", vim.diagnostic.open_float, opts("LSP: line diagnostics"))
    map("n", "<C-c><C-f>", function() vim.lsp.buf.format({ async = true }) end,
      opts("LSP: format buffer"))
  end,
})

-- =====================
-- UI / Theme
-- =====================
vim.opt.background = "dark"
vim.cmd.colorscheme("modus")

-- Let the terminal provide the background, matching the Vim config.
for _, group in ipairs({ "Normal", "NormalNC", "NormalFloat", "SignColumn", "EndOfBuffer" }) do
  vim.api.nvim_set_hl(0, group, { bg = "none" })
end

vim.opt.laststatus = 2
vim.g.lightline = { colorscheme = "one" }
