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

    -- Parsers and queries only; Neovim owns the highlighter itself. The plugin
    -- does not support lazy loading, and :TSUpdate on every sync is what keeps
    -- the parsers on the versions this revision expects.
    --
    -- Needs the tree-sitter CLI, which is the tree-sitter-cli formula and not
    -- the tree-sitter one. Without it :TSUpdate cannot build a parser.
    {
      "nvim-treesitter/nvim-treesitter",
      branch = "main",
      lazy = false,
      build = ":TSUpdate",
    },
  })

-- =====================
-- Tree-sitter
-- =====================
-- Filetype names, which double as parser names for everything listed. sh is
-- left out for that reason: it parses as bash and would need the mapping.
--
-- Highlighting is per buffer, so a language absent here keeps the regex syntax
-- it already had rather than losing highlighting altogether.
local ts_filetypes = { "c", "cpp", "go", "lua", "odin", "python", "zig" }

require("nvim-treesitter").install(ts_filetypes)

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("rc_treesitter", { clear = true }),
  pattern = ts_filetypes,
  desc = "Tree-sitter highlighting, where a parser is installed",
  callback = function()
    pcall(vim.treesitter.start)
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
