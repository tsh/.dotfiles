local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.config/nvim/plugged')
-- DEV
Plug 'jbyuki/one-small-step-for-vimkind'
Plug 'williamboman/nvim-lsp-installer'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug ('hrsh7th/cmp-nvim-lua', { tag = 'main'})
Plug 'jose-elias-alvarez/null-ls.nvim'

Plug 'windwp/nvim-autopairs'
Plug 'numToStr/Comment.nvim'


Plug 'mfussenegger/nvim-dap'
Plug 'rcarriga/nvim-dap-ui'
-- Snip
Plug 'L3MON4D3/LuaSnip'
Plug 'rafamadriz/friendly-snippets'
Plug 'saadparwaiz1/cmp_luasnip'
-- UI
Plug 'nvim-lua/plenary.nvim'
Plug ('nvim-telescope/telescope.nvim', { tag = '0.1.0' })
Plug 'ryanoasis/vim-devicons'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rcarriga/nvim-notify' Plug 'scrooloose/nerdcommenter'
Plug 'sbdchd/neoformat'
Plug 'scrooloose/nerdtree'
Plug 'neomake/neomake'
Plug 'machakann/vim-highlightedyank'
Plug 'tmhedberg/SimpylFold'
--[[ Plug 'airblade/vim-gitgutter' ]]
Plug 'lewis6991/gitsigns.nvim'
Plug 'yuttie/comfortable-motion.vim'
Plug 'preservim/nerdtree'

-- COLORSCHEMES
Plug 'morhetz/gruvbox'

vim.call('plug#end')

require('tsh')

-- NULL_LS
local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.black,

        null_ls.builtins.completion.spell,

	null_ls.builtins.diagnostics.flake8
    },
})

require'lspconfig'.pylsp.setup{
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = {
          ignore = {'W391'},
          maxLineLength = 100
        }
      }
    }
  }
}


vim.opt.clipboard = 'unnamedplus'

local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- resize 
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)


vim.cmd [[
  syntax enable
  set nocompatible  

  filetype on
  filetype indent on
  filetype plugin on
  set fileencoding=utf-8

set encoding=utf-8
set incsearch
set hlsearch
set ignorecase
set smartcase
 
set visualbell
set backspace=indent,eol,start
 

syntax enable

:au FocusLost * silent! wa

colorscheme gruvbox
set background=dark 
    ]]

vim.cmd[[
"" AIRLINE
"" let g:airline_theme='wombat' 
"" autoformat
" Enable alignment
"" let g:neoformat_basic_format_align = 1
" Enable tab to spaces conversion
"" let g:neoformat_basic_format_retab = 1
" Enable trimmming of trailing whitespace
"" let g:neoformat_basic_format_trim = 1
  ]]


vim.cmd[[
"" CODE CHECK 
let g:neomake_python_enabled_makers = ['pylint']

""FOLD
set foldlevel=99
"" SIMPLE FOLDING
let g:SimpylFold_docstring_preview = 1
]]

vim.cmd[[
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>
let NERDTreeShowHidden=1
]]

  vim.bo.autoindent = true
  vim.go.showmatch = true
  vim.go.termguicolors = true
  vim.wo.number = true

  vim.api.nvim_set_option('undofile', true)
  vim.api.nvim_set_option('undodir', '$HOME/.vim_undo_files')
  vim.api.nvim_set_option('undolevels', 3000)


-- DIAGNOSTICS

 local signs = {
    { name = "DiagnosticSignError", text = "" },
    { name = "DiagnosticSignWarn", text = "" },
    { name = "DiagnosticSignHint", text = "" },
    { name = "DiagnosticSignInfo", text = "" },
  }

  for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end

  local config = {
    -- Display diagnostic message on same line
    virtual_text = false,
    -- show signs
    signs = {
      active = signs,
    },
    update_in_insert = true,
    underline = true,
    severity_sort = true,
    float = {
      focusable = false,
      style = "minimal",
      border = "rounded",
      source = "always",
      header = "",
      prefix = "",
    },
  }

  vim.diagnostic.config(config)

  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded",
  })

  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "rounded",
  })


  -- LSP


local nvim_lsp = require('lspconfig')
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
  -- Mappings.
  local opts = { noremap=true, silent=true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  --buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  --buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  --buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  --buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  --buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  --buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  --buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  --buf_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  --buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  --buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  --buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  --buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

-- Highlight lexem under the cursor and it's usage. TODO: Fix for lua
  if client.server_capabilities.documentHighlight then
    vim.api.nvim_exec(
      [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
      false
    )
  end

--  local notify = require("notify")
--  local root_dir = vim.inspect(vim.lsp.buf.list_workspace_folders())
  -- notify(root_dir, 'info', {title = ' LSP root at:', timeout = 7000})
end


local lsp_capabilities = vim.lsp.protocol.make_client_capabilities()
local capabilities = require('cmp_nvim_lsp').default_capabilities(lsp_capabilities)
local lsp_config = require('lspconfig')
local lsp_installer = require("nvim-lsp-installer")


lsp_installer.on_server_ready(function(server)

  if server.name == "sumneko_lua" then
  local opts ={
	  on_attach = on_attach,
	  capabilities = capabilities
  }
	local sumneko_opts = {
	settings = {
		Lua = { diagnostics = {
				globals = { "vim" },
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.stdpath("config") .. "/lua"] = true, 
	}, }, }, }, }
   opts = vim.tbl_deep_extend("force", sumneko_opts, opts)
  server:setup(opts)
  end
end)



vim.lsp.set_log_level("debug")


-- AUTOCOMPLETE CMP
--
 local kind_icons = {
  Text = "",
  Method = "m",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
} -- find more here: https://www.nerdfonts.com/cheat-sheet

 local check_backspace = function()
  local col = vim.fn.col "." - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
end

  local luasnip = require("luasnip")
  local cmp = require'cmp'
    cmp.setup({
    snippet = {
      expand = function(args)
        --vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
         require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
      ["<Tab>"] = cmp.mapping(function(fallback)
	      if cmp.visible() then
		cmp.select_next_item()
	      elseif luasnip.expandable() then
		luasnip.expand()
	      elseif luasnip.expand_or_jumpable() then
		luasnip.expand_or_jump()
	      elseif check_backspace() then
		fallback()
	      else
		fallback()
	      end
	    end, {"i", "s",}),
    }),
    formatting = {
	    fields = { "kind", "abbr", "menu" },
	    format = function(entry, vim_item)
	      -- Kind icons
	      vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
	      -- vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
	      vim_item.menu = ({
		nvim_lsp = "[LSP]",
		nvim_lua = "[NVIM_LUA]",
		luasnip = "[Snippet]",
		buffer = "[Buffer]",
		path = "[Path]",
	      })[entry.source.name]
	      return vim_item
	    end,
  },
  sources = {
	  { name = 'nvim_lsp' },
	  { name = "nvim_lua" },
	  { name = 'luasnip' },
	  { name = 'buffer' },
	  { name = "path" },
  },
    experimental = {
	    -- show a suggestion as you type after cursor
	    ghost_text = false,
    }
  })

  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  
    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })


require("luasnip/loaders/from_vscode").lazy_load()
