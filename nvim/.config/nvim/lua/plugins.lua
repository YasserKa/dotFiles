vim.cmd [[packadd packer.nvim]]
return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    use 'https://github.com/gruvbox-community/gruvbox'

    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim', 'kyazdani42/nvim-web-devicons'}
    } }

    use 'https://github.com/rmagatti/goto-preview'
    use 'https://github.com/williamboman/mason.nvim'
    use 'https://github.com/neovim/nvim-lspconfig'

    -- Filetypes
    use 'https://github.com/YasserKa/vim-sxhkdrc'
    use 'https://github.com/fladson/vim-kitty'
    -- Editing
    -- Lua minimal modules
    use 'https://github.com/echasnovski/mini.nvim'
    -- Expand/shrink visual region via _ & +
    use 'https://github.com/terryma/vim-expand-region'

     use { 'kkoomen/vim-doge', run = ':call doge#install()' }
    use 'https://github.com/danymat/neogen'

    use 'https://github.com/lukas-reineke/indent-blankline.nvim'
    use 'https://github.com/romainl/vim-cool' -- Disable search highlighting when done

    use "https://github.com/nvim-treesitter/nvim-treesitter"
 
    use({'https://github.com/JoosepAlviste/nvim-ts-context-commentstring', after = 'nvim-treesitter' })
    use({'https://github.com/p00f/nvim-ts-rainbow', after = 'nvim-treesitter' })
    use({'https://github.com/nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-treesitter'})
    use({'https://github.com/nvim-treesitter/nvim-treesitter-context', after = 'nvim-treesitter' })

    -- Key bindings
    -- Readline Movement
    use 'https://github.com/tpope/vim-rsi'
    use 'https://github.com/folke/which-key.nvim'
    use 'https://github.com/tpope/vim-surround'
    use 'https://github.com/tpope/vim-unimpaired'

    use 'https://github.com/itchyny/lightline.vim'
    use 'https://github.com/maximbaz/lightline-ale'
    use 'https://github.com/tpope/vim-fugitive'
    -- Extenion for fugitive that allows to browse github from code
    use 'https://github.com/tpope/vim-rhubarb'
    use 'https://github.com/lewis6991/gitsigns.nvim'


    use 'https://github.com/windwp/nvim-autopairs'
    use 'https://github.com/tpope/vim-repeat'
    use 'https://github.com/wellle/targets.vim'
    use 'https://github.com/liuchengxu/vista.vim'
    use 'https://github.com/jeetsukumaran/vim-commentary'

    use 'https://github.com/puremourning/vimspector'
    use 'https://github.com/szw/vim-maximizer'
    use 'https://github.com/mrjones2014/smart-splits.nvim'  -- Smart split navigation & movement

    use 'https://github.com/simnalamburt/vim-mundo'

    use 'https://github.com/junegunn/fzf.vim'
    use 'https://github.com/dense-analysis/ale'
    use({'https://github.com/neoclide/coc.nvim', branch = 'release'})
    use 'https://github.com/SirVer/ultisnips'
    use 'https://github.com/L3MON4D3/LuaSnip'
    use 'https://github.com/honza/vim-snippets'
    use({'https://github.com/mattn/emmet-vim', ft = { {'html', 'blade.php', 'vue'}}})
    use 'https://github.com/plasticboy/vim-markdown'

    use({'https://github.com/jwalton512/vim-blade', ft = { 'blade.php'}})
    use({'https://github.com/posva/vim-vue', ft = { 'vue'}})

    use({'https://github.com/kristijanhusak/vim-dadbod-ui', ft = { 'sql'}})
    use({'https://github.com/tpope/vim-dadbod', ft = { 'sql'}})
    use({'https://github.com/vim-scripts/SQLComplete.vim', ft = { 'sql'}})

    use({ "iamcco/markdown-preview.nvim", run = "cd app && npm install", setup = function() vim.g.mkdp_filetypes = { "markdown", "plantuml" } end, ft = { "markdown", "plantuml" }, })

    use({'https://github.com/lervag/vimtex', ft = {"tex", "markdown"}})
    use 'https://github.com/dhruvasagar/vim-table-mode'
    use({'https://github.com/aklt/plantuml-syntax', ft = {  'markdown'}})

    use 'https://github.com/untitled-ai/jupyter_ascending.vim'

    -- Ipython
    use({'jpalardy/vim-slime', ft = {  'python' }})
    use({'hanschen/vim-ipython-cell', ft = {  'python' }})
end)
