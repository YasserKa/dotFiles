-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 500, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
      autopairs = true, -- enable autopairs at start
      cmp = true, -- enable completion at start
      diagnostics_mode = 3, -- diagnostic mode on start (0 = off, 1 = no signs/virtual text, 2 = no virtual text, 3 = on)
      highlighturl = true, -- highlight URLs at start
      notifications = true, -- enable notifications at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = true,
      underline = true,
    },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        relativenumber = true, -- sets vim.opt.relativenumber
        number = true, -- sets vim.opt.number
        spell = false, -- sets vim.opt.spell
        signcolumn = "auto", -- sets vim.opt.signcolumn to auto
        wrap = false, -- sets vim.opt.wrap
      },
      g = { -- vim.g.<key>
        -- configure global vim variables (vim.g)
        -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
        -- This can be found in the `lua/lazy_setup.lua` file
      },
    },
    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      n = {

        -- [";"] = { ":" },
        -- [":"] = { ";" },
        ["<Leader>ex"] = { "<cmd>Neotree toggle<cr>", desc = "Toggle Explorer" },
        ["gx"] = { "<cmd>lua system_open()<cr>", desc = "Open the file under cursor with system app" },
        ["<space><space>"] = { "<cmd>buffer#<cr>", desc = "Alternate buffer" },
        ["<localLeader>l"] = false,
        ["<localLeader>m"] = {
          function() require("nabla").popup() end,
          desc = "Preview Math",
        },
        ["<localLeader>M"] = {
          function() require("nabla").toggle_virt() end,
          desc = "Preview Math",
        },
        ["<Leader>."] = { "<cmd>cd %:p:h<cr>", desc = "Set CWD" },
        ["<Leader>bO"] = { "<cmd>silent :%bdelete | edit# | bdelete#<cr>", desc = "Remove all other buffers" },
        ["<Leader>gS"] = {
          function() require("gitsigns").stage_buffer() end,
          desc = "Stage Buffer",
        },
        ["<Leader>gU"] = {
          function() require("gitsigns").reset_buffer_index() end,
          desc = "Unstage Buffer",
        },
        ["<Leader>gc"] = { name = "Commit" },
        ["<Leader>gcc"] = { "<cmd>silent Git commit --quiet<CR>", noremap = true, desc = "Commit" },
        ["<Leader>a"] = { name = "Annotate" },
        ["<Leader>a<cr>"] = {
          function() require("neogen").generate() end,
          desc = "Current",
        },
        ["<Leader>ac"] = {
          function() require("neogen").generate { type = "class" } end,
          desc = "Class",
        },
        ["<Leader>af"] = {
          function() require("neogen").generate { type = "func" } end,
          desc = "Function",
        },
        ["<Leader>at"] = {
          function() require("neogen").generate { type = "type" } end,
          desc = "Type",
        },
        ["<Leader>aF"] = {
          function() require("neogen").generate { type = "file" } end,
          desc = "File",
        },
        ["<Leader>f?"] = { "<cmd>Telescope help_tags<cr>", desc = "Find help" },
        ["<Leader>f'"] = { "<cmd>Telescope marks<cr>", desc = "Marks" },
        ["<Leader>f."] = { "<cmd>Telescope resume<cr>", desc = "Open previous picker" },
        ["<Leader>fB"] = { "<cmd>Telescope bibtex<cr>", desc = "BibTeX" },
        ["<Leader>bb"] = { "<cmd>tabnew<cr>", desc = "New tab" },
        ["<Leader>bc"] = { "<cmd>BufferLinePickClose<cr>", desc = "Pick to close" },
        ["<Leader>bj"] = { "<cmd>BufferLinePick<cr>", desc = "Pick to jump" },
        ["<Leader>bt"] = { "<cmd>BufferLineSortByTabs<cr>", desc = "Sort by tabs" },

        ["<Leader>q"] = { "<cmd>exit<cr>", desc = "Delete buffer" },
        ["<Leader>Q"] = { "<cmd>quitall<cr>", desc = "Exit Vim" },

        -- Navigation
        L = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        H = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- Text
        ["j"] = { "gj" },
        ["k"] = { "gk" },
        ["gj"] = { "j" },
        ["gk"] = { "k" },
        ["<Leader>ff"] = {
          function() require("telescope.builtin").find_files() end,
          desc = "Search files",
        },
        ["<Leader>fw"] = {
          function() require("telescope.builtin").live_grep() end,
          desc = "Search words",
        },
        ["g:"] = { "g;" },
        ["q;"] = { "q:" },
        ["@;"] = { "@;" },
        ["<Leader>es"] = { "<cmd>split ~/.config/nvim/lua/user/init.lua<cr>", desc = "Split config file" },
        ["<Leader>ss"] = { "<cmd>source ~/.config/nvim/init.lua<cr>", desc = "Source config file" },

        ["<C-s>"] = { "<cmd>w!<cr>", desc = "Save File" },

        ["yod"] = {
          function() require("astronvim.utils.ui").toggle_diagnostics() end,
          desc = "Toggle diagnostics",
        },
        ["yog"] = {
          function() require("astronvim.utils.ui").toggle_signcolumn() end,
          desc = "Toggle signcolumn",
        },
        ["yoi"] = {
          function() require("astronvim.utils.ui").set_indent() end,
          desc = "Change indent setting",
        },
        ["yol"] = {
          function() require("astronvim.utils.ui").toggle_statusline() end,
          desc = "Toggle statusline",
        },
        ["yon"] = {
          function() require("astronvim.utils.ui").change_number() end,
          desc = "Change line numbering",
        },
        ["yos"] = {
          function() require("astronvim.utils.ui").toggle_spell() end,
          desc = "Toggle spellcheck",
        },
        ["yop"] = {
          function() require("astronvim.utils.ui").toggle_paste() end,
          desc = "Toggle paste mode",
        },
        ["yot"] = {
          function() require("astronvim.utils.ui").toggle_tabline() end,
          desc = "Toggle tabline",
        },
        ["you"] = {
          function() require("astronvim.utils.ui").toggle_url_match() end,
          desc = "Toggle URL highlight",
        },
        ["yow"] = {
          function() require("astronvim.utils.ui").toggle_wrap() end,
          desc = "Toggle wrap",
        },
        ["yoy"] = {
          function() require("astronvim.utils.ui").toggle_syntax() end,
          desc = "Toggle syntax highlight",
        },
      },
      v = {
        [";"] = { ":" },
        [":"] = { ";" },
      },
      i = {
        ["<C-l>"] = { "<c-g>u<Esc>[s1z=`]a<c-g>u", desc = "Fix last misspell" },
        ["<C-s>"] = { "<C-o>:w!<cr>", desc = "Save File" },
      },
      x = {
        ["il"] = { "g_o^", desc = "Inside line text object" },
        ["al"] = { "$o^", desc = "Around line text object" },
      },
      o = {
        -- line text-objects
        ["il"] = { ":normal vil<cr>", desc = "Inside line text object" },
        ["al"] = { ":normal val<cr>", desc = "Around line text object" },
      },
    },
  },
}
