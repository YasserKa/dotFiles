-- This will run last in the setup process and is a good place to configure
-- things like custom filetypes. This just pure lua so anything that doesn't
-- fit in the normal config locations above can go here

-- Set up custom filetypes
vim.filetype.add {
  extension = {
    foo = "fooscript",
  },
  filename = {
    ["Foofile"] = "fooscript",
  },
  pattern = {
    ["~/%.config/foo/.*"] = "fooscript",
  },
}

-- Unmap Astronvim mappings
-- unmap = function(mapping)
-- 	vim.cmd("silent! unmap " .. mapping)
-- end
--
-- unmap("<Leader>e") --  explorer binding
--
-- remove highlighting after yanking (bugged)
local id = vim.api.nvim_create_augroup("highlightyank", { clear = false })
vim.api.nvim_del_augroup_by_id(id)

vim.api.nvim_create_augroup("my_skeletons", { clear = true })
vim.api.nvim_create_autocmd("BufNewFile", {
  desc = "Skeleton",
  group = "my_skeletons",
  pattern = { "*.sh" },
  command = "0r ~/.config/nvim/skeletons/skeleton.sh | exe 'normal jo' | startinsert",
})
vim.api.nvim_create_autocmd("BufNewFile", {
  desc = "Skeleton",
  group = "my_skeletons",
  pattern = { "*.bash" },
  command = "0r ~/.config/nvim/skeletons/skeleton.bash | exe 'normal jo' | startinsert",
})

vim.api.nvim_create_autocmd("BufNewFile", {
  desc = "Skeleton",
  group = "my_skeletons",
  pattern = { "tmp.py" },
  command = "0r ~/.config/nvim/skeletons/skeleton.py | exe 'normal jo' | startinsert",
})

vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  pattern = { "*.py" },
  callback = function(args)
    local first_line_file = vim.api.nvim_buf_get_lines(args.buf, 0, 1, false)[1]
    if first_line_file:match "^#%s*%%%%" then
      vim.api.nvim_create_autocmd({ "BufWritePost" }, {
        callback = function()
          local file_path = vim.fn.expand "%:p"

          local payload = {
            file_path = file_path,
          }
          local json = vim.fn.json_encode(payload)
          local url = "http://localhost:" .. vim.g.my_free_port .. "/send_save_signal"
          local cmd = string.format("curl -X GET -H \"Content-Type: application/json\" -d '%s' %s", json, url)
          vim.fn.jobstart(cmd, { detach = true })
        end,
      })

      local function find_pyproject_root()
        local root_file = vim.fs.find("pyproject.toml", {
          upward = true,
          path = vim.api.nvim_buf_get_name(0),
        })[1]
        return root_file and vim.fs.dirname(root_file) or nil
      end

      local function find_free_port(start_port)
        local port = start_port or 5000

        -- If jupyter running in the driectory, get the PWD of where it's running
        local root = find_pyproject_root()
        if root then
          local handle = io.popen(
            "ps aux | grep '"
              .. root
              .. "' | grep 'jupyter_selenium' |  grep -v grep | grep -v environments | awk -F' ' '{print $NF}'"
          )
          if not handle then return nil end
          local result = handle:read "*a"
          -- The result returns an empty line at the end sometimes
          result = result:gsub("%s+$", "")
          if result ~= "" then return result end
        end
        while true do
          local handle = io.popen("lsof -iTCP:" .. port .. " -sTCP:LISTEN")
          if not handle then return nil end
          local result = handle:read "*a"
          handle:close()
          if result == "" then
            return port
          else
            port = port + 1
          end
        end
      end

      -- Set a global variable for the current Neovim session
      vim.g.my_free_port = find_free_port(5000)

      -- Get current Jupyter-style cell index at cursor (0-based)
      local function get_cell_index(line_num)
        local bufnr = vim.api.nvim_get_current_buf()
        local current_line = line_num

        local lines = vim.api.nvim_buf_get_lines(bufnr, 0, current_line, false)

        local cell = -1
        for _, line in ipairs(lines) do
          if line:match "^#%s*%%%%%s*$" or line:match "^#%s*%%%%%s*%[markdown%]%s*$" then cell = cell + 1 end
        end

        return cell
      end

      local function execute_cells(line_nums)
        local file_path = vim.fn.expand "%:p"
        local cell_indices = {}
        for _, line_num in ipairs(line_nums) do
          table.insert(cell_indices, get_cell_index(line_num))
        end

        local payload = {
          index = cell_indices,
          file_path = file_path,
        }
        local json = vim.fn.json_encode(payload)
        local url = "http://localhost:" .. vim.g.my_free_port .. "/run_cells"
        local cmd = string.format("curl -X GET -H \"Content-Type: application/json\" -d '%s' %s", json, url)
        vim.fn.jobstart(cmd, { detach = true })
      end

      local function execute_cell()
        local cursor = vim.api.nvim_win_get_cursor(0) -- {line, col}
        execute_cells { cursor[1] }
      end

      -- Execute visually selected cells
      Execute_selected_cells = function()
        -- Leave visual mode to update the "<" and ">" marks
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", true)
        local start_line = vim.api.nvim_buf_get_mark(0, "<")[1]
        local end_line = vim.api.nvim_buf_get_mark(0, ">")[1]

        print(start_line)
        if start_line == 0 or end_line == 0 then
          print "No visual selection"
          return
        end
        local lines = {}
        -- Add first cell if the # %% isn't selected
        local first_line = vim.api.nvim_buf_get_lines(0, start_line - 1, start_line, false)[1]
        if not first_line:find "^# %%" then table.insert(lines, start_line) end

        for line_num = start_line, end_line do
          local line = vim.api.nvim_buf_get_lines(0, line_num - 1, line_num, false)[1]
          if line:find "^# %%" then table.insert(lines, line_num) end
        end
        execute_cells(lines)
      end

      local function execute_all_cells()
        local file_path = vim.fn.expand "%:p"

        local payload = {
          file_path = file_path,
        }
        local json = vim.fn.json_encode(payload)
        local url = "http://localhost:" .. vim.g.my_free_port .. "/run_all_cells"
        local cmd = string.format("curl -X GET -H \"Content-Type: application/json\" -d '%s' %s", json, url)
        vim.fn.jobstart(cmd, { detach = true })
      end

      local function goto_cell()
        local file_path = vim.fn.expand "%:p"
        local cursor = vim.api.nvim_win_get_cursor(0)
        local cell_index = get_cell_index(cursor[1])

        local payload = {
          index = cell_index,
          file_path = file_path,
        }
        local json = vim.fn.json_encode(payload)
        local url = "http://localhost:" .. vim.g.my_free_port .. "/goto_cell"
        local cmd = string.format("curl -X GET -H \"Content-Type: application/json\" -d '%s' %s", json, url)
        vim.fn.jobstart(cmd, { detach = true })
      end

      local function restart_kernel()
        local file_path = vim.fn.expand "%:p"

        local payload = {
          file_path = file_path,
        }
        local json = vim.fn.json_encode(payload)
        local url = "http://localhost:" .. vim.g.my_free_port .. "/restart_kernel"
        local cmd = string.format("curl -X GET -H \"Content-Type: application/json\" -d '%s' %s", json, url)
        vim.fn.jobstart(cmd, { detach = true })
      end

      local function start_jupyter_selenium_api()
        vim.g.my_free_port = find_free_port(5000)
        local filename = vim.api.nvim_buf_get_name(0)

        local root = find_pyproject_root()
        local handle = io.popen(
          "ps aux | grep '"
            .. root
            .. "' | grep 'jupyter_selenium' |  grep -v grep | grep -v environments | awk -F' ' '{print $NF}'"
        )
        if not handle then return nil end
        local result = handle:read "*a"
        -- The result returns an empty line at the end sometimes
        result = result:gsub("%s+$", "")
        if result == "" then
          local cmd = string.format("$HOME/.config/jupyter/bin/jupyter_selenium '%s' %d", filename, vim.g.my_free_port)
          vim.fn.jobstart(cmd, { detach = true })
        else
          local file_path = vim.fn.expand "%:p"

          local payload = {
            file_path = file_path,
          }
          local json = vim.fn.json_encode(payload)
          local url = "http://localhost:" .. vim.g.my_free_port .. "/run"
          local cmd = string.format("curl -X POST -H \"Content-Type: application/json\" -d '%s' %s", json, url)
          vim.fn.jobstart(cmd, { detach = true })
        end
      end

      local function shutdown_jupyter_selenium_api()
        local url = "http://localhost:" .. vim.g.my_free_port .. "/shutdown"
        local cmd = string.format("curl -X POST %s", url)
        vim.fn.jobstart(cmd, { detach = true })
      end

      local function delete_cell()
        local bufnr = vim.api.nvim_get_current_buf()
        local cursor = vim.api.nvim_win_get_cursor(0)
        local cur_line = cursor[1]

        local line_count = vim.api.nvim_buf_line_count(bufnr)

        -- find start marker
        local start_line = 1
        for l = cur_line, 1, -1 do
          local text = vim.api.nvim_buf_get_lines(bufnr, l - 1, l, false)[1]
          if text:match "^#%s*%%%%" then
            start_line = l
            break
          end
        end

        -- find end marker
        local end_line = line_count + 1
        for l = cur_line + 1, line_count do
          local text = vim.api.nvim_buf_get_lines(bufnr, l - 1, l, false)[1]
          if text:match "^#%s*%%%%" then
            end_line = l
            break
          end
        end

        -- delete whole cell
        vim.api.nvim_buf_set_lines(bufnr, start_line - 1, end_line - 1, false, {})
      end

      local wk = require "which-key"
      wk.add {
        { "<localLeader>n", group = "Jupyter" },
        { "<localLeader>nc", execute_cell, desc = "Execute cell/s" },
        {
          "<localLeader>nC",
          function()
            execute_cell()
            vim.cmd [[call search('^# %%\s*$')]]
          end,
          desc = "Execute cell and jump to next cell",
        },
        { "<localLeader>nd", delete_cell, desc = "Delete cell" },
        { "<localLeader>ng", goto_cell, desc = "Goto cell" },
        {
          "<localLeader>nJ",
          start_jupyter_selenium_api,
          desc = "Start API",
        },
        {
          "<localLeader>nS",
          shutdown_jupyter_selenium_api,
          desc = "Shutdown API",
        },
        { "<localLeader>nR", restart_kernel, desc = "Restart Kernel" },
        { "<localLeader>nr", execute_all_cells, desc = "Execute all cells" },
      }

      wk.add {
        { "<localLeader>n", group = "Jupyter Ascending", mode = "v" },
        { "<localLeader>nc", ":lua Execute_selected_cells()<CR>", desc = "Execute selected cells", mode = "v" },
      }
    end
  end,
})

vim.api.nvim_create_augroup("user_mail", {
  clear = true,
})
-- heirline starts flickering while using vimtex & cmdheight=0
-- Problem from cmdheight
-- https://github.com/lervag/vimtex/issues/2516
-- https://github.com/AstroNvim/AstroNvim/issues/1124
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "tex" },
  command = "set cmdheight=1",
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "gitcommit" },
  command = "startinsert | set spell | inoremap <C-c><C-c> <cmd>wq<cr>",
})

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  desc = "Settings for various files",
  group = "user_mail",
  pattern = { "neomutt-*", "tuir*", "qutebrowser-editor-*", "*eml" },
  command = "set spell textwidth=100",
})
vim.api.nvim_create_autocmd({ "BufRead" }, {
  desc = "Settings for mail files",
  group = "user_mail",
  pattern = { "neomutt-*", "*eml" },
  command = "normal )j",
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "markdown", "tex" },
  command = "set spell textwidth=100",
})

vim.api.nvim_create_augroup("user_markdown", {
  clear = true,
})

-- Yank org links to files
_G.encodeChar = function(chr) return string.format("%%%X", string.byte(chr)) end

_G.encodeString = function(str)
  local output, _ = string.gsub(str, "[^%w]", encodeChar)
  return output
end

_G.YankOrgLink = function()
  local r, _ = unpack(vim.api.nvim_win_get_cursor(0))
  local cmd = '"${TERMINAL}" --directory "'
    .. vim.fn.expand "%:p:h"
    .. '" --detach -e "${EDITOR}" +'
    .. r
    .. ' "'
    .. vim.fn.expand "%:t"
    .. '"'
  local encoded_org_link = "[[" .. "link-handler://" .. encodeString(cmd) .. "][" .. vim.fn.expand "%:t" .. "]]"
  vim.fn.setreg("+", encoded_org_link)
end
vim.api.nvim_create_user_command("YankOrgLink", _G.YankOrgLink, {})

_G.WatchFile = function()
  vim.cmd(
    "silent !chmod +x "
      .. vim.fn.expand "%:p"
      .. '&& "${TERMINAL}" --detach --directory "'
      .. vim.fn.expand "%:p:h"
      .. '" bash -c \'echo "'
      .. vim.fn.expand "%:t"
      .. "\" | entr -c /_'"
  )
end
vim.api.nvim_create_user_command("WatchFile", _G.WatchFile, {})

vim.keymap.set("x", "@", '":norm @" . getcharstr() . "<cr>"', { expr = true })
-- vim.keymap.set("n", "<Leader>c", MiniBufremove.delete) -- Remove buffer, but keep split
vim.keymap.set("n", "<Leader>C", "<cmd>bdelete<cr>")
vim.keymap.set({ "n" }, "<C-Space>", "<cmd>bdelete<cr>")

-- {{{
require("luasnip.loaders.from_lua").lazy_load { paths = "~/.config/nvim/snippets" }

vim.keymap.set("n", "<Leader><Leader>s", "<cmd>source ~/.config/nvim/snippets/python.lua")
require("luasnip").config.set_config { -- Setting LuaSnip config

  -- Enable autotriggered snippets
  enable_autosnippets = true,
}
-- }}}
vim.keymap.set("n", "<C-S-h>", require("smart-splits").resize_left)
vim.keymap.set("n", "<C-S-j>", require("smart-splits").resize_down)
vim.keymap.set("n", "<C-S-k>", require("smart-splits").resize_up)
vim.keymap.set("n", "<C-S-l>", require("smart-splits").resize_right)

vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "J", "mzJ`z") -- Join line without moving cursor
vim.keymap.set("n", "N", "Nzzzv")
-- Keep visual selection after indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

local Path = require "plenary.path"
require("luasnip.loaders.from_lua").load { paths = "~/.config/nvim/snippets" }
require("luasnip.loaders.from_snipmate").lazy_load()

function vim_opt_toggle(opt, on, off, name)
  local is_off = vim.opt[opt]:get() == off
  vim.opt[opt] = is_off and on or off
  M.quick_notification(name .. " " .. (is_off and "Enabled" or "Disabled"))
end

vim.keymap.set("n", "<localLeader>c", function() vim_opt_toggle("conceallevel", 2, 0, "Conceal") end)
local luasnip = require "luasnip"
vim.keymap.set({ "i" }, "<C-e>", function()
  if luasnip.choice_active() then
    luasnip.change_choice(1)
  else
    require("readline").end_of_line()
  end
end)

require "syntax-tree-surfer" -- {{{
-- Syntax Tree Surfer
-- local opts = { noremap = true, silent = true }

-- Normal Mode Swapping:
-- Swap The Master Node relative to the cursor with it's siblings, Dot Repeatable
-- vim.keymap.set("n", "vU", function()
-- 	vim.opt.opfunc = "v:lua.STSSwapUpNormal_Dot"
-- 	return "g@l"
-- end, { silent = true, expr = true })
-- vim.keymap.set("n", "vD", function()
-- 	vim.opt.opfunc = "v:lua.STSSwapDownNormal_Dot"
-- 	return "g@l"
-- end, { silent = true, expr = true })

-- -- Swap Current Node at the Cursor with it's siblings, Dot Repeatable
-- vim.keymap.set("n", "vd", function()
-- 	vim.opt.opfunc = "v:lua.STSSwapCurrentNodeNextNormal_Dot"
-- 	return "g@l"
-- end, { silent = true, expr = true })
-- vim.keymap.set("n", "vu", function()
-- 	vim.opt.opfunc = "v:lua.STSSwapCurrentNodePrevNormal_Dot"
-- 	return "g@l"
-- end, { silent = true, expr = true })

-- -- Visual Selection from Normal Mode
-- vim.keymap.set("n", "<Leader>su", "<cmd>STSSelectMasterNode<cr>", opts)
-- vim.keymap.set("n", "vn", "<cmd>STSSelectCurrentNode<cr>", opts)
-- -- O in insert mode
-- vim.keymap.set("n", "gO", function()
-- 	require("syntax-tree-surfer").go_to_top_node_and_execute_commands(
-- 		false,
-- 		{ "normal! O", "normal! O", "startinsert" }
-- 	)
-- end, opts)

-- Select Nodes in Visual Mode
--vim.keymap.set("x", "J", "<cmd>STSSelectNextSiblingNode<cr>", opts)
--vim.keymap.set("x", "K", "<cmd>STSSelectPrevSiblingNode<cr>", opts)
--vim.keymap.set("x", "H", "<cmd>STSSelectParentNode<cr>", opts)
--vim.keymap.set("x", "L", "<cmd>STSSelectChildNode<cr>", opts)

---- Swapping Nodes in Visual Mode
--vim.keymap.set("x", "<A-j>", "<cmd>STSSwapNextVisual<cr>", opts)
--vim.keymap.set("x", "<A-k>", "<cmd>STSSwapPrevVisual<cr>", opts)
---- Syntax Tree Surfer V2 Mappings
---- Targeted Jump with virtual_text
local sts = require "syntax-tree-surfer"
--vim.keymap.set("n", "gsv", function() -- only jump to variable_declarations
--	sts.targeted_jump({ "variable_declaration" })
--end, opts)
--vim.keymap.set("n", "gfu", function() -- only jump to functions
--	sts.targeted_jump({ "function", "arrrow_function", "function_definition" })
--	--> In this example, the Lua language schema uses "function",
--	--  when the Python language uses "function_definition"
--	--  we include both, so this keymap will work on both languages
--end, opts)
--vim.keymap.set("n", "gif", function() -- only jump to if_statements
--	sts.targeted_jump({ "if_statement" })
--end, opts)
--vim.keymap.set("n", "gfo", function() -- only jump to for_statements
--	sts.targeted_jump({ "for_statement" })
--end, opts)
--vim.keymap.set("n", "gj", function() -- jump to all that you specify
--	sts.targeted_jump({
--		"function",
--		"if_statement",
--		"else_clause",
--		"else_statement",
--		"elseif_statement",
--		"for_statement",
--		"while_statement",
--		"switch_statement",
--	})
--end, opts)

---------------------------------
---- filtered_jump --
---- "default" means that you jump to the default_desired_types or your lastest jump types
---- vim.keymap.set("n", "<A-n>", function()
---- 	sts.filtered_jump("default", true) --> true means jump forward
---- end, opts)
---- vim.keymap.set("n", "<A-p>", function()
---- 	sts.filtered_jump("default", false) --> false means jump backwards
---- end, opts)
--vim.keymap.set("n", "gj", function() -- jump to all that you specify
--	sts.targeted_jump({
--		"function",
--		"if_statement",
--		"else_clause",
--		"else_statement",
--		"elseif_statement",
--		"for_statement",
--		"while_statement",
--		"switch_statement",
--	})
--end, opts)

-- Jump between nodes
-- vim.keymap.set("n", "<A-n>", function()
-- 	sts.filtered_jump({
-- 		"function",
-- 		"class",
-- 		"arrow_function",
-- 		"variable_assignment",
-- 		"function_definition",
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_statement",
-- 		"elseif_statement",
-- 		"for_statement",
-- 		"while_statement",
-- 		"switch_statement",
-- 	}, true)
-- end, opts)
-- vim.keymap.set("n", "<A-p>", function()
-- 	sts.filtered_jump({
-- 		"function",
-- 		"class",
-- 		"arrow_function",
-- 		"function_definition",
-- 		"variable_assignment",
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_statement",
-- 		"elseif_statement",
-- 		"for_statement",
-- 		"while_statement",
-- 		"switch_statement",
-- 	}, false)
-- end, opts)

-- Navigate between elements
vim.keymap.set("n", "]f", function()
  sts.filtered_jump({
    "if_statement",
  }, true)
end, opts)
vim.keymap.set("n", "[f", function()
  sts.filtered_jump({
    "if_statement",
  }, false)
end, opts)

-- vim.keymap.set("n", "]c", function()
--   sts.filtered_jump({
--     "class",
--   }, true)
-- end, opts)
-- vim.keymap.set("n", "[c", function()
--   sts.filtered_jump({
--     "class",
--   }, false)
-- end, opts)

vim.keymap.set("n", "]/", function()
  sts.filtered_jump({
    "comment",
  }, true)
end, opts)
vim.keymap.set("n", "[/", function()
  sts.filtered_jump({
    "comment",
  }, false)
end, opts)

-------------------------------
-- jump with limited targets --
-- jump to sibling nodes only
-- vim.keymap.set("n", "-", function()
-- 	sts.filtered_jump({
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_statement",
-- 	}, false, { destination = "siblings" })
-- end, opts)
-- vim.keymap.set("n", "=", function()
-- 	sts.filtered_jump({ "if_statement", "else_clause", "else_statement" }, true, { destination = "siblings" })
-- end, opts)

-- local gen_spec = require("mini.ai").gen_spec
-- require("mini.ai").setup({
-- 	custom_textobjects = {
-- 		["*"] = gen_spec.pair("*", "*", { type = "greedy" }),
-- 		["_"] = gen_spec.pair("_", "_", { type = "greedy" }),
-- 		B = gen_spec.treesitter({ a = "@block.outer", i = "@block.inner" }),
-- 		C = gen_spec.treesitter({ a = "@conditional.outer", i = "@conditional.inner" }),
-- 		F = gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
-- 		L = gen_spec.treesitter({ a = "@loop.outer", i = "@loop.inner" }),
-- 		P = gen_spec.treesitter({ a = "@parameter.outer", i = "@parameter.inner" }),
-- 		x = gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
-- 	},
-- })
-- jump to parent or child nodes only
-- vim.keymap.set("n", "_", function()
-- 	sts.filtered_jump({
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_statement",
-- 	}, false, { destination = "parent" })
-- end, opts)
-- vim.keymap.set("n", "+", function()
-- 	sts.filtered_jump({
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_st atement",
-- 	}, true, { destination = "children" })
-- end, opts)

-- -- Setup Function example:
-- -- These are the default options:
-- require("syntax-tree-surfer").setup({
-- 	highlight_group = "STS_highlight",
-- 	disable_no_instance_found_report = false,
-- 	default_desired_types = {
-- 		"function",
-- 		"arrow_function",
-- 		"function_definition",
-- 		"if_statement",
-- 		"else_clause",
-- 		"else_statement",
-- 		"elseif_statement",
-- 		"for_statement",
-- 		"while_statement",
-- 		"switch_statement",
-- 	},
-- 	left_hand_side = "fdsawervcxqtzb",
-- 	right_hand_side = "jkl;oiu.,mpy/n",
-- 	icon_dictionary = {
-- 		["if_statement"] = "",
-- 		["else_clause"] = "",
-- 		["else_statement"] = "",
-- 		["elseif_statement"] = "",
-- 		["for_statement"] = "ﭜ",
-- 		["while_statement"] = "ﯩ",
-- 		["switch_statement"] = "ﳟ",
-- 		["function"] = "",
-- 		["function_definition"] = "",
-- 		["variable_declaration"] = "",
-- 	},
-- })

-- Autoindent on entering insert mode
vim.keymap.set("n", "i", function()
  if #vim.fn.getline "." == 0 then
    return [["_cc]]
  else
    return "i"
  end
end, { expr = true, desc = "properly indent on empty line when insert" })

--  }}}
-- {{{ vim-expand-region
vim.keymap.set({ "n", "x" }, "+", "<Plug>(expand_region_expand)")
vim.keymap.set({ "n", "x" }, "_", "<Plug>(expand_region_shrink)")
-- }}}
-- {{{ vim-mundo
-- Enable persistent undo so that undo history persists across vim sessions
-- set undofile
vim.opt.undofile = true
vim.keymap.set({ "n" }, "<Leader>eu", "<cmd>MundoToggle<cr>")
-- }}}
-- {{{ vim-cool
-- Show number of matches in command-line
vim.g["CoolTotalMatches"] = 1
-- }}}
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "help", "man" },
  callback = function()
    vim.cmd [[
     nnoremap <silent> <buffer> q :close<CR>
     nnoremap <silent> <buffer> <esc> :close<CR>
     set nobuflisted
     ]]
  end,
})

-- Updated version of utils.system_open adding fn.expand, so paths with ~
-- are expanded
function system_open(path)
  local cmd
  if vim.fn.has "win32" == 1 and vim.fn.executable "explorer" == 1 then
    cmd = "explorer"
  elseif vim.fn.has "unix" == 1 and vim.fn.executable "xdg-open" == 1 then
    cmd = "xdg-open"
  elseif (vim.fn.has "mac" == 1 or vim.fn.has "unix" == 1) and vim.fn.executable "open" == 1 then
    cmd = "open"
  end

  local file_path = vim.fn.expand "<cfile>"
  if not cmd then M.notify("Available system opening tool not found!", "error") end
  vim.fn.jobstart({ cmd, path or vim.fn.expand(file_path) }, { detach = true })
end

-- }}}
--
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "markdown" },
  callback = function()
    vim.cmd [[
     hi MyStrikethrough gui=strikethrough
     call matchadd('MyStrikethrough', '\~\~\zs.\+\ze\~\~')
     call matchadd('Conceal',  '\~\~\ze.\+\~\~', 10, -1, {'conceal':''})
     call matchadd('Conceal',  '\~\~.\+\zs\~\~\ze', 10, -1, {'conceal':''})

     hi MyUnderlineMatch gui=underline
     call matchadd('MyUnderlineMatch', '__\zs[^X]\+\ze__')
     call matchadd('Conceal',  '__\ze[^X]\+__', 10, -1, {'conceal':''})
     call matchadd('Conceal',  '__[^X]\+\zs__\ze', 10, -1, {'conceal':''})

     ]]
  end,
})
-- Visually select last inserted text
vim.keymap.set("n", "gl", "`[v`]")
-- Binding to open command-line window
vim.o.cedit = "<C-Y>"
-- Prevent csv for key map overriding space, S-h, S-l
vim.g["csv_nomap_space"] = 1
vim.g["csv_nomap_h"] = 1
vim.g["csv_nomap_l"] = 1
-- {{{ vim-maximizer
vim.keymap.set("n", "<C-w>m", "<cmd>MaximizerToggle!<CR>")
-- }}}
vim.api.nvim_exec2(
  [[
 nnoremap ; :
 nnoremap : ;
 augroup TMP_FILES
 autocmd!
 autocmd BufRead,BufNewFile tmp.* inoremap <C-c><C-c> <esc>:q<cr>
 autocmd BufRead,BufNewFile tmp.* set noswapfile
 autocmd ExitPre tmp.* :w
 augroup END
 let g:python3_host_prog  = '/bin/python'
 let g:ipython_cell_run_command	= '%run -t "{filepath}"'

 xnoremap gcc :Commentary<cr>
 "  {{{ markdown-preview.nvim
 let g:mkdp_command_for_global = 1
 let g:mkdp_page_title = '${name}'
 let g:mkdp_auto_close = 0
 nmap <Leader>em <Plug>MarkdownPreviewToggle

 " open page in new window
 function! OpenNewBrowserWindow(url)
 execute "silent ! qutebrowser --target window " . a:url
 endfunction

 " https://github.com/Ron89/thesaurus_query.vim
 let g:tq_openoffice_en_file =  "../../spell/MyThes-1.0/th_en_US_new"
 let g:tq_mthesaur_file =  "~/.config/nvim/spell/mthesaur.txt"
 let g:tq_enabled_backends= ["datamuse_com", "openoffice_en", "mthesaur_txt"]

 let g:mkdp_browserfunc = 'OpenNewBrowserWindow'
 " }}}
 " {{{ vimtex
 let g:vimtex_compiler_silent = 1
 " Use nabla.nvim
 " let g:vimtex_syntax_conceal_disable=1
 let g:tex_conceal = 'abdg'
 let g:vimtex_syntax_conceal = {
 \ 'accents': 1,
 \ 'ligatures': 1,
 \ 'cites': 1,
 \ 'fancy': 1,
 \ 'greek': 0,
 \ 'math_bounds': 0,
 \ 'math_delimiters': 0,
 \ 'math_fracs': 0,
 \ 'math_super_sub': 0,
 \ 'math_symbols': 0,
 \ 'sections': 0,
 \ 'styles': 1,
 \}
 let g:vimtex_view_method = 'zathura'
 let g:vimtex_fold_enabled = 1
 let g:vimtex_compiler_method = "latexmk"

 let g:vimtex_compiler_latexmk = {
 \ 'aux_dir' : './tex_output',
 \ 'options' : [
 \   '-xelatex',
 \   '-verbose',
 \   '-outdir=./tex_output',
 \   '-file-line-error',
 \   '-shell-escape',
 \   '-synctex=1',
 \   '-interaction=nonstopmode',
 \ ],
 \}
 " }}}
]],
  {}
)
