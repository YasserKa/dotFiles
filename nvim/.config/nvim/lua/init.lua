require('mini.align').setup()
require('hop').setup({
    multi_windows = true,
})
vim.keymap.set('n', 'c', "<cmd>HopChar1<cr>")
vim.keymap.set('n', 'C', "<cmd>HopChar2<cr>")
vim.keymap.set('n', 'l', "<cmd>HopLine<cr>")
vim.keymap.set('n', 'p', "<cmd>HopPattern<cr>")
vim.keymap.set('n', 'w', "<cmd>HopWord<cr>")
-- smart-splits.nvim {{{
-- resizing splits
vim.keymap.set('n', '<C-w>r', require('smart-splits').start_resize_mode)

-- moving between splits
vim.keymap.set('n', '<C-w>h', require('smart-splits').move_cursor_left)
vim.keymap.set('n', '<C-w>j', require('smart-splits').move_cursor_down)
vim.keymap.set('n', '<C-w>k', require('smart-splits').move_cursor_up)
vim.keymap.set('n', '<C-w>l', require('smart-splits').move_cursor_right)

function _G.set_terminal_keymaps()
    local opts = {buffer = 0}
    vim.keymap.set('t', '<C-w>h', [[<Cmd>wincmd h<CR>]], opts)
    vim.keymap.set('t', '<C-w>j', [[<Cmd>wincmd j<CR>]], opts)
    vim.keymap.set('t', '<C-w>k', [[<Cmd>wincmd k<CR>]], opts)
    vim.keymap.set('t', '<C-w>l', [[<Cmd>wincmd l<CR>]], opts)
end
 
-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')
--- }}}
require("toggleterm").setup{
    -- size can be a number or function which is passed the current terminal
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
        end
    end,
    open_mapping = [[<c-\>]],
    hide_numbers = true, -- hide the number column in toggleterm buffers
    start_in_insert = true,
    insert_mappings = true, -- whether or not the open mapping applies in insert mode
    terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
    persist_size = true,
    persist_mode = true, -- if set to true (default) the previous terminal mode will be remembered
    direction = 'vertical',
    close_on_exit = true, -- close the terminal window when the process exits
    shell = vim.o.shell, -- change the default shell
    float_opts = {
        border = 'single',
        winblend = 3,
    },
    winbar = {
        enabled = false,
    },
}
local Terminal  = require('toggleterm.terminal').Terminal

function _run_last_command()
    vim.cmd("ToggleTerm")
    vim.cmd('call feedkeys("\\<Up>\\<Cr>")')
end

vim.api.nvim_set_keymap("n", "<leader>g", "<cmd>lua _run_last_command()<CR>", {noremap = true, silent = true})
-- indent-blankline {{{
require("indent_blankline").setup()
-- }}}
