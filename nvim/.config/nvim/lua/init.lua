require('mini.align').setup()
-- smart-splits.nvim {{{
-- resizing splits
vim.keymap.set('n', '<A-H>', require('smart-splits').resize_left)
vim.keymap.set('n', '<A-J>', require('smart-splits').resize_down)
vim.keymap.set('n', '<A-K>', require('smart-splits').resize_up)
vim.keymap.set('n', '<A-L>', require('smart-splits').resize_right)

-- moving between splits
vim.keymap.set('n', '<A-h>', require('smart-splits').move_cursor_left)
vim.keymap.set('n', '<A-j>', require('smart-splits').move_cursor_down)
vim.keymap.set('n', '<A-k>', require('smart-splits').move_cursor_up)
vim.keymap.set('n', '<A-l>', require('smart-splits').move_cursor_right)

function _G.set_terminal_keymaps()
    local opts = {buffer = 0}
    vim.keymap.set('t', '<A-h>', [[<Cmd>wincmd h<CR>]], opts)
    vim.keymap.set('t', '<A-j>', [[<Cmd>wincmd j<CR>]], opts)
    vim.keymap.set('t', '<A-k>', [[<Cmd>wincmd k<CR>]], opts)
    vim.keymap.set('t', '<A-l>', [[<Cmd>wincmd l<CR>]], opts)
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
