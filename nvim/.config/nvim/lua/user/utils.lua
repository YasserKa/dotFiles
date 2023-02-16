M = {}

function M.quick_notification(msg)
	vim.notify(msg, "info", { title = "AstroNvim", timeout = 0 })
end

function M.vim_opt_toggle(opt, on, off, name)
	local is_off = vim.opt[opt]:get() == off
	vim.opt[opt] = is_off and on or off
	M.quick_notification(name .. " " .. (is_off and "Enabled" or "Disabled"))
end

function M.has_words_before()
	local line, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

return M
