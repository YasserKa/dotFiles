local M = {}

-- Lines that start the closing/signature; refs get inserted above the last match.
-- Edit these to match how you sign off.
local closing_patterns = { "^%-%- $", "^[Kk]ind regards", "^[Bb]est regards", "^[Rr]egards" }

function M.add()
  -- prefill with the clipboard if it looks like a URL
  local clip = vim.fn.getreg "+"
  local default = clip:match "^https?://%S+$" and clip or ""
  local url = vim.fn.input("URL: ", default)
  if url == "" then return end

  local buf = 0
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  -- find the last existing reference line
  local last
  for i, l in ipairs(lines) do
    if l:match "^%[%d+%] " then last = i end
  end

  local n = last and (tonumber(lines[last]:match "^%[(%d+)%]") + 1) or 1
  local marker = ("[%d]"):format(n)
  local ref = ("%s %s"):format(marker, url)

  -- insert the marker at the cursor (after the cursor char in normal mode)
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local in_insert = vim.api.nvim_get_mode().mode:sub(1, 1) == "i"
  if not in_insert then col = col + #vim.fn.strcharpart(lines[row]:sub(col + 1), 0, 1) end
  vim.api.nvim_buf_set_text(buf, row - 1, col, row - 1, col, { marker })
  vim.api.nvim_win_set_cursor(0, { row, col + #marker })

  -- insert the reference line
  if last then
    vim.api.nvim_buf_set_lines(buf, last, last, false, { ref })
    return
  end

  -- first reference: search from the bottom for the closing line
  local at
  for i = #lines, 1, -1 do
    for _, p in ipairs(closing_patterns) do
      if lines[i]:match(p) then
        at = i
        break
      end
    end
    if at then break end
  end

  if at then
    vim.api.nvim_buf_set_lines(buf, at - 1, at - 1, false, { ref, "" })
  else
    vim.api.nvim_buf_set_lines(buf, -1, -1, false, { "", ref })
  end
end

-- Renumber references by order of first appearance in the text,
-- and reorder the reference lines to match.
function M.fix()
  local buf = 0
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)

  -- collect reference definition lines
  local defs, slots, def_order, is_def = {}, {}, {}, {}
  for i, l in ipairs(lines) do
    local n, rest = l:match "^%[(%d+)%] (.*)$"
    if n then
      n = tonumber(n)
      if defs[n] then
        vim.notify("Duplicate reference [" .. n .. "], aborting", vim.log.levels.WARN)
        return
      end
      defs[n] = rest
      is_def[i] = true
      slots[#slots + 1] = i -- keeps blank lines between refs intact
      def_order[#def_order + 1] = n
    end
  end
  if #slots == 0 then return end

  -- assign new numbers by first appearance in the body
  local map, order = {}, {}
  local function assign(n)
    if defs[n] and not map[n] then
      order[#order + 1] = n
      map[n] = #order
    end
  end
  for i, l in ipairs(lines) do
    if not is_def[i] then
      for d in l:gmatch "%[(%d+)%]" do
        assign(tonumber(d))
      end
    end
  end
  -- references never cited in the text go last, in their original order
  for _, n in ipairs(def_order) do
    assign(n)
  end

  -- build the new buffer contents
  local new = vim.deepcopy(lines)
  for i, l in ipairs(lines) do
    if not is_def[i] then
      new[i] = l:gsub("%[(%d+)%]", function(d)
        local m = map[tonumber(d)]
        if m then return "[" .. m .. "]" end -- nil keeps the original
      end)
    end
  end
  for k, old in ipairs(order) do
    new[slots[k]] = ("[%d] %s"):format(k, defs[old])
  end

  -- write back only the lines that changed
  for i = 1, #lines do
    if new[i] ~= lines[i] then vim.api.nvim_buf_set_lines(buf, i - 1, i, false, { new[i] }) end
  end
end

function M.attach(buf)
  if vim.b[buf].addref_attached then return end
  vim.b[buf].addref_attached = true

  require("which-key").add {
    { "<localleader>i", M.add, desc = "Insert reference", buffer = buf },
    { "<localleader>I", M.fix, desc = "Fix reference order", buffer = buf },
    { "<C-,>f", M.add, desc = "Insert reference", mode = "i", buffer = buf },
  }
end

return M
