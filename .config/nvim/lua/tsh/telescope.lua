local telescope = require('telescope')
local actions = require('telescope.actions')

telescope.setup {
    defaults = {
        color_devicons = false,
        prompt_prefix = "$ ",
        file_ignore_patterns = { "^.git/" },
        mappings = {
            i = {
                ['<esc>'] = actions.close
            }
        }
    }
}

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
