local dotlazy_au = vim.api.nvim_create_augroup("dotlazy_au", { clear = true })

vim.o.makeprg = "stack build"
vim.api.nvim_create_autocmd("FileType", {
  group = dotlazy_au,
  pattern = { "haskell" },
  callback = function()
    vim.b.dispatch = "stack exec -- doctest -isrc %"
  end,
})

return {}
