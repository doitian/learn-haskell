local dotlazy_au = vim.api.nvim_create_augroup("dotlazy_au", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = dotlazy_au,
  pattern = { "haskell" },
  callback = function()
    vim.b.dispatch = "stack exec -- doctest -isrc %"
    vim.bo.makeprg = "stack"
  end,
})

return {}
