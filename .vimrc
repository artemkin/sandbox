
let g:syntastic_cpp_compiler_options = ' -std=c++14'

augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.hpp,*.cpp set filetype=cpp.doxygen
  autocmd Filetype cpp.doxygen setlocal ts=4 sts=4 sw=4 et
  autocmd Filetype html setlocal ts=2 sts=2 sw=2 et
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 et
augroup END

" Set right column
set colorcolumn=120

" Set relative line numbers
set relativenumber

set tags=./tags;

