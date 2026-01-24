set incsearch
set nocompatible
set autoindent
set nowrap        " Disable line wrapping
" -- Spelling
" -- Indentation settings
set tabstop=2       " Number of spaces a tab counts for
set shiftwidth=2    " Number of spaces for auto-indent
set expandtab       " Use spaces instead of tabs
set softtabstop=2   " Number of spaces for tab/backspace
set list            " Show invisible characters
set listchars=tab:→\ ,trail:·,nbsp:·  " Show tabs as arrows, trailing spaces as dots

" Clipboard integration with system clipboard
if has('unnamedplus')
  " Use the + register (system clipboard) for all yank/delete/put operations
  set clipboard=unnamedplus,unnamed
elseif has('clipboard')
  " Fallback to the * register if + is not available
  set clipboard=unnamed
endif

" Make f and t work across line boundaries
" Store last search for ; and , repeat
let s:last_find_char = ''
let s:last_find_forward = 1
let s:last_find_before = 0

nnoremap <silent> f :<C-u>call <SID>FindCharForward(0)<CR>
nnoremap <silent> F :<C-u>call <SID>FindCharBackward(0)<CR>
nnoremap <silent> t :<C-u>call <SID>FindCharForward(1)<CR>
nnoremap <silent> T :<C-u>call <SID>FindCharBackward(1)<CR>
nnoremap <silent> ; :<C-u>call <SID>RepeatFind(0)<CR>
nnoremap <silent> , :<C-u>call <SID>RepeatFind(1)<CR>

function! s:FindCharForward(before)
  let c = nr2char(getchar())
  let s:last_find_char = c
  let s:last_find_forward = 1
  let s:last_find_before = a:before
  
  call s:DoFind(c, 1, a:before)
endfunction

function! s:FindCharBackward(before)
  let c = nr2char(getchar())
  let s:last_find_char = c
  let s:last_find_forward = 0
  let s:last_find_before = a:before
  
  call s:DoFind(c, 0, a:before)
endfunction

function! s:RepeatFind(reverse)
  if s:last_find_char == ''
    return
  endif
  
  let forward = a:reverse ? !s:last_find_forward : s:last_find_forward
  call s:DoFind(s:last_find_char, forward, s:last_find_before)
endfunction

function! s:DoFind(char, forward, before)
  let flags = a:forward ? 'W' : 'bW'
  let end_line = a:forward ? line('$') : 0
  
  " Search for the character
  let found = search('\V' . escape(a:char, '\'), flags, end_line)
  
  if found && a:before
    if a:forward
      " For 't', move one character back if not at line start
      if col('.') > 1
        normal! h
      endif
    else
      " For 'T', move one character forward if not at line end
      if col('.') < col('$') - 1
        normal! l
      endif
    endif
  endif
endfunction
