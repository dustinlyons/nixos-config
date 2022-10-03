" Lucius vim color file
" Maintainer: Jonathan Filip <jfilip1024@gmail.com>
" Version: 7.1.1

hi clear
if exists("syntax_on")
    syntax reset
endif
let colors_name="lucius"

" Summary:
" Color scheme with dark and light versions (GUI and 256 color terminal).
" 
" Description:
" This color scheme was originally created by combining my favorite parts of
" the following color schemes:
" 
" * oceandeep (vimscript #368)
" * peaksea (vimscript #760)
" * wombat (vimscript #1778)
" * moria (vimscript #1464)
" * zenburn (vimscript #415)
" 
" Version 7 has unified the 256 color terminal and GUI versions (the GUI
" version only uses colors available on the 256 color terminal). The overall
" colors were also toned down a little bit (light version is now a light gray
" instead of white and the dark version is slightly lighter) to make it easier
" on the eyes.
" 
" Version 6+ has been revamped a bit from the original color scheme. If you
" prefer the old style, or the 'blue' version, use the 5Final release. Version
" 6+ only has a light and dark version. The new version tries to unify some of
" the colors and also adds more contrast between text and interface.
" 
" The color scheme is dark, by default. You can change this by setting the
" g:lucius_style variable to "light", "dark", or "dark_dim". Once the color
" scheme is loaded, you can use the commands "LuciusLight", "LuciusDark", or
" "LuciusDarkDim" to change schemes quickly.
" 
" Screenshots of version 7:
" 
" * Dark: http://i.imgur.com/tgUsz.png
" * DarkDim: http://i.imgur.com/0bOCv.png
" * Light: http://i.imgur.com/ndd9A.png
" 
" Screenshots of version 6:
" 
" * Dark: http://i.imgur.com/IzYcB.png
" * Light: http://i.imgur.com/kfJcm.png
" 
" Screenshots of the version 5Final:
" 
" * Dark: http://i.imgur.com/z0bDr.png
" * Light: http://i.imgur.com/BXDiv.png
" * Blue: http://i.imgur.com/Ea1Gq.png
" 
" colorsupport.vim (vimscript #2682) is used to help with mapping the GUI
" settings to the 256 terminal colors.
" 
" This color scheme also has custom colors defined for the following plugins:
"
" * vimwiki (vimscript #2226)
" * tagbar (vimscript #3465)
"
" Installation:
" Copy the file to your vim colors directory and then do :colorscheme lucius.

set background=dark
if exists("g:lucius_style")
    if g:lucius_style == "light"
        set background=light
    endif
else
    let g:lucius_style = "dark"
endif

" set colorcolumn=21,37,53,68,86,100

if g:lucius_style == "dark"

    hi CocWarningSign  guifg=#d7d7d7   guibg=#192023   ctermfg=187    ctermbg=NONE      gui=none      cterm=none
    hi Normal       guifg=#d7d7d7   guibg=#192023   ctermfg=188    ctermbg=NONE      gui=none      cterm=none
    hi Comment      guifg=#808080   guibg=NONE      ctermfg=244    ctermbg=NONE      gui=none      cterm=none
    hi Constant     guifg=#d7d7af   guibg=NONE      ctermfg=187    ctermbg=NONE      gui=none      cterm=none
    hi BConstant    guifg=#d7d7af   guibg=NONE      ctermfg=187    ctermbg=NONE      gui=bold      cterm=bold
    hi Identifier   guifg=#afd787   guibg=NONE      ctermfg=150    ctermbg=NONE      gui=none      cterm=none
    hi BIdentifier  guifg=#afd787   guibg=NONE      ctermfg=150    ctermbg=NONE      gui=bold      cterm=bold
    hi Statement    guifg=#87d7ff   guibg=NONE      ctermfg=117    ctermbg=NONE      gui=none      cterm=none
    hi BStatement   guifg=#87d7ff   guibg=NONE      ctermfg=117    ctermbg=NONE      gui=bold      cterm=bold
    hi PreProc      guifg=#87d7af   guibg=NONE      ctermfg=115    ctermbg=NONE      gui=none      cterm=none
    hi BPreProc     guifg=#87d7af   guibg=NONE      ctermfg=115    ctermbg=NONE      gui=bold      cterm=bold
    hi Type         guifg=#87d7d7   guibg=NONE      ctermfg=116    ctermbg=NONE      gui=none      cterm=none
    hi BType        guifg=#87d7d7   guibg=NONE      ctermfg=116    ctermbg=NONE      gui=bold      cterm=bold
    hi Special      guifg=#d7afd7   guibg=NONE      ctermfg=182    ctermbg=NONE      gui=none      cterm=none
    hi BSpecial     guifg=#d7afd7   guibg=NONE      ctermfg=182    ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Markup ##
    hi Underlined   guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi Error        guifg=#ff8787   guibg=#870000   ctermfg=210    ctermbg=88        gui=none      cterm=none
    hi Todo         guifg=#d7d75f   guibg=#5f5f00   ctermfg=185    ctermbg=58        gui=none      cterm=none
    hi MatchParen   guifg=bg        guibg=#afd75f   ctermfg=NONE   ctermbg=149       gui=none      cterm=bold
    hi NonText      guifg=#5f5f87   guibg=NONE      ctermfg=60     ctermbg=NONE      gui=none      cterm=none
    hi SpecialKey   guifg=#5f875f   guibg=NONE      ctermfg=65     ctermbg=NONE      gui=none      cterm=none
    hi Title        guifg=#5fafd7   guibg=NONE      ctermfg=74     ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Selection ##
    hi Cursor       guifg=bg        guibg=#87afd7   ctermfg=NONE     ctermbg=110       gui=none      cterm=none
    hi CursorIM     guifg=bg        guibg=#87afd7   ctermfg=NONE     ctermbg=110       gui=none      cterm=none
    hi CursorColumn guifg=NONE      guibg=#444444   ctermfg=NONE   ctermbg=238       gui=none      cterm=none
    hi CursorLine   guifg=NONE      guibg=#242d33
    hi Visual       guifg=NONE      guibg=#005f87   ctermfg=NONE   ctermbg=24        gui=none      cterm=none
    hi VisualNOS    guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi IncSearch    guifg=bg        guibg=#57d7d7   ctermfg=NONE   ctermbg=80        gui=none      cterm=none
    hi Search       guifg=bg        guibg=#d78700   ctermfg=NONE   ctermbg=172       gui=none      cterm=none

    " == UI ==
    hi Pmenu        guifg=bg        guibg=#b2b2b2   ctermfg=NONE   ctermbg=233       gui=none      cterm=none
    hi PmenuSel     guifg=fg        guibg=#005f87   ctermfg=fg     ctermbg=239        gui=none      cterm=none
    hi PmenuSbar    guifg=#b2b2b2   guibg=#d0d0d0   ctermfg=249    ctermbg=252       gui=none      cterm=none
    hi PmenuThumb   guifg=fg        guibg=#808080   ctermfg=fg     ctermbg=244       gui=none      cterm=none
    hi StatusLine   guifg=bg        guibg=#b2b2b2   ctermfg=NONE   ctermbg=NONE       gui=bold      cterm=bold
    hi StatusLineNC guifg=#444444   guibg=#b2b2b2   ctermfg=238    ctermbg=239       gui=none      cterm=none
    hi TabLine      guifg=bg        guibg=#b2b2b2   ctermfg=NONE   ctermbg=NONE      gui=none      cterm=none
    hi TabLineFill  guifg=#444444   guibg=#b2b2b2   ctermfg=NONE   ctermbg=NONE       gui=none      cterm=none
    hi TabLineSel   guifg=fg        guibg=#005f87   ctermfg=233    ctermbg=150        gui=bold      cterm=bold
    hi VertSplit    guifg=#626262   guibg=#b2b2b2   ctermfg=241    ctermbg=249       gui=none      cterm=none
    hi Folded       guifg=#bcbcbc   guibg=#4e4e4e   ctermfg=250    ctermbg=239       gui=bold      cterm=none
    hi FoldColumn   guifg=#bcbcbc   guibg=#4e4e4e   ctermfg=250    ctermbg=239       gui=bold      cterm=none

    " ## Spelling ##
    hi SpellBad     guisp=#d70000                   ctermfg=160    ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellCap     guisp=#00afd7                   ctermfg=38     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellRare    guisp=#5faf00                   ctermfg=70     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellLocal   guisp=#d7af00                   ctermfg=178    ctermbg=NONE      gui=undercurl cterm=underline

    " ## Diff ##
    hi DiffAdd      guifg=fg        guibg=#5f875f   ctermfg=fg     ctermbg=65        gui=none      cterm=none
    hi DiffChange   guifg=fg        guibg=#87875f   ctermfg=fg     ctermbg=101       gui=none      cterm=none
    hi DiffDelete   guifg=fg        guibg=#875f5f   ctermfg=fg     ctermbg=95        gui=none      cterm=none
    hi DiffText     guifg=#ffff87   guibg=#87875f   ctermfg=228    ctermbg=101       gui=none      cterm=none

    " ## Misc ##
    hi Directory    guifg=#afd7af   guibg=NONE      ctermfg=151    ctermbg=NONE      gui=none      cterm=none
    hi ErrorMsg     guifg=#ff5f5f   guibg=NONE      ctermfg=203    ctermbg=NONE      gui=none      cterm=none
    hi SignColumn   guifg=#b2b2b2   guibg=#242d33   ctermfg=NONE   ctermbg=NONE      gui=none      cterm=none
    hi LineNr       guifg=#626262   guibg=#444444   ctermfg=NONE   ctermbg=NONE       gui=none      cterm=none
    hi CursorLineNr guifg=#626262   guibg=#444444   ctermfg=NONE   ctermbg=NONE       gui=none      cterm=none
    hi MoreMsg      guifg=#5fd7d7   guibg=NONE      ctermfg=80     ctermbg=NONE      gui=none      cterm=none
    hi ModeMsg      guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi Question     guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi WarningMsg   guifg=#d7875f   guibg=NONE      ctermfg=173    ctermbg=NONE      gui=none      cterm=none
    hi WildMenu     guifg=fg        guibg=#005f87   ctermfg=fg     ctermbg=24        gui=none      cterm=none
    hi ColorColumn  guifg=NONE      guibg=#87875f   ctermfg=NONE   ctermbg=101       gui=none      cterm=none
    hi Ignore       guifg=bg                        ctermfg=NONE


elseif g:lucius_style == "dark_dim"


    hi Normal       guifg=#bcbcbc   guibg=#192023   ctermfg=250    ctermbg=236       gui=none      cterm=none
    hi Comment      guifg=#6c6c6c   guibg=NONE      ctermfg=242    ctermbg=NONE      gui=none      cterm=none
    hi Constant     guifg=#afaf87   guibg=NONE      ctermfg=144    ctermbg=NONE      gui=none      cterm=none
    hi BConstant    guifg=#afaf87   guibg=NONE      ctermfg=144    ctermbg=NONE      gui=bold      cterm=bold
    hi Identifier   guifg=#87af5f   guibg=NONE      ctermfg=107    ctermbg=NONE      gui=none      cterm=none
    hi BIdentifier  guifg=#87af5f   guibg=NONE      ctermfg=107    ctermbg=NONE      gui=bold      cterm=bold
    hi Statement    guifg=#57afd7   guibg=NONE      ctermfg=74     ctermbg=NONE      gui=none      cterm=none
    hi BStatement   guifg=#57afd7   guibg=NONE      ctermfg=74     ctermbg=NONE      gui=bold      cterm=bold
    hi PreProc      guifg=#5faf87   guibg=NONE      ctermfg=72     ctermbg=NONE      gui=none      cterm=none
    hi BPreProc     guifg=#5faf87   guibg=NONE      ctermfg=72     ctermbg=NONE      gui=bold      cterm=bold
    hi Type         guifg=#5fafaf   guibg=NONE      ctermfg=73     ctermbg=NONE      gui=none      cterm=none
    hi BType        guifg=#5fafaf   guibg=NONE      ctermfg=73     ctermbg=NONE      gui=bold      cterm=bold
    hi Special      guifg=#af87af   guibg=NONE      ctermfg=139    ctermbg=NONE      gui=none      cterm=none
    hi BSpecial     guifg=#af87af   guibg=NONE      ctermfg=139    ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Markup ##
    hi Underlined   guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi Error        guifg=#d75f5f   guibg=#870000   ctermfg=167    ctermbg=88        gui=none      cterm=none
    hi Todo         guifg=#afaf00   guibg=#5f5f00   ctermfg=142    ctermbg=58        gui=none      cterm=none
    hi MatchParen   guifg=bg        guibg=#87af5f   ctermfg=bg     ctermbg=107       gui=none      cterm=bold
    hi NonText      guifg=#5f5f87   guibg=NONE      ctermfg=60     ctermbg=NONE      gui=none      cterm=none
    hi SpecialKey   guifg=#5f875f   guibg=NONE      ctermfg=65     ctermbg=NONE      gui=none      cterm=none
    hi Title        guifg=#00afd7   guibg=NONE      ctermfg=38     ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Selection ##
    hi Cursor       guifg=bg        guibg=#5f87af   ctermfg=bg     ctermbg=67        gui=none      cterm=none
    hi CursorIM     guifg=bg        guibg=#5f87af   ctermfg=bg     ctermbg=67        gui=none      cterm=none
    hi CursorColumn guifg=NONE      guibg=#444444   ctermfg=NONE   ctermbg=238       gui=none      cterm=none
    hi CursorLine   guifg=NONE      guibg=#444444   ctermfg=NONE   ctermbg=238       gui=none      cterm=none
    hi Visual       guifg=NONE      guibg=#005f87   ctermfg=NONE   ctermbg=24        gui=none      cterm=none
    hi VisualNOS    guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi IncSearch    guifg=bg        guibg=#00afaf   ctermfg=bg     ctermbg=37        gui=none      cterm=none
    hi Search       guifg=bg        guibg=#d78700   ctermfg=bg     ctermbg=172       gui=none      cterm=none

    " == UI ==
    hi Pmenu        guifg=bg        guibg=#8a8a8a   ctermfg=bg     ctermbg=245       gui=none      cterm=none
    hi PmenuSel     guifg=fg        guibg=#005f87   ctermfg=fg     ctermbg=24        gui=none      cterm=none
    hi PmenuSbar    guifg=#8a8a8a   guibg=#bcbcbc   ctermfg=245    ctermbg=250       gui=none      cterm=none
    hi PmenuThumb   guifg=fg        guibg=#585858   ctermfg=fg     ctermbg=240       gui=none      cterm=none
    hi StatusLine   guifg=bg        guibg=#8a8a8a   ctermfg=bg     ctermbg=245       gui=bold      cterm=bold
    hi StatusLineNC guifg=#444444   guibg=#8a8a8a   ctermfg=238    ctermbg=245       gui=none      cterm=none
    hi TabLine      guifg=bg        guibg=#8a8a8a   ctermfg=bg     ctermbg=245       gui=none      cterm=none
    hi TabLineFill  guifg=#444444   guibg=#8a8a8a   ctermfg=238    ctermbg=245       gui=none      cterm=none
    hi TabLineSel   guifg=fg        guibg=#005f87   ctermfg=fg     ctermbg=24        gui=bold      cterm=bold
    hi VertSplit    guifg=#626262   guibg=#8a8a8a   ctermfg=241    ctermbg=245       gui=none      cterm=none
    hi Folded       guifg=#a8a8a8   guibg=#4e4e4e   ctermfg=248    ctermbg=239       gui=bold      cterm=none
    hi FoldColumn   guifg=#a8a8a8   guibg=#4e4e4e   ctermfg=248    ctermbg=239       gui=bold      cterm=none

    " ## Spelling ##
    hi SpellBad     guisp=#d70000                   ctermfg=160    ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellCap     guisp=#00afd7                   ctermfg=38     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellRare    guisp=#5faf00                   ctermfg=70     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellLocal   guisp=#d7af00                   ctermfg=178    ctermbg=NONE      gui=undercurl cterm=underline

    " ## Diff ##
    hi DiffAdd      guifg=fg        guibg=#5f875f   ctermfg=fg     ctermbg=65        gui=none      cterm=none
    hi DiffChange   guifg=fg        guibg=#87875f   ctermfg=fg     ctermbg=101       gui=none      cterm=none
    hi DiffDelete   guifg=fg        guibg=#875f5f   ctermfg=fg     ctermbg=95        gui=none      cterm=none
    hi DiffText     guifg=#d7d75f   guibg=#87875f   ctermfg=185    ctermbg=101       gui=none      cterm=none

    " ## Misc ##
    hi Directory    guifg=#87af87   guibg=NONE      ctermfg=108    ctermbg=NONE      gui=none      cterm=none
    hi ErrorMsg     guifg=#d75f5f   guibg=NONE      ctermfg=167    ctermbg=NONE      gui=none      cterm=none
    hi SignColumn   guifg=#8a8a8a   guibg=#4e4e4e   ctermfg=245    ctermbg=239       gui=none      cterm=none
    hi LineNr       guifg=#626262   guibg=#444444   ctermfg=241    ctermbg=238       gui=none      cterm=none
    hi CursorLineNr guifg=#626262   guibg=#444444   ctermfg=241    ctermbg=238       gui=none      cterm=none
    hi MoreMsg      guifg=#00afaf   guibg=NONE      ctermfg=37     ctermbg=NONE      gui=none      cterm=none
    hi ModeMsg      guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi Question     guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi WarningMsg   guifg=#af875f   guibg=NONE      ctermfg=173    ctermbg=NONE      gui=none      cterm=none
    hi WildMenu     guifg=fg        guibg=#005f87   ctermfg=fg     ctermbg=24        gui=none      cterm=none
    hi ColorColumn  guifg=NONE      guibg=#87875f   ctermfg=NONE   ctermbg=101       gui=none      cterm=none
    hi Ignore       guifg=bg                        ctermfg=bg


elseif g:lucius_style == "light"


    hi Normal       guifg=#444444   guibg=#eeeeee   ctermfg=238    ctermbg=255       gui=none      cterm=none
    hi Comment      guifg=#808080   guibg=NONE      ctermfg=244    ctermbg=NONE      gui=none      cterm=none
    hi Constant     guifg=#af5f00   guibg=NONE      ctermfg=130    ctermbg=NONE      gui=none      cterm=none
    hi BConstant    guifg=#af5f00   guibg=NONE      ctermfg=130    ctermbg=NONE      gui=bold      cterm=bold
    hi Identifier   guifg=#008700   guibg=NONE      ctermfg=28     ctermbg=NONE      gui=none      cterm=none
    hi BIdentifier  guifg=#008700   guibg=NONE      ctermfg=28     ctermbg=NONE      gui=bold      cterm=bold
    hi Statement    guifg=#005faf   guibg=NONE      ctermfg=25     ctermbg=NONE      gui=none      cterm=none
    hi BStatement   guifg=#005faf   guibg=NONE      ctermfg=25     ctermbg=NONE      gui=bold      cterm=bold
    hi PreProc      guifg=#008787   guibg=NONE      ctermfg=30     ctermbg=NONE      gui=none      cterm=none
    hi BPreProc     guifg=#008787   guibg=NONE      ctermfg=30     ctermbg=NONE      gui=bold      cterm=bold
    hi Type         guifg=#005f87   guibg=NONE      ctermfg=24     ctermbg=NONE      gui=none      cterm=none
    hi BType        guifg=#005f87   guibg=NONE      ctermfg=24     ctermbg=NONE      gui=bold      cterm=bold
    hi Special      guifg=#870087   guibg=NONE      ctermfg=90     ctermbg=NONE      gui=none      cterm=none
    hi BSpecial     guifg=#870087   guibg=NONE      ctermfg=90     ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Markup ##
    hi Underlined   guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi Error        guifg=#af0000   guibg=#d7afaf   ctermfg=124    ctermbg=181       gui=none      cterm=none
    hi Todo         guifg=#875f00   guibg=#ffffaf   ctermfg=94     ctermbg=229       gui=none      cterm=none
    hi MatchParen   guifg=NONE      guibg=#5fd7d7   ctermfg=NONE   ctermbg=80        gui=none      cterm=none
    hi NonText      guifg=#afafd7   guibg=NONE      ctermfg=146    ctermbg=NONE      gui=none      cterm=none
    hi SpecialKey   guifg=#afd7af   guibg=NONE      ctermfg=151    ctermbg=NONE      gui=none      cterm=none
    hi Title        guifg=#005faf   guibg=NONE      ctermfg=25     ctermbg=NONE      gui=bold      cterm=bold

    " ## Text Selection ##
    hi Cursor       guifg=bg        guibg=#5f87af   ctermfg=bg     ctermbg=67        gui=none      cterm=none
    hi CursorIM     guifg=bg        guibg=#5f87af   ctermfg=bg     ctermbg=67        gui=none      cterm=none
    hi CursorColumn guifg=NONE      guibg=#dadada   ctermfg=NONE   ctermbg=253       gui=none      cterm=none
    hi CursorLine   guifg=NONE      guibg=#dadada   ctermfg=NONE   ctermbg=253       gui=none      cterm=none
    hi Visual       guifg=NONE      guibg=#afd7ff   ctermfg=NONE   ctermbg=153       gui=none      cterm=none
    hi VisualNOS    guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=underline cterm=underline
    hi IncSearch    guifg=fg        guibg=#57d7d7   ctermfg=fg     ctermbg=80        gui=none      cterm=none
    hi Search       guifg=fg        guibg=#ffaf00   ctermfg=fg     ctermbg=214       gui=none      cterm=none

    " ## UI ##
    hi Pmenu        guifg=bg        guibg=#808080   ctermfg=bg     ctermbg=fg       gui=none      cterm=none
    hi PmenuSel     guifg=fg        guibg=#afd7ff   ctermfg=fg     ctermbg=153       gui=none      cterm=none
    hi PmenuSbar    guifg=#808080   guibg=#444444   ctermfg=244    ctermbg=238       gui=none      cterm=none
    hi PmenuThumb   guifg=fg        guibg=#9e9e9e   ctermfg=fg     ctermbg=247       gui=none      cterm=none
    hi StatusLine   guifg=bg        guibg=#808080   ctermfg=bg     ctermbg=244       gui=bold      cterm=bold
    hi StatusLineNC guifg=#e4e4e4   guibg=#808080   ctermfg=254    ctermbg=244       gui=none      cterm=none
    hi TabLine      guifg=bg        guibg=#808080   ctermfg=bg     ctermbg=244       gui=none      cterm=none
    hi TabLineFill  guifg=#b2b2b2   guibg=#808080   ctermfg=249    ctermbg=244       gui=none      cterm=none
    hi TabLineSel   guifg=fg        guibg=#afd7ff   ctermfg=fg     ctermbg=153       gui=none      cterm=none
    hi VertSplit    guifg=#e4e4e4   guibg=#808080   ctermfg=254    ctermbg=244       gui=none      cterm=none
    hi Folded       guifg=#626262   guibg=#bcbcbc   ctermfg=241    ctermbg=250       gui=bold      cterm=none
    hi FoldColumn   guifg=#626262   guibg=#bcbcbc   ctermfg=241    ctermbg=250       gui=bold      cterm=none

    " ## Spelling ##
    hi SpellBad     guisp=#d70000                   ctermfg=160    ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellCap     guisp=#00afd7                   ctermfg=38     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellRare    guisp=#5faf00                   ctermfg=70     ctermbg=NONE      gui=undercurl cterm=underline
    hi SpellLocal   guisp=#d7af00                   ctermfg=178    ctermbg=NONE      gui=undercurl cterm=underline

    " ## Diff ##
    hi DiffAdd      guifg=fg        guibg=#afd7af   ctermfg=fg     ctermbg=151       gui=none      cterm=none
    hi DiffChange   guifg=fg        guibg=#d7d7af   ctermfg=fg     ctermbg=187       gui=none      cterm=none
    hi DiffDelete   guifg=fg        guibg=#d7afaf   ctermfg=fg     ctermbg=181       gui=none      cterm=none
    hi DiffText     guifg=#d75f00   guibg=#d7d7af   ctermfg=166    ctermbg=187       gui=bold      cterm=bold

    " ## Misc ##
    hi Directory    guifg=#00875f   guibg=NONE      ctermfg=29     ctermbg=NONE      gui=none      cterm=none
    hi ErrorMsg     guifg=#af0000   guibg=NONE      ctermfg=124    ctermbg=NONE      gui=none      cterm=none
    hi SignColumn   guifg=#626262   guibg=#d0d0d0   ctermfg=241    ctermbg=252       gui=none      cterm=none
    hi LineNr       guifg=#9e9e9e   guibg=#dadada   ctermfg=247    ctermbg=253       gui=none      cterm=none
    hi CursorLineNr guifg=#9e9e9e   guibg=#dadada   ctermfg=247    ctermbg=253       gui=none      cterm=none
    hi MoreMsg      guifg=#005fd7   guibg=NONE      ctermfg=26     ctermbg=NONE      gui=none      cterm=none
    hi ModeMsg      guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi Question     guifg=fg        guibg=NONE      ctermfg=fg     ctermbg=NONE      gui=none      cterm=none
    hi WarningMsg   guifg=#af5700   guibg=NONE      ctermfg=130    ctermbg=NONE      gui=none      cterm=none
    hi WildMenu     guifg=fg        guibg=#afd7ff   ctermfg=fg     ctermbg=153       gui=none      cterm=none
    hi ColorColumn  guifg=NONE      guibg=#d7d7af   ctermfg=NONE   ctermbg=187       gui=none      cterm=none
    hi Ignore       guifg=bg                        ctermfg=bg


endif


" ## Vimwiki Colors ##
hi link VimwikiHeader1 BIdentifier
hi link VimwikiHeader2 BPreProc
hi link VimwikiHeader3 BStatement
hi link VimwikiHeader4 BSpecial
hi link VimwikiHeader5 BConstant
hi link VimwikiHeader6 BType

" ## Tagbar Colors ##
hi link TagbarAccessPublic Constant
hi link TagbarAccessProtected Type
hi link TagbarAccessPrivate PreProc

" ## Commands ##
command! LuciusLight let g:lucius_style = "light" | colorscheme lucius
command! LuciusDark let g:lucius_style = "dark" | colorscheme lucius
command! LuciusDarkDim let g:lucius_style = "dark_dim" | colorscheme lucius


