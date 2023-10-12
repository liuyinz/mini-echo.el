# mini-echo.el

## todo

- [x] adjust length with current frame (parent frame? exclude others: posframe,..)
- [x] respect options face as themselves, like evil/meow keep their own origin faces
- [x] handle other packages which faces inherit from changed mode-line,like tabbar, topsy
- [x] disable mode and will recover modeline automatically
- [x] hide modeline small text using faces, ref to hide-mode-line mode
- [x] set mode-lint-format nil , and use window divider to separate windows
- [x] remove hide-mode-line, nerd-icon, doom-modeline,
- [ ] use hl-line to identify which window is active, or overlay only in active window
- [ ] use different overlays which binding to selected minibuffer window
- [ ] handle long/short sections smartly, hide project/filename,due to header info exsits
