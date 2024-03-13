# Emacs was a mistake

If I could travel back in time and prevent myself from spending countless hours spent on configuring and constantly fixing this wonderful 30+ years old piece of software and do something fun instead, I would. 

This is my attempt to provide sane default config there I try to disable as much as possible and still provide enought features for muscle memory to work.

Let the god be my witness, if I ever encounter something working only half of the time without any conseivable reason, I would burn this config to the ground and move to the neovim instead.

# Known issues

Do not use GUI on macOS.

- [Emacs redisplay sometimes fails to properly redraw screen.](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32932)

There is a potential solution, but it has major performance problems in its current form and I don't have the time to try and intuit a fix from Apple's, shockingly lacking, documentation

I recommed to use Alacritty as a separate window. Funny enough, it's also feels much, much snappier.

# Differences from other popular emacs configurations

- startup time optimizations is (mostly) not worth it
- org-mode is powerful, but only limited to emacs. I use markdown for notes instead.
- minimum amount of lisp code
