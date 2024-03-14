![Supports Emacs 27.1 - 29.2](https://img.shields.io/badge/Supports-Emacs_27.1--29.2-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white) 

> Emacs was a mistake.
> 
> If I could travel back in time and prevent myself from spending countless hours configuring and constantly fixing this wonderful 30+ years old piece of software and do something fun instead, I would.
> 
> It's hard to call this time "wasted" because Lisp is still an excellent language, and emacs provide an opportunity to learn it properly. I also think configuring & debugging a 30+-year-old piece of software is much closer to "real engineering" than using something comparably new, "shiny," and in line with whatever programming trend is popular. But still, it's hundreds of hours...
> 
> Let god be my witness; if I ever encounter something working only half the time without any conceivable reason, I would burn this config to the ground and move to the neovim instead.

### Fate emacs

This is my attempt to provide sane default config there I try to disable as much as possible and still provide enought features for muscle memory to work.

### Known issues

Do not use GUI on macOS.

- [Emacs redisplay sometimes fails to properly redraw screen.](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32932)

There is a potential solution, but it has major performance problems in its current form and I don't have the time to try and intuit a fix from Apple's, shockingly lacking, documentation

I recommed to use Alacritty as a separate window. Funny enough, it's also feels much, much snappier.

### Differences from other popular emacs configurations

#### startup time optimizations is (mostly) not worth it
#### org-mode is powerful, but only limited to emacs. I use markdown for notes instead.
#### minimum amount of lisp code


