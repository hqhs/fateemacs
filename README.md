![Supports Emacs 27.1 - 29.2](https://img.shields.io/badge/Supports-Emacs_27.1--29.2-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white) 

> Emacs was a mistake.
> 
> If I could travel back in time and prevent myself from spending countless hours configuring and constantly fixing this wonderful 30+ years old piece of software and do something fun instead, I would.
> 
> It's hard to call this time "wasted" because Lisp is still an excellent language, and emacs provide an opportunity to learn it properly. I also think configuring & debugging a 30+-year-old piece of software is much closer to "real engineering" than using something comparably new, "shiny," and in line with whatever programming trend is popular. But still, it's hundreds of hours...
> 
> Let god be my witness; if I ever encounter something working only half the time without any conceivable reason, I would burn this config to the ground and move to the neovim instead.

To try:
```bash
git clone git@github.com:hqhs/fateemacs.git ${EMACS_INSTALLATION}
emacs -nw --init-directory ${EMACS_INSTALLATION}
```
To install: 
```bash
git clone git@github.com:hqhs/fateemacs.git ~/.emacs.d
emacs -nw
```

external tools:
```
# Ubuntu/Debian
sudo apt install global ripgrep
```

```
# macOS
brew install global ripgrep
```

### Fate emacs

This is my attempt to provide sane default config there I try to disable as much as possible and still provide enought features for muscle memory to work.

### Known issues

Do not use GUI on macOS.

- [Emacs redisplay sometimes fails to properly redraw screen.](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32932)

There is a potential solution, but it has major performance problems in its current form and I don't have the time to try and intuit a fix from Apple's, shockingly lacking, documentation

I recommed to use Alacritty as a separate window. Funny enough, it's also feels much, much snappier.

### Differences from other popular emacs configurations

#### startup time optimizations is (mostly) not worth it

E.g. doom emacs employs various hacks to provide separate elisp binary to manage emacs installation outside of the emacs to pre-build packages for faster startup time. Although smart, it doesn't feel like "emacs way" of doing things. This config does the simplest thing to provide comparable startup time: `use-package` with `straight.el` integration.

#### No org-mode 

Org-mode is powerful, but only limited to emacs. Markdown is fine. I don't want to be limited to single tool to manage my notes. Pure text files is the most popular data format. There are hordes of software supporting markdown. Another reason is that I want note tracking process to be as simple as possible to not get distructed by it.

#### Lots of "IDE" features are missing

I want a text editor with minimal amount of features, but still usable for active developement. 

#### minimum amount of lisp code

I believe that to fully utilize Emacs one should become guru of lisp developement, testing and debugging. It's not really achievable with using someones' else config. "Know your tools" produces much more powerful approach than "use the most capable tools available without understanding them."

[Here's a good "Introduction to Programming in Emacs Lisp"](https://www.gnu.org/software/emacs/manual/html_node/eintr/)


