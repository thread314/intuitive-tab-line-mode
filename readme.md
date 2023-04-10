# Intuitive Tabs in Emacs.

### Installation

1. clone into a folder of your choice (~/.emacs.d/git for example)

2. with use-package, configure it like this:

```
(use-package intuitive-tab-line
  :load-path "git/intuitive-tab-line-mode"
  :bind (("C-<tab>" . tab-line-switch-to-next-tab)
         ("M-<left>" . intuitive-tab-line-shift-tab-left)
         ("M-<right>" . intuitive-tab-line-shift-tab-right))
  :custom
  (tab-line-tabs-function 'intuitive-tab-line-buffers-list)
  (tab-line-switch-cycling t)
  :config
  (global-tab-line-mode 1))
```

### The Goal:

Make Emacs tabs behave intuitively (similar to how tabs behave in a browser).


### Problem:

I know many don't feel this way, but I like using tabs. And more than this, I like the simplicity of having one buffer per tab.

I tried doing things The Emacs Way (TM) of having buffers managed invisibly in the background, but I could never get a good mental model for this. I needed a visual representation of what buffers were open and their order.

What I wanted was:

-   For any buffer manually opened by the user to be represented by its own tab.
-   For their order to persist (until manually changed).

Put simply, I want Emacs tabs to work like a browser would.


### The Final Solution:

So using `tab-line-mode` I have created my own custom `tab-line-tabs-function`: a simple list to manage the tab/buffers I wanted displayed. I then created a set of functions to manipulate that list to get the functionality I was after.

This took a good amount of work on my part (I am not a programmer) and no small amount of guidance from  [helpful](https://www.reddit.com/r/emacs/comments/qdf1hv/can_i_force_each_buffer_to_use_its_own_tab/) [internet](https://stackoverflow.com/questions/69950296/what-is-the-correct-way-to-edit-an-in-built-function) [strangers](https://stackoverflow.com/questions/70042843/how-to-advice-add-a-function-with-no-arguments-to-a-function-that-takes-argument), but I've been using it for 6 months now and it is everything I had hoped it would be. 

The various functions should more or less be self-explanatory. Please note I use `tab-line-switch-to-next-tab` and `tab-line-switch-to-prev-tab` to cycle between tabs.

Tested with Emacs 28.1. 


### Notes:

- `kill-this-buffer` will generally work just fine, but will sometimes cause a previously dropped buffer to get a tab again. This can be prevented by instead using `intuitive-tab-line-drop-tab` with a non-nil `kill` argument.
- You can define which buffers you would like get their own tab and which you would like to remain tabless by editing `intuitive-tab-line-add-current-buffer-to-tab`. 
- You can manually add a buffer that would normally not be added with `intuitive-tab-line-manually-add-current-buffer-to-tab`.
- You can remove the tab for a buffer, without killing the buffer with `intuitive-tab-line-drop-tab`. The next time you visit the buffer, it will be given a tab again.
- You can change the order of the tabs with `intuitive-tab-line-shift-tab-left` and `intuitive-tab-line-shift-tab-right`.
- You can set a 'default' tab with `intuitive-tab-line-set-default-tab` and go to the default tab with `intuitive-tab-line-goto-default-tab`.
- You can remove all tabs and leave only your `initial-buffer-choice` open with `intuitive-tab-line-load-initial-buffer-only`.

### Conclusion:

As I say, this has been a goal of mine ever since I started using Emacs. It was a lot of confusing work to get it to work, but work it does and it is enormously gratifying. Very open to feedback and criticism, but do keep in mind I'm not a programmer and trying my best. 
