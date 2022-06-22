# Intuitive Tabs in Emacs.

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

- You can define which buffers you would like get their own tab and which you would like to remain tabless by editing `my/add-current-buffer-to-tab`. 
- You can manually add a buffer that would normally not be added with `my/manually-add-current-buffer-to-tab`.
- You can remove the tab for a buffer, without killing the buffer with `my/drop-tab`. The next time you visit the buffer, it will be given a tab again.
- You can change the order of the tabs with `my/shift-tab-left` and `my/shift-tab-right`.
- You can set a 'default' tab with `my/set-default-tab` and go to the default tab with `my/goto-default-tab`.
- You can remove all tabs and leave only your `initial-buffer-choice` open with `my/load-initial-buffer-only`.

### Conclusion:

As I say, this has been a goal of mine ever since I started using Emacs. It was a lot of confusing work to get it to work, but work it does and it is enormously gratifying. Very open to feedback and criticism, but do keep in mind I'm not a programmer and trying my best. 
