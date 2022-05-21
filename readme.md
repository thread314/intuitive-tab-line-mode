# Intuitive Tabs in Emacs.

### TLDR:

A way to make Emacs tabs behave intuitively (similar to how tabs behave in a browser).


### Problem:

I know many don't feel this way, but I like using tabs. And more than this, I like the simplicity of having one buffer per tab.

I tried doing things The Emacs Way (TM) of having buffers managed invisibly in the background, but I could never get a good mental model for this. I needed a visual representation of what buffers were open and their order.

What I wanted was:

-   For any buffer manually opened by the user to be represented by its own tab.
-   For their order to persist (until manually changed).

Put simply, I want Emacs tabs to work like a browser would. For years, this has been the holy grail for me.


### What I tried:

First up I tried to get this behaviour with `tab-bar-mode`. This felt like forcing a square peg through a round hole. It took a bit of work, and did what I expected it to do about 90% of the time. It felt very inelegant, and was not reliable. Failure #1. 

Next up I experimented with `tab-line-mode`. While the functionality is much more limited than `tab-bar-mode`, it is clearly a lot closer to my use-case (one buffer per tab).

Setting `tab-line-tabs-function` let me define what buffers I wanted to be displayed as tabs, but none of the standard options behaved how I wanted. They all either showed buffers I did not want to be shown or changed the order of the tabs very unpredictably. Failure #2.

So, next I tried creating my own custom `tab-line-tabs-function`. I created a simple list to manage the tab/buffers I wanted displayed. I then created a set of functions to manipulate that list to get the functionality I was after.

This took a good amount of work on my part (I am not a programmer) and no small amount of guidance from  [helpful](https://www.reddit.com/r/emacs/comments/qdf1hv/can_i_force_each_buffer_to_use_its_own_tab/) [internet](https://stackoverflow.com/questions/69950296/what-is-the-correct-way-to-edit-an-in-built-function) [strangers](https://stackoverflow.com/questions/70042843/how-to-advice-add-a-function-with-no-arguments-to-a-function-that-takes-argument), but I've been using it for 6 months now and it is everything I had hoped it would be. 

[Check it out here.](https://github.com/thread314/my-tab-line-mode)

Tested with Emacs 28.1. 


### Still Todo:

I want to create a new tab each time the user manually creates a new buffer (with `my/add-current-buffer-to-tab`). Problem is there are many different ways a user can do this. I had hoped to find one function that is common to all of them that I could use, but I have not been able to find a good candidate. So this has meant advising every different function I might use to create/open a new buffer.

For a while this was like playing whac-a-mole: I kept realising noticing all the ways I'd manually open a buffer and then have to `add-advice` for that. In particular `find-file` would not play nice as sometimes it does and does not require an argument, so I had to define my own `find-file` functions.

That said it currently covers 99.99% of the ways I open a new buffer and works just beautifully. Others will open buffers in different ways and will likely need to `add-advice` of their own. If anyone can suggest a function to advise that would cover more scenarios, I would love to hear it. 

And finally I simply wrote over `tab-line-tabs-mode-buffers`, because I couldn't figure out how to add another option to `tab-line-tabs-function`. Ugly, but it worked. 


### Conclusion:

As I say, this has been a goal of mine ever since I started using Emacs. It was a lot of confusing work to get it to work, but work it does and it is enormously gratifying. Very open to feedback and criticism, but do keep in mind I'm not a programmer and trying my best. 
