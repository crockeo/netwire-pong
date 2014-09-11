# netwire-pong

This is a copy of the classic game of Pong written in Haskell using GLFW, OpenGL, and Netwire 5. This is my first attempt ot write
any sort of somewhat large application using Netwire. Thus what I learned from writing this might merit itself a blog post (on
http://todayincode.tumblr.com (shameless self promotion)).

# Installation

The biggest hurdles to install this are probably:

* Make sure you have bindings to OpenGL installed in your Haskell distribution.
* Make sure you can compile GLFW. The Haskell library seems rather cross-platform, so you should be fine.

Given that this code is and will not be uploaded to Hackage (not important or re-useable enough), you have to install it directly
from source. Luckily, you're in the right place.

Getting the code:
```bash
git clone git@github.com:crockeo/netwire-pong.git
# OR
git clone https://github.com/crockeo/netwire-pong.git
```

Building it:
```bash
cd netwire-pong/                  # Moving into the directory
cabal sandbox init                # Optional, but I always suggest using sandboxes
cabal install --only-dependencies # Installing the required dependencies
cabal run                         # To run
cabal build                       # To build (and run later)
```

If you have any sort of problem installing, feel free to submit a bug report. Even if it isn't a problem with the code I'll be happy to (try to) help.
