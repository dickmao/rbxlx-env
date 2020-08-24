|build-status|

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://github.com/dickmao/rbxlx-env/workflows/CI/badge.svg
   :target: https://github.com/dickmao/rbxlx-env/actions
   :alt: Build Status

Basic Usage
===========
Clone this repo then::

   cd rbxlx-env
   cp [your .rbxlx file] .
   make unfurl

This extracts all Lua_ scripts into ``./src``.  You may now apply all your normal tools, e.g., git, emacs, etc., to the code.  To recompose the ``.rbxlx`` file::

   make furl

Typical Linux Workflow
======================
Identify the parent directory of your ``.rbxlx`` file in ``$HOME/.wine``.  A typical Wine_ installation might have it in ``$HOME/.wine/drive_c/users/$(whoami)``.  Assuming this,::

   ln -s $HOME/.wine/drive_c/users/$(whoami) ./drive_c
   ln -s drive_c/[your rbxlx file] .
   make

Continue using Roblox Studio for all non-editing tasks, i.e., creating and manipulating Roblox objects in Explorer.  Suppose you create a new LocalScript_ in ReplicatedStorage_ and name it "Sword", and now want to edit it.  Save in Studio and run::

   make unfurl

The new LocalScript will now appear in ``./src`` under ``ReplicatedStorage/Sword``.  I recommend Projectile_ to sanely navigate the source.

.. _Wine: https://en.wikipedia.org/wiki/Wine_(software)
.. _Lua: http://lua.org/about.html
.. _LocalScript: https://developer.roblox.com/en-us/api-reference/class/LocalScript
.. _ReplicatedStorage: https://developer.roblox.com/en-us/api-reference/class/ReplicatedStorage
.. _Projectile: https://github.com/bbatsov/projectile
