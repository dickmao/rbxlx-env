|build-status|

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://github.com/dickmao/rbxlx-env/workflows/CI/badge.svg
   :target: https://github.com/dickmao/rbxlx-env/actions
   :alt: Build Status

 Install
=========
Clone this repo then::

   curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

Basic Usage
===========
::

   cd rbxlx-env
   cp [your .rbxlx file] .
   make unfurl

This extracts all scripts into ``./src``.  You may now apply all your normal tools, e.g., git, emacs, etc.  To reconstitute the ``.rbxlx`` file from (the presumably modified) ``./src``::

   make furl

Typical Linux Workflow
======================
Identify the parent directory of your ``.rbxlx`` file in ``$HOME/.wine``.  A typical Wine_ installation might have it in ``$HOME/.wine/drive_c/users/$(whoami)``.  Assuming this::

   cd rbxlx-env
   ln -s $HOME/.wine/drive_c/users/$(whoami) ./drive_c
   ln -s drive_c/[your rbxlx file] .
   make

Continue using Roblox Studio for all non-editing tasks, i.e., creating and manipulating game objects in Explorer.  Suppose you create a new LocalScript_ in ReplicatedStorage_ and name it "Sword", and now want to edit it with a `reasonable editor`_.  Save in Studio and run::

   make unfurl

The new LocalScript will now appear in ``./src`` under ``ReplicatedStorage/Sword``.  I recommend Projectile_ to sanely navigate the source.

.. _Wine: https://en.wikipedia.org/wiki/Wine_(software)
.. _Lua: http://lua.org/about.html
.. _LocalScript: https://developer.roblox.com/en-us/api-reference/class/LocalScript
.. _ReplicatedStorage: https://developer.roblox.com/en-us/api-reference/class/ReplicatedStorage
.. _Projectile: https://github.com/bbatsov/projectile
.. _Rojo: https://rojo.space/docs/why-rojo/
.. _reasonable editor: https://savannah.gnu.org/projects/emacs
