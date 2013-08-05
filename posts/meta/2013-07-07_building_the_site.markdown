---
title: My build-setup
date: 2013-07-07
author: Kristoffer Josefsson
description: How we are building this site
tags: meta
---

First create a Heroku account [here](http://www.heroku.com).
I would say, get a [GitHub](http://www.github.com) account as well. [^1]

[^1]: There's an issue with time-out when including Hakyll in the Cabal dependency so this is kind of pointless at the moment.

Get the toolchains for [Heroku](https://toolbelt.heroku.com) and [GitHub](http://mac.github.com) too.

Clone [this](https://github.com/puffnfresh/haskell-buildpack-demo) repo

	git clone https://github.com/pufuwozu/haskell-buildpack-demo.git

Then create a Heroku app

	heroku create --stack=cedar --buildpack https://github.com/pufuwozu/heroku-buildpack-haskell.git

I went in and changed the name of the app (appname), thereafter you need to also rename your git-info

	heroku git:remote -a appname
	git remote add heroku git@heroku.com:appname.git 

You also want to make sure you add your public key

	heroku keys:add

You can now push it

	git push heroku master

Try out making changes by

	cd src
	vi Main.hs

Then commit your changes

	git commit -a

and push them to see your website update

	git push heroku master