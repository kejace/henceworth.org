---
title: My linode-setup
date: 2013-08-24
author: Kristoffer Josefsson
description: How we are deploying this site to linode
---

I bought myself a linode accound for $20 a month.

Once there, I set up everything according to their tutorial, using a debian 7 (64bit) image. 

Since we are only serving static content, I use a nginx server instead of the apache default (according to their tutorials). [Here](https://library.linode.com/web-servers/nginx/configuration/basic) is the basic setup for nginx.

I want to use haskell so

    sudo apt-get update

    apt-get upgrade --show-upgraded

    sudo apt-get install haskell-platform

does the trick.

Afterwards, we do a 

    cabal update

    cabal install hakyll

This takes a while on my linode, perhaps 15mins.

Now, I want a git base repo on the server so that I can push to it. The idea is that when we do a push, the site rebuilds automatically on the remote server, and it updates the static webpages and serves them immediately. There is a workflow described [here](http://chrisdone.com/posts/hakyll-and-git-for-you-blog) that sort of works. [This](https://benjeffrey.com/posts/building-benjeffrey.com-with-hakyll) was another helpful reference.

First make sure git is installed

    sudo apt-get install git

create a bare repository

    mkdir henceworth.org-site.git

    cd henceworth.org-site.git

    git init --bare

Then on our local machine we set up a new remote

    git remote add gutenberg




