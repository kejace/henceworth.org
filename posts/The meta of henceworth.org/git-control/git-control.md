---
title: My git-setup
date: 2013-08-07
author: Kristoffer Josefsson
description: How we are managing version control
---

There are a couple of lessons. 
First, in order to learn all the GIT basics, I found [this](http://try.github.io/) very helpful.
Then later, this [site](https://github.com/Kunena/Kunena-Forum/wiki/Create-a-new-branch-with-git-and-manage-branches) was very helpful in understanding branching. Now I always branch in order not to mess up with my changes.

For the first time, I do


    git branch mynewtest


Then I keep this branch for whatever experimental stuff I want to work on. When that is all done, I do


    git checkout mynewtest

.... (changes)

    git commit -a -m "Hey look at my new experimental features."


If all works, and I want to merge it into my master repository, I do


    git checkout master

    git merge mynewtest


After that, I might want to push branches to github


	git push


Voila!

Now, we want our project to have a couple of dependencies - for example the [Skeleton](www.getskeleton.com) or [Bootstrap](twitter.github.io/bootstrap/‎). We want to have version control information intact so that we can update the libraries, or perhaps even commit our own patches. So first we fork a copy on github using the fork button on the website.

Then, I follow [this](http://git-scm.com/book/en/Git-Tools-Submodules) tutorial on how to manage submodules.

We check out our fork inside our directory using


    cd myproject

    git submodule add https://github.com/kejace/Skeleton.git skeleton

