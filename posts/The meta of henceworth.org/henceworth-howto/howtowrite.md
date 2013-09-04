---
title: Workflow for Henceworth editing
author: Kristoffer Josefsson
date: 2013-09-04
description: How-to for henceworth.org
---


## Structure of articles

The idea is quite simple, henceworth.org will consist of a number of *topics*. Each topic will be curated by an author or multiple authors. We are thinking that each *topic* then will consist of several articles.
Each article will be a re-write of an original published article. The author(s) annotate it using footnotes. The footnotes are meant to be roughly the same amount of text as the original article. This will let us display the articles with the footnotes side by side, something similar to [literate programming](http://jashkenas.s3.amazonaws.com/misc/docco/docco.html). With the help of some CSS and Javascript, the website will automatically scroll the left and right columns so that the texts are always aligned.

> *Apology:* a lot of the features on the website are not done, in particular the design and the reading experience. Until now we have been focusing on getting a workflow for the publishing. 

We'll use an enhanced version of [Markdown](http://en.wikipedia.org/wiki/Markdown) as the primary format for writing the articles in. It is what most blog-backends use, and there's plenty of tools for it. If you never saw it before, copy-and-paste this text and go to [markdownpreviewlive.com] and press **preview**.

Our backend is based using [Pandoc](http://johnmacfarlane.net/pandoc/) which has extra support for footnotes[^note_1]. But it also lets use other input such as LateX if we need to in the future.

[^note_1]: Does this work in your markdown viewer?
   Standard Markdown doesn't support footnotes, so this might look odd. But in the henceworth backend, it will be displayed on the right column, grouped together with the paragraph in which it was written.

More over, each article has a few extra fields, such as author name, a short decription etc. This is noted in the beginning of the file, as above.

As of now, the folder structure is something like this:


    works-of-einstein/
             The Principle of Conservation of Motion of the Center of Gravity and the Inertia of Energy/
           	 		article.markdown
           	 		
           	 		figure01.png

           	 		...

             Theoretical Remarks on Brownian Motion.markdown/

             		anotherarticle.markdown
             		figure.png
             		photo.jpg


----

In order to keep things simple, we are building on available tools and technology as much as possible.

## Version Control and collaboration


For collaboration and version control, we are using [Github](http://github.com). Each topic will be a separate repository that gets imported as a submodule into the main site's repository of all articles. This has the advantage that anyone can use their own curstom *git* workflows. Moreover, we can use all collaboration features of Github, revision trees, online editing and inbuilt support for Markdown files etc.

If you are not familiar with git or github, it might all look a bit scary since it is designed for programmers. Don't worry! You can contribute and edit using only your web-browser. 

1. Sign up for an account on the [main page](http://github.com)
2. Create a new repository, named after your topic.
3. Create subfolders and markdown files by using the website features.

When you have a couple of drafts, send us an email and we will take the topic into the website.

The good thing about this separation is that it will now automtically incoporate changes by you or your co-authors whenever we rebuild the website. 

Further (optional) steps you can look into now could be to

1. invite collaborators to your git repository
2. download the github client for your computer so that you can work on your topic offline





