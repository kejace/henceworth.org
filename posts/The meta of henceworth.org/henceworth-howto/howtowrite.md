---
title: Workflow for Henceworth editing
author: Kristoffer Josefsson
date: 2013-09-04
description: How-to for henceworth.org
---


## Henceworth.org & Annotated Articles

Our idea is simple: henceworth.org will consist of a number of *topics*. Each topic will be curated by an author or multiple authors. Each *topic* will be populated by annotated articles.  Our aim is to provide the text of classic publications with commentary by experts, dreamers and innovators in the field or topical area.

Each article will be a simple text re-write of an original published journal article. Author(s) will annotate articles using footnotes. The footnotes are meant to be roughly the same amount of text as the original article. This will let us display the articles with footnotes side by side, something similar to [literate programming](http://jashkenas.s3.amazonaws.com/misc/docco/docco.html). With the help of some CSS and Javascript, the website will automatically scroll the left and right columns so that the texts are always aligned.

> *Apology:* Many features on henceworth.org are in progress (in particular the design and the reading experience). Until now we have been focusing on getting a workflow for the publishing. 

We'll use an enhanced version of [Markdown](http://en.wikipedia.org/wiki/Markdown) as the primary format for writing articles. Most blog-backends use Markdown.  If you have never seen Markdown before, copy-and-paste the text on this page in the window of [markdownlivepreview.com] and press **preview**.  Or simply look at the example provided. 

Our backend uses [Pandoc](http://johnmacfarlane.net/pandoc/) which has extra support for footnotes[^note_1]. But it also allows for input such as LateX. 

[^note_1]: Does this work in your markdown viewer?
   Standard Markdown doesn't support footnotes, so this might look odd. But in the henceworth backend, it will be displayed on the right column, grouped together with the paragraph in which it was written.

Additionally, every article will have a few extra fields, such as author name, a short decription etc. This is noted in the beginning of the file, as you see at the top of this page.

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

In order to keep things simple, we are using available tools and technology as much as possible.

## Version Control & Collaboration


For collaboration and version control, we are using [Github](http://github.com). Each topic will be a separate repository that will be imported as a submodule into the main site's repository of all articles. This advantage of this method is that anyone can use their own custom *git* workflows.  We can use collaboration features of Github, revision trees, online editing and inbuilt support for Markdown files, etc.

If you are not familiar with git or github, it might all look a bit scary since it is designed for programmers. Don't worry! You can contribute and edit using only your web-browser. 

1. Sign up for an account on the [main page](http://github.com)
2. Create a new repository, named after your topic.
3. Create subfolders and markdown files by within your repository.

When you have a couple of drafts, send us an email and we will take the topic into the website.  If you're having difficulties with any of this, we can help!

The good thing about this separation is that the workflow will automatically incoporate changes by you or your co-authors whenever we rebuild the website. 

Further (optional) steps:

1. Invite collaborators to your git repository
2. Download the github client for your computer so that you can work on your topic offline





