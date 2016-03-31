![GeneralAssemb.ly](../../img/icons/FEWD_Logo.png)

#FEWD - Jekyll and Github




---


##Agenda

*	Publishing on GitHub
*	What is Jekyll?
*	Configuration
*	Includes
*	Templates
*	Content Pages
*	Publish


---


##Background: Git Branching

Branching? What on earth is a branch?

---

##Publishing on GitHub

*	Create a branch "gh-pages"
*	Published on USERNAME.github.io/REPOSITORY

---

##Jekyll

Jekyll is a templating system

	See the docs at:
	https://jekyllrb.com/

---

##Templates

![Template Diagram](../../img/TemplateDiagram.png)

---

##Running Jekyll

Because the site is built in pieces, you need to run Jekyll to preview work.

	Command Line:
	jekyll serve

---

##Installing Jekyll

Windows
	*	Install Ruby 2.1.8
	*	Add ruby to PATH with "set PATH=%PATH%;c:\Ruby21-x64\bin"
	*	Install Jekyll with "gem install jekyll"

Apple OSX
	*	Install Xcode (free from app store)
	*	Install Jekyll with "gem install jekyll"


---

##Create a Jekyll Site

	jekyll new FOLDERNAME --force

---

##Jekyll File & Folder Anatomy

*	_layouts
*	_includes
*	_sass
*	_config.yml

---

##Configuring Jekyll: _config.yml

	# Site settings

	title: Your awesome title

	email: your-email@domain.com

	baseurl: /myRepoName

	description: describe your site

	github_username:  My USERNAME

	# Build settings
	markdown: kramdown


---

##Includes

	{% include header.html %}

![Template Diagram](../../img/IncludeDiagram.png)


---


##Templates

![Template Diagram](../../img/TemplateDiagram.png)

Stored in _layouts

---

##Content Pages

*	Content only, no header or code from the templates
*	HTML, Markdown, or YAML

---

##Content Pages: Head Matter

	&ndash;&ndash;&ndash;
	layout: default
	published: true
	title: Contact Us
	&ndash;&ndash;&ndash;

---

##Publishing

	git commit
	git push (aka sync in GitHub Desktop)





