# Garden

A static site generator that challenges how we organize and
discover information on the web.

## The Problem

Modern websites and social media are built around feeds and timelines,
where new content constantly pushes old content out of view.
This creates several problems:

* We lose track of valuable older content
* We repeat discussions that have already happened
* We miss connectoins between related ideas
* We struggle to find things we've seen before
* We get overwhelmed by endless feeds of information

Instead of being bombarded with everything at once,
we need a way to explore content deliberately -
following connections one by one,
going down rabbit holes of interest,
and discovering related ideas naturally.

## The Solution

Garden helps you build a website that:

* Preserves and connects ideas instead of burying them
* Shows you how pages link to each other
* Makes old content as discoverable as new content
* Creates a network of knowledge instead of a timeline
* Lets readers explore content at their own pace

## Features

* Write in simple markdown
* Automatic backlinks show what references each page
* Clean, minimal design that loads instantly
* No JavaScript, no tracking

## Usage

1. Install:

    ```bash
    git clone https://github.com/smomara/garden.git
    cd garden
    cabal build
    ```

2. Write markdown files in the content directory:

    ```markdown
    # content/index.md
    Welcome to my garden! Check out my [thoughts](thoughts.html)
    ```

3. Generate the site:

    ```bash
    cabal run
    ```

    Your static site will be generated in the `site` directory

## Project Structure

```bash
.
├── app/
│   └── Main.hs         # Main program
├── content/            # Your markdown files
│   └── *.md
└── site/              # Generated HTML
    └── *.html
```
