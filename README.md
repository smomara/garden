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
* Site map shows most connected pages and good starting points
* Clean, minimal design that loads instantly
* No JavaScript, no tracking
* Custom HTML templates and CSS styling support

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

3. Optionally customize the look:

    ```bash
    # Create custom styles directory
    mkdir styles

    # Add custom template
    # styles/template.html
    <!DOCTYPE html>
    <html>
    <head>
        <title>{{title}}</title>
        <style>{{custom_css}}</style>
    </head>
    <body>
        {{content}}
    </body>
    </html>

    # Add custom CSS
    # styles/custom.css
    body {
        font-family: monospace;
        max-width: 650px;
        margin: 0 auto;
    }
    ```

4. Generate the site:

    ```bash
    cabal run
    ```

    Your static site will be generated in the `site` directory with:
      * An HTML file for each markdown file
      * Backlinks at the bottom of each page
      * A site map (`map.html`) showing:
        * Best starting points based on outgoing links
        * Most connected pages
        * Pages that need more connections

## Project Structure

```bash
.
├── app/
│   ├── Main.hs            # Main program
│   ├── Types.hs           # Core types
│   └── TableOfContents.hs # Site map generation
├── content/               # Your markdown files
│   └── *.md
├── styles/                # Optional custom styling
│   ├── template.html      # Custom HTML template
│   └── custom.css         # Custom CSS styles
└── site/                  # Generated HTML
    └── *.html
```

## Customization

Garden supports custom HTML templates and CSS through the `styles` directory:

* `styles/template.html`: Custom HTML template with placeholders:
  * `{{title}}`: Page title
  * `{{Content}}`: Page content
  * `{{custom_css}}`: Custom CSS styles
* `styles/custom.css`: Custom CSS styles for your site

If these files don't exist, Garden will use defaults
that emphasize clean, minimal styling.
