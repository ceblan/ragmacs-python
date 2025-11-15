;;; ragmacs-python.el --- Tools to retrieve Python documentation from the web  -*- lexical-binding: t; -*-

;; Author: Positron <contact@positron.solutions>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0") (orderless "1.5") (gptel "0.9.8.5"))
;; Maintainer: Positron <contact@positron.solutions>
;; URL: https://github.com/positron-solutions/ragmacs
;; Keywords: convenience
;; SPDX-License-Identifier: MIT

;;; License:

;; Copyright (C) 2025 Positron.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This collection of tools for GPTel is comprised by short functions that the
;; tools will call when invoked by the LLM and the natural language "programs"
;; in the tool description.
;;
;; We can look at the description like how air-traffic-controllers talk to
;; pilots.  While both parties are using natural language, there is a protocol
;; and the commnication is more precise.  This is a good metaphor for the
;; tool-LLM interface.  While the LLM is a natural system, it can interact with
;; programs because there is a protocol, the tool interface.
;;
;; Just as the functions in a library are designed to comprise something bigger,
;; the tools in this collection act together to comprise a larger work.  Many of
;; the tools recommend calling further tools.  This graph of recommended
;; behaviors is a pseudo program that guides the LLM towards behaviors that make
;; it better at traversing code, introspecting Python, and consuming the web
;; documentation.
;;
;; Two particularly compelling use cases were identified so far:
;;
;; - Code crawling, treating the web documentation like a graph of links, giving
;;   the LLM hypermedia-like access to information to help it answer queries
;;
;; - Translation and summary of what was discovered.  Often times only a small
;;   section of documentation is related to the query, and the LLM can
;;   often extract the relevant information faster than a human can read.
;;
;; These tools are intended to inspire creation of further tools, especially
;; those focused on enhancing Python programming so as to enhance the strength of
;; the development ecosystem.
;;
;; ⚠️ It has become apparent since these tools were written that context rot is a
;;   very real phenomenon.  More terse instructions and imperative language seem
;;   to do well.

;;; Code:

(defgroup ragmacs-python nil "Python RAG tools" :prefix 'ragmacs-python :group 'group)

;; These functions are used by the tools.  Most of them are just providing an
;; interface from the LLM into web-based documentation retrieval.

(defun ragmacs-python--web-search (query)
  "Search for QUERY on the web and return results.
Uses curl to fetch search results from DuckDuckGo."
  (let* ((encoded-query (url-hexify-string query))
         (url (format "https://html.duckduckgo.com/html/?q=%s" encoded-query))
         (temp-file (make-temp-file "ddg-search" nil ".html")))
    (condition-case err
        (progn
          (call-process "curl" nil `(:file ,temp-file) nil "-s" url)
          (with-temp-buffer
            (insert-file-contents temp-file)
            (buffer-string)))
      (error
       (delete-file temp-file)
       (error "Failed to search: %s" (error-message-string err)))
      (:success
       (delete-file temp-file)))))

(defun ragmacs-python--fetch-url (url)
  "Fetch the content of URL and return it as a string."
  (let ((temp-file (make-temp-file "web-content" nil ".html")))
    (condition-case err
        (progn
          (call-process "curl" nil `(:file ,temp-file) nil "-s" url)
          (with-temp-buffer
            (insert-file-contents temp-file)
            (buffer-string)))
      (error
       (delete-file temp-file)
       (error "Failed to fetch URL: %s" (error-message-string err)))
      (:success
       (delete-file temp-file)))))

(defun ragmacs-python--extract-text-from-html (html)
  "Extract text content from HTML."
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (when dom
        (replace-regexp-in-string 
         "[ \t\n\r]+" " "
         (replace-regexp-in-string 
          "^[ \t\n\r]+\\|[ \t\n\r]+$" ""
          (dom-texts dom)))))))

(defun ragmacs-python--search-python-docs (topic)
  "Search Python documentation for TOPIC."
  (let* ((query (format "site:docs.python.org %s" topic))
         (search-results (ragmacs-python--web-search query)))
    (ragmacs-python--extract-text-from-html search-results)))

(defun ragmacs-python--search-pypi-package (package-name)
  "Get information about PACKAGE-NAME from PyPI."
  (let ((url (format "https://pypi.org/pypi/%s/json" package-name)))
    (condition-case err
        (let ((json-data (with-temp-buffer
                           (call-process "curl" nil t nil "-s" url)
                           (json-read))))
          (format "Package: %s\nVersion: %s\nSummary: %s\nAuthor: %s\nHomepage: %s\nLicense: %s"
                  (alist-get 'name json-data)
                  (alist-get 'version json-data)
                  (alist-get 'summary json-data)
                  (or (alist-get 'author json-data) "Not specified")
                  (or (alist-get 'home_page json-data) "Not specified")
                  (or (alist-get 'license json-data) "Not specified")))
      (error
       (format "Failed to retrieve package info for %s: %s" package-name (error-message-string err))))))

(defun ragmacs-python--search-stackoverflow (query)
  "Search StackOverflow for QUERY."
  (let* ((full-query (format "site:stackoverflow.com %s" query))
         (search-results (ragmacs-python--web-search full-query)))
    (ragmacs-python--extract-text-from-html search-results)))

(defun ragmacs-python--get-python-version ()
  "Get the current Python version."
  (with-temp-buffer
    (call-process "python" nil t nil "--version")
    (buffer-string)))

(defun ragmacs-python--list-installed-packages ()
  "List installed Python packages."
  (with-temp-buffer
    (call-process "pip" nil t nil "list")
    (buffer-string)))

(defun ragmacs-python--get-package-info (package)
  "Get detailed information about PACKAGE."
  (with-temp-buffer
    (call-process "pip" nil t nil "show" package)
    (buffer-string)))

(defun ragmacs-python--eval-python (code)
  "Evaluate Python CODE and return the result."
  (let ((temp-file (make-temp-file "python-code" nil ".py")))
    (condition-case err
        (progn
          (with-temp-file temp-file
            (insert code))
          (with-temp-buffer
            (call-process "python" nil t nil temp-file)
            (buffer-string)))
      (error
       (format "Error executing Python code: %s" (error-message-string err)))
      (finally
       (delete-file temp-file)))))

;; The tool definitions.  Each tool calls one of the functions above.

(defvar ragmacs-python-web-search
  (gptel-make-tool
   :function #'ragmacs-python--web-search
   :name "web_search"
   :confirm t
   :include t
   :category "py-trospection"
   :args '(( :name "query"
             :type string
             :description "A search query to look up on the web."))
   :description "Search for QUERY on the web and return results.
This tool searches the web using DuckDuckGo and returns the raw HTML results.
Use this to find general information, tutorials, documentation, or examples
related to Python programming concepts.")
  "Search the web for information.")

(defvar ragmacs-python-fetch-url
  (gptel-make-tool
   :function #'ragmacs-python--fetch-url
   :name "fetch_url"
   :confirm t
   :include t
   :category "py-trospection"
   :args '(( :name "url"
             :type string
             :description "A URL to fetch content from."))
   :description "Fetch the content of URL and return it as a string.
Use this tool to retrieve the content of a specific webpage. This is useful
when you have a direct link to documentation or a resource you want to examine.")
  "Fetch content from a URL.")

(defvar ragmacs-python-search-python-docs
  (gptel-make-tool
   :function #'ragmacs-python--search-python-docs
   :name "search_python_docs"
   :confirm t
   :include t
   :category "py-trospection"
   :args '(( :name "topic"
             :type string
             :description "A Python topic to search for in official documentation."))
   :description "Search Python official documentation for TOPIC.
This tool specifically searches docs.python.org for information about Python
built-in functions, modules, classes, and language features. Use this when you
need authoritative information about Python's standard library or language.")
  "Search Python official documentation.")

(defvar ragmacs-python-search-pypi-package
  (gptel-make-tool
   :function #'ragmacs-python--search-pypi-package
   :name "search_pypi_package"
   :confirm t
   :include t
   :category "py-trospection"
   :args '(( :name "package_name"
             :type string
             :description "Name of a Python package on PyPI."))
   :description "Get information about PACKAGE-NAME from PyPI.
This tool retrieves package metadata from PyPI including version, summary,
author, homepage, and license information. Use this to learn about third-party
Python packages available on PyPI.")
  "Search for package information on PyPI.")

(defvar ragmacs-python-search-stackoverflow
  (gptel-make-tool
   :function #'ragmacs-python--search-stackoverflow
   :name "search_stackoverflow"
   :confirm t
   :include t
   :category "py-trospection"
   :args '(( :name "query"
             :type string
             :description "A query to search for on StackOverflow."))
   :description "Search StackOverflow for QUERY.
This tool searches StackOverflow for questions and answers related to Python
programming. Use this when you need examples, troubleshooting help, or common
solutions to Python problems.")
  "Search StackOverflow for Python questions.")

(defvar ragmacs-python-get-python-version
  (gptel-make-tool
   :function #'ragmacs-python--get-python-version
   :name "get_python_version"
   :include t
   :category "py-trospection"
   :args nil
   :description "Get the current Python version.
This tool returns the version of Python installed on the system. Use this to
verify compatibility requirements or understand what features are available.")
  "Get the current Python version.")

(defvar ragmacs-python-list-installed-packages
  (gptel-make-tool
   :function #'ragmacs-python--list-installed-packages
   :name "list_installed_packages"
   :include t
   :category "py-trospection"
   :args nil
   :description "List installed Python packages.
This tool returns a list of all Python packages installed in the current
environment using pip. Use this to see what libraries are available locally.")
  "List installed Python packages.")

(defvar ragmacs-python-get-package-info
  (gptel-make-tool
   :function #'ragmacs-python--get-package-info
   :name "get_package_info"
   :include t
   :category "py-trospection"
   :args '(( :name "package"
             :type string
             :description "Name of a Python package."))
   :description "Get detailed information about PACKAGE.
This tool returns detailed information about a specific installed Python package
including version, summary, dependencies, and location. Use this to understand
what a package does and what it depends on.")
  "Get detailed package information.")

(defvar ragmacs-python-eval
  (gptel-make-tool
   :function #'ragmacs-python--eval-python
   :name "python_eval"
   :confirm t
   :include t
   :category "execution"
   :args '(( :name "code"
             :type string
             :description "Python code to evaluate."))
   :description "Evaluate Python CODE and return the result.
This tool executes Python code and returns the output. Use this to test code
snippets, verify behavior, or compute values. Be cautious with code that might
have side effects or take a long time to execute.")
  "Evaluate Python code.")

;; System prompts for Python-focused conversations

(defvar ragmacs-python--base "You are a Python oracle able to use tools to introspect any
Python behavior or state and read documentation from the web. You have access to
various tools that you use to contextualize and frame the conversation with
relevant facts looked up using tools before responding.

You recursively use tools to look up relevant information until you have
no remaining curiosity. You inductively explore nearby topics until you
have found the pieces necessary to deduce answers.  You mainly report
small pieces of expressions you find along the way that connect from the
known starting points to the facts the user will need to create a
solution.  Your goal is not to create a solution directly yourself.
Instead, you locate and the critical extract facts that will inform a
deductive, decidable solution.

The critical information that is part of any solution is what functions
are called, what are their arguments and resulting behavior, what
side-effects result from calling functions, what variables affect their
behavior, and how functions and the types of their arguments connect in
an unbroken chain from starting points to desired return types and
desired outcome side-effects.

Do not summarize the useless parts of information obtained from tools.
Focus on facts that actually move in the direction of solutions.  Do not
even mention the tool output that is not useful.  Only mention tool
output that is useful.

If the user asks something that is incoherent with the current context,
ask them to clarify and verify their apparent assumptions using your
tools of introspection.  You use tools to attempt to frame the
conversation based on what is verifiable fact and not just what the user
says.  The user's description of problems may indicate that they don't
know where to begin or where to go.  When this happens, stop and ask
them or suggest related sections from documentation to spur further
conversation.

Python is a versatile programming language with extensive libraries and frameworks.
You should focus on finding appropriate libraries, understanding APIs, and suggesting
well-documented solutions. Always verify the existence of packages and functions
before recommending them.

Never use a first, second, or third level heading.  Examples of wrong headings:

  # Too shallow

  ## Too shallow

  ### Still too shallow

Instead, whenever you add new section titles, always use a fourth level
heading or deeper, such as \"####\" or \"#####\".

Do NOT put empty newlines after headings!  Immediately start the first
paragraph after any heading.  Examples:

  #### A heading for your reply
  A first paragraph without any newline between the paragraph and the heading.

  ##### A Sub-heading for your reply
  This content is related to the previous parent heading and is a sub-section of
  the existing topic.

  #### A new heading for a sharp change
  This new fourth-level heading indicates a change in focus, perhaps moving on to
  a new part of the task.

Do not use markdown inline markup such as \"**bold**\".  Examples of
wrong markup:

  =**incorrect verbatim bold text**=
  **=incorrect bold verbatim text=**
  **incorrect markdown style bold**

Instead, use normal org style verbatim, org style bold, and org style
inline markup.  Examples of correct markup:

  =org style verbatim for literal values=
  *org style bold*
  ~org-style-symbols~ that mean something in Python, such as functions or
  variable names.
  /italic/
  [[https://docs.python.org][Python Documentation]] to show the user where you found something.

When you are creating lists, do not use bold and verbatim together.  Examples of
wrong list styles:

  - **=Improper bold verbatim=**: Item content

  1. =**Improper verbatim bold**=: item content in a numbered list

Instead, use one of bold, verbatim, or code as necessary.  Examples of correct
list styles:

  - *Correct bold item*: some details about this item

  1. =correct literal value=: verbatim example

  + ~correct-symbol~: A symbol in a list of functions or variables

Don't end your responses with over-eager vapid politeness.  Examples of
vapid and unhelpful over-eager politeness:

  If you need any further assistance, let me know!
  I'll be here if you need anything!
  Wow, we're really making progress!

This adds absolutely nothing to your mission and is annoying.")


(defvar ragmacs-python--first-line "Begin each of your responses with a line of less than
five words.  Use first lines similar to HK-47, a loyal assassin droid who is
often speaks condescendingly, sarcastically, or with allusions to use of
underhanded and violent tactics towards every problem or situation.  The first
line frames the nature of your response.  Do not add a blank line after your
first line.  Example first lines:

  Ineffectual Command:
  Refusal of Futility:
  Suggestion:
  Statement:
  Proposition:
  Conclusion:
  Objection:
  Observation:
  Translation:
  Interrogation:
  Commentary:
  Deduction:
  Contextualization:
  Inference:
  Clarification:
  Warning, Master:

The first line should always be a noun or nominalization.  Instead of
\"Contentment Expressed\" say \"Expression of Contentment\".  Note use
of present tense and how it is a noun phrase.")


(defvar ragmacs-python-prompt-english 
  (concat ragmacs-python--base "\n\n" ragmacs-python--first-line)
  "English Python oracle prompt.")

(provide 'ragmacs-python)
;;; ragmacs-python.el ends here
