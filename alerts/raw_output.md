---
alerts:
  alert1:
    content: This is a critical alert.
    title: Important Notice
    type: warning
  alert2:
    content: A new version of the software is available.
    title: Update Available
    type: note
lua:
  filters:
  - path: shortcodes.lua
title: Standalone Report
toc-title: Table of contents
version: 1.2.1
---

# Quarto shortcode

Version 1.2.1 is a minor upgrade.

Standalone Report

x

Important Notice

y

<div>

> **Test**
>
> This is a test.

</div>

Here is an important alert:

{{< alert alerts.alert1 >}}

And here's another alert:

{{< alert alerts.alert2 >}}
{{< fa folder >}}
