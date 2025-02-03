---
alerts_list:
  test:
    content: This is a critical alert.
    title: Important Notice
    type: tip
  test2:
    content: This is just an informational message.
    icon: false
    title: FYI
    type: info
  test3:
    collapse: true
    content: Just the tip.
    title: TIP
    type: tip
crossref:
  custom:
  - caption-location: top
    key: ale
    kind: float
    reference-prefix: Alert
title: Alert Example
toc-title: Table of contents
---

## Heading?

<div>

> **Important Notice**
>
> This is a critical alert.

</div>

[Alert 1](#ale-x){.quarto-xref}

::: {#tip-example}
> **Tip 1: Cross-Referencing a Tip**
>
> Add an ID starting with `#tip-` to reference a tip.
:::

[Tip 1](#tip-example){.quarto-xref}
