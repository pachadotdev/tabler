# How to use tabler?

The [package website](https://pacha.dev/tabler) covers the basics, customization, and
advanced usage of the package.

Edit the UI, server, or modules in ./R and then rebuild the package. One option is to
use tinydev like:

```r
tinydev::pkg_document()
tinydev::pkg_load()
run_app()
```

When you are ready with the changes:

```r
tinydev:::pkg_install()
```

And you can run your dashboard with `mypkg::run_app()`.
