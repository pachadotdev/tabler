library(tabler)

# Shared fixtures
aside_ul      <- sidebar_menu(menu_item("Side", tab_name = "side"))
aside_fixture <- aside(
  class = "navbar navbar-vertical",
  div(
    class = "container-fluid",
    div(class = "collapse navbar-collapse", id = "sidebar-menu", aside_ul)
  )
)
top_fixture        <- topbar(title = "TopTest")
brand_nav_fixture  <- navbar_menu(menu_item("Home", tab_name = "home"), brand = sidebar_brand(text = "Brand"))
body_fixture       <- body("BODY")

# utils ----

expect_equal(NULL %||% 5, 5)
expect_equal(3   %||% 5, 3)

expect_true(tabler:::validate_color("primary"))
expect_false(tabler:::validate_color("not-a-color"))

expect_true(tabler:::validate_size("sm"))
expect_false(tabler:::validate_size("xxl"))

expect_equal(tabler:::css_class("a", NULL, "b"), "a b")
expect_null(tabler:::css_class(NULL, ""))

expect_error(tabler:::validate_tab_name("bad.name"), pattern = "tab_name must not have a '.'")
expect_null(tabler:::validate_tab_name("goodname"))

# filter_theme_li removes li items that carry theme anchors
li_theme <- li(
  class = "nav-item mt-auto",
  list(
    a(href = "?theme=dark",  class = "hide-theme-dark",  "D"),
    a(href = "?theme=light", class = "hide-theme-light", "L")
  )
)
li_keep  <- li(class = "nav-item", a(href = "#", "Keep"))
nav_tag  <- ul(class = "navbar-nav", li_keep, li_theme)
cleaned  <- tabler:::filter_theme_li(nav_tag)
cleaned_str <- as.character(cleaned)
expect_true(grepl("Keep", cleaned_str))
expect_false(grepl("hide-theme-dark|hide-theme-light", cleaned_str))

# core ----

# icon class strings
i1 <- icon("home")
expect_true(grepl("ti ti-home", as.character(i1)))

i2 <- icon("alarm", library = "bootstrap", class = "foo")
s2 <- as.character(i2)
expect_true(grepl("bi bi-alarm", s2))
expect_true(grepl("foo", s2))

# card shorthand
c1 <- card("My Title", "Some body text")
s1 <- as.character(c1)
expect_true(grepl("My Title", s1))
expect_true(grepl("Some body text", s1))

c2 <- card(title = "Titled", footer = "F", status = "primary")
s2 <- as.character(c2)
expect_true(grepl("Titled", s2))
expect_true(grepl("F", s2))
expect_true(grepl("card-status-primary", s2))

# menu_dropdown
md <- menu_dropdown("Actions", items = list(c("A", "/a"), c("B", "/b"), a(href = "#", "X")))
s  <- as.character(md)
expect_true(grepl("dropdown-menu", s))
expect_true(grepl("/a", s))
expect_true(grepl("/b", s))
expect_true(grepl("X", s))

# menu_item
mi <- menu_item("Label", icon = "dog", href = "/dogs")
s  <- as.character(mi)
expect_true(grepl("Label", s))
expect_true(grepl("/dogs", s))
expect_true(grepl("icon|<!-- Download SVG icon", s))

# sidebar_menu / horizontal_menu active state and title
li1 <- menu_item("One", href = "/1")
li2 <- menu_item("Two", href = "/2")
sm  <- sidebar_menu(li1, li2, title = list(text = "App", img = "logo.png"))
s   <- as.character(sm)
expect_true(grepl("navbar-nav", s))
expect_true(grepl("logo.png", s))

hm <- horizontal_menu(li1, li2)
expect_true(grepl("navbar-nav", as.character(hm)))

# get_layout_attributes
expect_equal(tabler:::get_layout_attributes("boxed")$class, "layout-boxed")

# invalid layout errors
expect_error(page(layout = "nope"), pattern = "Invalid layout")

# boxed page includes layout class
p <- page(title = "X", layout = "boxed", theme = "dark", color = "blue",
           show_theme_button = FALSE)
expect_true(grepl("layout-boxed", as.character(p)))

# page() returns head + body tagList
ui <- page(title = "Combo Dashboard", layout = "combo", body = body("Welcome to Tabler!"))
expect_inherits(ui, "tabler.tag.list")
expect_inherits(ui[[1]], "tabler.tag")
expect_equal(ui[[1]]$name, "head")
expect_inherits(ui[[2]], "tabler.tag")
expect_equal(ui[[2]]$name, "body")
expect_true(grepl("Welcome to Tabler", as.character(ui[[2]])))

# RTL dir attribute
ui_rtl     <- page(layout = "rtl", body = body("rtl"))
ui_rtl_str <- as.character(ui_rtl)
expect_true(grepl('class="page"|class=\\"page\\"', ui_rtl_str))
expect_true(grepl('dir="rtl"|dir=\\"rtl\\"', ui_rtl_str))

# body / topbar / footer / sidebar
b <- body("content")
expect_inherits(b, "tabler.tag")
expect_true(grepl("page-body", b$attribs$class))

nav <- topbar(title = "Brand")
expect_inherits(nav, "tabler.tag")
expect_true(grepl("Brand", as.character(nav)))

img_nav <- topbar(brand_image = "logo.png", title = "ImgTitle")
expect_true(grepl("logo.png", as.character(img_nav)))
expect_true(grepl("navbar-brand-image", as.character(img_nav)))
expect_true(grepl("ImgTitle|Dashboard", as.character(img_nav)))

img_nav2 <- topbar(brand_image = "logo2.png")
expect_true(grepl('alt="Dashboard"|alt=\\"Dashboard\\"', as.character(img_nav2)))

ft <- footer(left = "L", right = "R")
expect_inherits(ft, "tabler.tag")
expect_true(grepl("L", as.character(ft)))
expect_true(grepl("R", as.character(ft)))

# sidebar active state
m1   <- menu_item("Dogs", tab_name = "dogs", icon = "dog", badge = "NEW")
m2   <- menu_item("Cats", tab_name = "cats")
sm2  <- sidebar_menu(m1, m2)
expect_true(grepl("active", as.character(sm2)))

# tab_items marks first child show active
t1   <- tab_item("dogs", "dog content")
t2   <- tab_item("cats", "cat content")
tabs <- tab_items(t1, t2)
expect_inherits(tabs, "tabler.tag")
expect_true(grepl("show active", as.character(tabs)))

# components ----

crd <- card("body", title = "T", footer = "F", status = "danger", class = "extra")
expect_inherits(crd, "tabler.tag")
expect_true(grepl("card-status-danger", as.character(crd)))
expect_true(grepl("extra", crd$attribs$class))

vb <- value_box("42", "Label", icon = "star", color = "success", width = 4)
expect_inherits(vb, "tabler.tag")
expect_true(grepl("col-4",    as.character(vb)))
expect_true(grepl("bg-success", as.character(vb)))
expect_true(grepl("star",     as.character(vb)))

ti <- icon("home")
expect_true(grepl("ti ti-home", as.character(ti)))
bi <- icon("alarm", library = "bootstrap")
expect_true(grepl("bi bi-alarm", as.character(bi)))
fe <- icon("x", library = "feather")
expect_true(grepl("fe fe-x", as.character(fe)))

al <- alert("msg", type = "warning", dismissible = TRUE, title = "Heads up")
expect_inherits(al, "tabler.tag")
expect_true(grepl("Heads up",  as.character(al)))
expect_true(grepl("btn-close", as.character(al)))

btn <- button("Go", color = "primary", size = "sm", outline = TRUE, icon = "search")
expect_inherits(btn, "tabler.tag")
expect_true(grepl("btn-sm",              as.character(btn)))
expect_true(grepl("btn-outline-primary", as.character(btn)))
expect_true(grepl("search",              as.character(btn)))

# menu_item text-only
mi2 <- menu_item("JustText")
expect_inherits(mi2, "tabler.tag")
expect_true(grepl("JustText", as.character(mi2)))

# card without title/footer
c3 <- card("only body")
expect_inherits(c3, "tabler.tag")
expect_true(grepl("only body", as.character(c3)))

b2 <- button("Hi")
expect_inherits(b2, "tabler.tag")
expect_true(grepl("btn", as.character(b2)))

# aside handling ----

aside_tag <- aside(
  class = "custom-aside navbar-vertical",
  ul(class = "navbar-nav pt-lg-3", li("item"))
)
p_aside   <- page(title = "Test", navbar = aside_tag, body = body("hi"), layout = "vertical")
html_str  <- as.character(p_aside)
expect_true(grepl("<aside", html_str))
expect_true(grepl("navbar-vertical", html_str))

# layout: boxed ----

bdy  <- body("body-content")
top  <- topbar(title = "TTop")
side <- sidebar_menu(menu_item("SI", tab_name = "si"))

combo_out <- tabler:::layout_boxed(list(top = top, side = side), NULL, bdy, NULL)
combo_str <- as.character(combo_out)
expect_true(grepl("page", combo_str))
expect_true(grepl("SI|navbar-vertical", combo_str))

boxed_nav <- navbar_menu(menu_item("Home", tab_name = "home"),
                          brand = sidebar_brand(text = "B", img = "i.png"))
boxed_out <- tabler:::layout_boxed(boxed_nav, NULL, bdy, NULL)
boxed_str <- as.character(boxed_out)
expect_true(grepl("page-wrapper|page-body", boxed_str))
expect_true(grepl("B|Home|navbar-brand-image", boxed_str))

# layout: combo ----

theme_a  <- a(href = "#", "T")
theme_li <- li(class = "mt-auto", theme_a)
ul_c     <- ul(theme_li, li(a(href = "#", "Item")))
aside_c  <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_c))
)
out_c  <- tabler:::layout_combo(list(top = NULL, side = aside_c), NULL, body("C"), NULL)
out_cs <- as.character(out_c)
expect_true(grepl('data-bs-theme="dark"', out_cs))
expect_true(grepl("Item", out_cs))

# combo: theme li filtered when show_theme_button = FALSE
theme_a2  <- a(href = "?theme=dark", class = "hide-theme-dark")
theme_li2 <- li(class = "mt-auto", theme_a2)
ul_c2     <- ul(li(a(href = "#", "LinkC")), theme_li2)
aside_c2  <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_c2))
)
out_c2 <- tabler:::layout_combo(list(top = NULL, side = aside_c2), NULL, body("Body"), NULL,
                         show_theme_button = FALSE)
s_c2 <- as.character(out_c2)
expect_true(grepl('data-bs-theme="dark"', s_c2))
expect_false(grepl("hide-theme-dark", s_c2))

# combo: navbar-expand-lg appended when missing
aside_noexp <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu",
          ul(li(a(href = "#", "B")))))
)
out_exp <- tabler:::layout_combo(list(top = NULL, side = aside_noexp), NULL, body("Body"), NULL)
s_exp   <- as.character(out_exp)
expect_true(grepl('data-bs-theme="dark"', s_exp))
expect_true(grepl("navbar-expand-lg", s_exp))

# combo: no duplicate navbar-expand-lg
aside_exp <- aside(
  class = "navbar navbar-vertical navbar-expand-lg",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu",
          ul(li(a(href = "#", "Dup")))))
)
out_dup <- tabler:::layout_combo(list(top = NULL, side = aside_exp), NULL, body("D"), NULL)
s_dup   <- as.character(out_dup)
m_dup   <- regmatches(s_dup, regexpr("<aside [^>]*>", s_dup))
expect_true(length(m_dup) == 1L)
expect_true(grepl("navbar-expand-lg", m_dup))
expect_false(grepl("navbar-expand-lg[[:space:]]+navbar-expand-lg", m_dup))

# layout: condensed ----

brand_div_cond <- div(class = "navbar-brand", img(src = "logo.png"))
theme_li_cond  <- li(class = "mt-auto", a(href = "#", class = "theme-toggle", "T"))
ul_cond <- ul(li(a(href = "#", "One")), theme_li_cond)
aside_cond <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid", brand_div_cond,
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_cond))
)
out_cond <- tabler:::layout_condensed(aside_cond, NULL, body("B"), NULL, show_theme_button = TRUE)
s_cond   <- as.character(out_cond)
expect_true(grepl("navbar-brand",   s_cond))
expect_true(grepl("theme-toggle",   s_cond))
expect_true(grepl("One",            s_cond))

# layout: edge-cases ----

aside_ec <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "navbar-brand navbar-brand-autodark", a(href = "#", "Tabler Unit Testing")),
      div(class = "collapse navbar-collapse", id = "sidebar-menu",
          sidebar_menu(menu_item("A", tab_name = "a"))))
)
out_ec  <- tabler:::layout_condensed(aside_ec, NULL, body("C"), NULL)
out_ecs <- as.character(out_ec)
expect_true(grepl("Tabler Unit Testing|navbar-brand", out_ecs))
expect_true(grepl("C", out_ecs))

# navbar_overlap adds text-white
hdr_ec  <- header("H", subtitle = "S")
nav_ec  <- navbar_menu(menu_item("N", tab_name = "n"), brand = sidebar_brand(text = "B"))
out_ov  <- tabler:::layout_navbar_overlap(nav_ec, NULL, list(hdr_ec), NULL)
expect_true(grepl("text-white|navbar-overlap", as.character(out_ov)))

# RTL brand inside ul
aside_rtl <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "navbar-brand navbar-brand-autodark", a(href = "#", "Brand")),
      div(class = "collapse navbar-collapse", id = "sidebar-menu",
          sidebar_menu(menu_item("Side", tab_name = "side"))))
)
out_rtl <- tabler:::layout_rtl(aside_rtl, NULL, body("R"), NULL)
s_rtl   <- as.character(out_rtl)
expect_true(grepl('dir="rtl"|dir=\\"rtl\\"', s_rtl))
expect_true(grepl("Brand", s_rtl))

# layout: fluid ----

a_dd     <- a(href = "#", `data-bs-toggle` = "dropdown", "Menu")
li_dd    <- li(class = "nav-item dropdown", a_dd)
ul_fl    <- ul(li_dd, li(class = "mt-auto", div(a(href = "#", class = "theme-toggle", "Theme"))))
aside_fl <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_fl))
)
out_fl  <- tabler:::layout_fluid(aside_fl, NULL, body("BODY"), NULL)
out_fls <- as.character(out_fl)
expect_true(grepl('aria-expanded="false"|data-bs-auto-close="outside"', out_fls))
expect_true(grepl("BODY", out_fls))

# layout: fluid-vertical ----

ul_fv    <- ul(li(class = "mt-auto", a(href = "#", class = "theme-toggle", "T")))
aside_fv <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_fv))
)
out_fv <- tabler:::layout_fluid_vertical(aside_fv, NULL, body("B"), NULL, show_theme_button = TRUE)
s_fv   <- as.character(out_fv)
expect_true(grepl('data-bs-theme="dark"', s_fv))
expect_true(grepl("navbar-expand-lg", s_fv))

# layout: horizontal ----

ul_hz    <- ul(li(class = "mt-auto", a(href = "#", class = "theme-toggle", "Tabler Unit Testing")))
aside_hz <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_hz))
)
out_hz_on  <- tabler:::layout_horizontal(aside_hz, NULL, body("H"), NULL, show_theme_button = TRUE)
out_hz_off <- tabler:::layout_horizontal(aside_hz, NULL, body("H"), NULL, show_theme_button = FALSE)
expect_true(grepl("Tabler Unit Testing",  as.character(out_hz_on)))
expect_false(grepl("Tabler Unit Testing", as.character(out_hz_off)))

# layout: navbar-dark ----

ul_nd    <- ul(li(class = "nav-item", a(href = "#", `data-bs-toggle` = "dropdown", "D")),
               li(class = "mt-auto", div(a(href = "#", class = "theme-toggle", "ThemeDark"))))
aside_nd <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_nd))
)
out_nd_on  <- tabler:::layout_navbar_dark(aside_nd, NULL, body("B"), NULL, show_theme_button = TRUE)
out_nd_off <- tabler:::layout_navbar_dark(aside_nd, NULL, body("B"), NULL, show_theme_button = FALSE)
expect_true(grepl('data-bs-theme="dark"',  as.character(out_nd_on)))
expect_true(grepl('aria-expanded="false"', as.character(out_nd_on)))
expect_false(grepl("ThemeDark",            as.character(out_nd_off)))

# layout: navbar-overlap ----

aside_ov <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "navbar-brand", "Brand"),
      div(class = "collapse navbar-collapse", id = "sidebar-menu",
          ul(li(a(href = "#", "OverlapItem")))))
)
bdy_ov  <- list(div(class = "page-header", "Head"), div(class = "page-body", "Content"))
out_ov2 <- tabler:::layout_navbar_overlap(aside_ov, NULL, bdy_ov, NULL, show_theme_button = FALSE)
s_ov2   <- as.character(out_ov2)
expect_true(grepl('data-bs-theme="dark"', s_ov2))
expect_true(grepl("text-white",           s_ov2))
expect_true(grepl("OverlapItem",          s_ov2))

# layout: navbar ----

sb <- navbar_menu(menu_item("One", tab_name = "one"),
                   brand = sidebar_brand(text = "MyApp", img = "logo.png"))
expect_true(grepl("navbar-vertical",              as.character(sb)))
expect_true(grepl("navbar-brand-image|logo.png",  as.character(sb)))

hd <- navbar_menu(menu_item("Two", tab_name = "two"), brand = NULL)
expect_true(grepl("Two", as.character(hd)))

# tabler:::layout_vertical with dropdown normalisation
a_vt   <- a(href = "#", `data-bs-toggle` = "dropdown")
li_vt  <- li(a_vt)
ul_vt  <- ul(class = "navbar-nav pt-lg-3", li_vt)
aside_vt <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_vt))
)
out_vt <- tabler:::layout_vertical(aside_vt, NULL, body("B"), NULL, show_theme_button = FALSE)
expect_true(grepl("page", as.character(out_vt)))

# layout: navbar-sticky ----

ul_ns    <- ul(li(class = "nav-item", a(href = "#", `data-bs-toggle` = "dropdown", "Drop")),
               li(class = "mt-auto", div(a(href = "#", class = "theme-toggle", "Theme"))))
aside_ns <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_ns))
)
out_ns <- tabler:::layout_navbar_sticky(aside_ns, NULL, body("BODY"), NULL)
s_ns   <- as.character(out_ns)
expect_true(grepl('aria-expanded="false"',       s_ns))
expect_true(grepl('data-bs-auto-close="outside"', s_ns))
expect_true(grepl("BODY", s_ns))

# layout: rtl ----

ul_rl    <- ul(li(a(href = "#", "ItemR")), li(class = "mt-auto", a(href = "#", "T")))
aside_rl <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "navbar-brand", img(src = "b.png")),
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_rl))
)
out_rl <- tabler:::layout_rtl(aside_rl, NULL, body("R"), NULL, show_theme_button = TRUE)
s_rl   <- as.character(out_rl)
expect_true(grepl('dir="rtl"',   s_rl))
expect_true(grepl("ItemR",       s_rl))
expect_true(grepl("navbar-brand", s_rl))

# layout: vertical-right ----

ul_vr    <- ul(li(class = "mt-auto", a(href = "#", class = "theme-toggle", "T")))
aside_vr <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_vr))
)
out_vr <- tabler:::layout_vertical_right(aside_vr, NULL, body("Z"), NULL, show_theme_button = FALSE)
s_vr   <- as.character(out_vr)
expect_true(grepl("navbar-end",          s_vr))
expect_true(grepl('data-bs-theme="dark"', s_vr))

# layout: vertical-transparent ----

ul_vt2    <- ul(li(class = "mt-auto", a(href = "#", class = "theme-toggle", "T")))
aside_vt2 <- aside(
  class = "navbar navbar-vertical",
  div(class = "container-fluid",
      div(class = "collapse navbar-collapse", id = "sidebar-menu", ul_vt2))
)
out_vt2 <- tabler:::layout_vertical_transparent(aside_vt2, NULL, body("B"), NULL, show_theme_button = FALSE)
s_vt2   <- as.character(out_vt2)
expect_true(grepl("navbar-transparent", s_vt2))
expect_true(grepl("navbar-expand-lg",   s_vt2))
