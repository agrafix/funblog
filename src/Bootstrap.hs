import Web.Blog

main =
    do cfg <- parseConfig "blog.cfg"
       runBlog cfg
