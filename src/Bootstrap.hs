import Web.Blog

main =
    do cfg <- parseConfig "blog.ini"
       runBlog cfg
