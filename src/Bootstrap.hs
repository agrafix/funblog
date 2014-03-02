import Web.Blog

main =
    do (connStr, port) <- parseConfig "blog.ini"
       runBlog connStr port
