# CPP with multi-line string literals

When you enable CPP language extension and compile a file that contains multi-line string literals, you'll find that it fails to compile.

    {-# LANGUAGE CPP #-}

    test = "abc \
              \def"

This is because cpp expands backslashes at the end of lines. To workaround this problem, you can use cpphs instead.

    {-# LANGUAGE CPP #-}
    {-# OPTIONS_GHC -pgmP cpphs -optP --cpp #-}

    test = "abc \
              \def"

You can specify an alternative preprocessor with `-pgmP` and options to the preprocessor with `-optP`. You need to use `--cpp` to tell cpphs to process command line options for cpp.
