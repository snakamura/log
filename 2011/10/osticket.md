# Use multi-byte subject in osTicket

[osTicket](http://osticket.com/) uses `iso-8859-1` to encode a subject on sending mail, it gets garbled if it uses characters outside `iso-8859-1`. To make it use `utf-8`, apply this patch.

    diff -ur osticket_1.6.0.orig/upload/include/class.email.php osticket_1.6.0/upload/include/class.email.php
    --- osticket_1.6.0.orig/upload/include/class.email.php       2010-02-25 20:15:48.000000000 +0000
    +++ osticket_1.6.0/upload/include/class.email.php   2011-10-26 07:15:52.000000000 +0000
    @@ -175,6 +175,7 @@
             $options=array('head_encoding' => 'quoted-printable',
                            'text_encoding' => 'quoted-printable',
                            'html_encoding' => 'base64',
    +                       'head_charset'  => 'utf-8',
                            'html_charset'  => 'utf-8',
                            'text_charset'  => 'utf-8');
             //encode the body
