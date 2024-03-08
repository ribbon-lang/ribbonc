#!/usr/env/bin ribboni

use ../Foo/{ bar
           , baz as (0) ` qux `
           , box/..
           , qubert/.. hiding llama
           , foo/.. hiding {a, b}
           } as Foo
