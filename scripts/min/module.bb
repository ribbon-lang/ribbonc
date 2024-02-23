;; comment

module "min" @ 0.1.0
    author "noxabellus"
    license "MIT"
    description "A minimal language test"
    repository "https://github.com/ribbon-lang/ribbonc.git"
    homepage "https://ribbon-lang.github.io"
    sources "src/", "src/x.bb", "foo/"
    dependencies
        "core" @ 0.1.0 as core
