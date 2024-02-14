;; comment

module "min" =
    version = "0.1.0"
    author = "noxabellus"
    license = "MIT"
    description = "A minimal language test"
    repository = "https://github.com/ribbon-lang/ribbonc.git"
    homepage = "https://ribbon-lang.github.io"
    sources = "src/"
    dependencies =
        "test-dep@0.1.0" as test


pub use file "./main.bb"..
