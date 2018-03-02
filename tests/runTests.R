require("cellCounter") || stop("unable to load Package:cellCounter")
require("RUnit") || stop("unable to load Package:RUnit")
BiocGenerics::testPackage("cellCounter")