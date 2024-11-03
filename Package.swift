// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterMotoko",
    products: [
        .library(name: "TreeSitterMotoko", targets: ["TreeSitterMotoko"]),
    ],
    dependencies: [
        .package(url: "https://github.com/ChimeHQ/SwiftTreeSitter", from: "0.8.0"),
    ],
    targets: [
        .target(
            name: "TreeSitterMotoko",
            dependencies: [],
            path: ".",
            sources: [
                "src/parser.c",
                "src/scanner.c",
                // NOTE: if your language has an external scanner, add it here.
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [.headerSearchPath("src")]
        ),
        .testTarget(
            name: "TreeSitterMotokoTests",
            dependencies: [
                "SwiftTreeSitter",
                "TreeSitterMotoko",
            ],
            path: "bindings/swift/TreeSitterMotokoTests"
        )
    ],
    cLanguageStandard: .c11
)
