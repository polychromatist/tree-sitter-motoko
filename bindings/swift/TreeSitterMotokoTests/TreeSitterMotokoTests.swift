import XCTest
import SwiftTreeSitter
import TreeSitterMotoko

final class TreeSitterMotokoTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_motoko())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Motoko grammar")
    }
}
