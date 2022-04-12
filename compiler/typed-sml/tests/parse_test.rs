use typed_sml::parse::parser;
use typed_sml::lex::Token;
use typed_sml::lex::LexerWrap;
use logos::Logos;

fn parse_input(input: &str){
    let lexer = LexerWrap{ lexer: Token::lexer(input) };
    assert!(parser::ExprParser::new().parse(lexer).is_ok());
}

#[test]
fn atoms() {
    parse_input("hehe");
}
