use crate::system_f::lex::Token;
use annotate_snippets::{
    display_list::FormatOptions,
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

pub type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, usize>;

#[derive(Debug)]
pub struct TypeError {
    ///Byte offsets from source code
    pub title: &'static str,
    pub annot_type: AnnotationType,
    pub annotations: Vec<SourceAnnotation<'static>>,
}

impl<'a> Into<Snippet<'a>> for TypeError {
    fn into(self) -> Snippet<'a> {
        Snippet {
            title: Some(Annotation {
                id: None,
                label: Some(self.title),
                annotation_type: self.annot_type,
            }),
            footer: vec![],
            slices: vec![Slice {
                source: "igfeuawiuhhjabhvuihafhlehehuegh;ewaielafeiafh;eiwleefiheileglifaueaflgaerg\nbwgffgefwaegoyewfhafyewaufewf\nfruagfaefeawfuegwfewa\nwaeufgewgfeiefhgeyfegfWEFGO", // TODO
                line_start: 1, // TODO
                origin: None,
                annotations: self.annotations,
                fold: false
            }],
            opt: FormatOptions {
                color: true,
                anonymized_line_numbers: false,
                margin: None,
            },
        }
    }
}
