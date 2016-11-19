#![feature(box_syntax)]

extern crate syntex_syntax as syntax;
extern crate syntex_errors as errors;

use syntax::ast as syn_ast;
use syntax::codemap::{mk_sp, CodeMap, Span};
use syntax::parse::{self, ParseSess};
use syntax::print::pprust;
use errors::{Handler, DiagnosticBuilder};
use errors::emitter::{ColorConfig, EmitterWriter};

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use ast::*;

// TODO generation
//
// Syntax for grammar and layout
// Generate lowering by annotating Rust Ast (see rust_ast.rs).


macro_rules! u {
    ($x: expr) => ($x as usize)
}


fn lower_krate(ast: &mut Ast, krate: &syn_ast::Crate) -> u32 {
    let krate = Instance {
        template: CRATE,
        node: InstanceKind::Repeat(krate.module.items.iter().map(|i| lower_item_or_mod(ast, i)).collect()),
    };
    ast.record_instance(krate)
}

fn ikw(s: &str) -> InstanceKind {
    InstanceKind::Keyword(s.to_owned())
}

fn lower_item_or_mod(ast: &mut Ast, item: &syn_ast::Item) -> InstanceKind {
    let item = match item.node {
        syn_ast::ItemKind::Mod(ref m) => {
            let block = Instance {
                template: BLOCK,
                // TODO
                node: InstanceKind::Repeat(vec![]),
            };
            let block = ast.record_instance(block);
            let item = Instance {
                template: MOD,
                node: InstanceKind::Sequence(vec![ikw("mod"),
                                                  InstanceKind::Name(item.ident.to_string()),
                                                  InstanceKind::Option(Some(box InstanceKind::Nt(block)))]),
            };
            let item = ast.record_instance(item);
            return InstanceKind::Choice(0, box InstanceKind::Nt(item));
        }
        syn_ast::ItemKind::Fn(ref decl, _, _, _, _, ref block) => {
            let block = Instance {
                template: BLOCK,
                // TODO
                node: InstanceKind::Repeat(vec![]),
            };
            let block = ast.record_instance(block);
            Instance {
                template: ITEM,
                node: InstanceKind::Sequence(vec![lower_vis(&item.vis),
                                                  InstanceKind::Choice(1, box ikw("fn")),
                                                  InstanceKind::Name(item.ident.to_string()),
                                                  InstanceKind::Nt(block)]),
            }
        }
        syn_ast::ItemKind::Struct(ref vd, _) => {
            let block = Instance {
                template: BLOCK,
                node: InstanceKind::Repeat(vd.fields().iter().map(|sf| {
                    let field = InstanceKind::Sequence(vec![lower_vis(&sf.vis),
                                                // TODO handle tuple structs
                                                InstanceKind::Name(sf.ident.unwrap().to_string()),
                                                // TODO hack
                                                InstanceKind::Name(pprust::ty_to_string(&sf.ty))]);
                    let field = ast.record_instance(Instance { template: FIELD, node: field });
                    let block_item = InstanceKind::Choice(2, box InstanceKind::Nt(field));
                    let block_item = ast.record_instance(Instance { template: BLOCK_ITEM, node: block_item });
                    InstanceKind::Nt(block_item)
                }).collect()),
            };
            let block = ast.record_instance(block);
            Instance {
                template: ITEM,
                node: InstanceKind::Sequence(vec![lower_vis(&item.vis),
                                                  InstanceKind::Choice(0, box ikw("struct")),
                                                  InstanceKind::Name(item.ident.to_string()),
                                                  InstanceKind::Nt(block)]),
            }
        }
        _ => unimplemented!(),
    };

    let item = ast.record_instance(item);
    InstanceKind::Choice(1, box InstanceKind::Nt(item))
}

fn lower_vis(vis: &syn_ast::Visibility) -> InstanceKind {
    InstanceKind::Option(match *vis {
        syn_ast::Visibility::Public => Some(box ikw("pub")),
        syn_ast::Visibility::Inherited => None,
        _ => unimplemented!(),
    })
}

fn main() {
    // let f = File::open("test.rs").unwrap();
    // let mut input = String::new();
    // f.read_to_string(&mut input).unwrap();

    let codemap = Rc::new(CodeMap::new());
    let tty_handler =
        Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(codemap.clone()));
    let mut parse_session = ParseSess::with_span_handler(tty_handler, codemap.clone());
    let krate = parse::parse_crate_from_file(&Path::new("test.rs"), Vec::new(), &parse_session).unwrap();

    let mut ast = Ast::new(init_templates());
    let krate = lower_krate(&mut ast, &krate);
    ast.verify();

    let engine = layout::Engine::new(&ast, init_layouts());
    println!("{}", engine.render(&ast.instances[u!(krate)]));
}

// TODO macro this up
const CRATE: u32 = 0;
const MOD: u32 = 1;
const ITEM: u32 = 2;
const BLOCK: u32 = 3;
const BLOCK_ITEM: u32 = 4;
const FIELD: u32 = 5;
const STMT: u32 = 6;

fn init_templates() -> Vec<Template> {
    fn kw(s: &str) -> Template {
        Template::Keyword(s.to_owned())
    }

    use ast::Template::*;

    vec![
        Repeat(box Choice(vec![Nt(MOD), Nt(ITEM)])),
        Sequence(vec![kw("mod"), Name, Option(box Nt(BLOCK))]),
        // TODO more items
        Sequence(vec![Option(box kw("pub")), Choice(vec![kw("struct"), kw("fn")]), Name, Nt(BLOCK)]),
        Repeat(box Nt(BLOCK_ITEM)),
        // TODO variant, expr, etc.
        Choice(vec![Nt(ITEM), Nt(MOD), Nt(FIELD), Nt(STMT)]),
        // TODO second name should be type
        Sequence(vec![Option(box kw("pub")), Name, Name]),
        Choice(vec![])
    ]
}

fn init_layouts() -> Vec<layout::Layout> {
    use layout::*;
    use layout::LNode::*;
    use layout::TNode::*;

    vec![
        // crate
        l(Repeat(box l(Choice(vec![l(Nt), l(Nt)])), vec![Break(BreakKind::Line)])),
        // mod
        l(Sequence(vec![l_post(Keyword, vec![Break(BreakKind::Space)]),
                        l(Name),
                        l_e(Option(box l_pre(Nt, vec![Break(BreakKind::Space)])), vec![Literal(';')])])),
        // item
        l(Sequence(vec![l(Option(box l_post(Keyword, vec![Break(BreakKind::Space)]))),
                        l_post(Choice(vec![l(Keyword), l(Keyword)]), vec![Break(BreakKind::Space)]),
                        l(Name),
                        l_pp(Nt, vec![Break(BreakKind::Space)], vec![Break(BreakKind::Line)])])),
        // TODO tie together the break-or-lines - use Group + tie to field newlines
        //   also, whether we break or not affects whether we should have a terminating comma for the fields
        // block
        l_ppe(Repeat(box l(Nt), vec![Break(BreakKind::SpaceOrLine)]),
              vec![Literal('{'), Break(BreakKind::SpaceOrLine), PushIndent],
              vec![PopIndent, Break(BreakKind::SpaceOrLine), Literal('}')],
              vec![Literal('{'), Literal('}')]),
        // block item
        l(Choice(vec![l(Nt), l(Nt), l(Nt), l(Nt)])),
        // field
        l_post(Sequence(vec![l(Option(box l_post(Keyword, vec![Break(BreakKind::Space)]))),
                             l_post(Name, vec![Literal(':'), Break(BreakKind::Space)]),
                             l(Name)]),
               vec![Literal(',')]),
        // TODO
        // statement
        l(Choice(vec![]))
    ]
}

mod ast {
    pub struct Ast {
        templates: Vec<Template>,
        pub instances: Vec<Instance>,
    }

    impl Ast {
        pub fn new(templates: Vec<Template>) -> Ast {
            Ast {
                templates: templates,
                instances: vec![],
            }
        }

        pub fn record_instance(&mut self, i: Instance) -> u32 {
            self.instances.push(i);
            (self.instances.len() - 1) as u32
        }

        pub fn verify(&self) {
            for i in &self.instances {
                let template = &self.templates[u!(i.template)];
                verify_instance_kind(&i.node, template, self);
            }
        }
    }

    fn verify_instance_kind(i: &InstanceKind, template: &Template, ast: &Ast) {
        match (template, i) {
            (&Template::Repeat(ref t), &InstanceKind::Repeat(ref is)) => {
                for i in is {
                    verify_instance_kind(i, t, ast);
                }
            }
            (&Template::Option(ref t), &InstanceKind::Option(Some(ref i))) => {
                verify_instance_kind(i, t, ast);
            }
            (&Template::Option(ref t), &InstanceKind::Option(None)) => {}
            (&Template::Sequence(ref ts), &InstanceKind::Sequence(ref is)) => {
                assert_eq!(ts.len(), is.len());
                for (t, i) in ts.iter().zip(is.iter()) {
                    verify_instance_kind(i, t, ast);
                }
            }
            (&Template::Choice(ref ts), &InstanceKind::Choice(n, ref i)) => {
                verify_instance_kind(i, &ts[u!(n)], ast);
            }
            (&Template::Nt(id), &InstanceKind::Nt(iid)) => {
                assert_eq!(id, ast.instances[u!(iid)].template);
            }
            (&Template::Name, &InstanceKind::Name(_)) => {}
            (&Template::Keyword(ref s), &InstanceKind::Keyword(ref is)) => {
                assert_eq!(s, is);
            }
            _ => panic!("Mismatched template/instance kinds"),
        }
    }

    // Templates
    pub enum Template {
        // Combinators
        Repeat(Box<Template>),
        Option(Box<Template>),
        Sequence(Vec<Template>),
        Choice(Vec<Template>),

        // Non-terminal
        Nt(u32),

        // Terminals
        Name,
        // TODO could be enumerated
        Keyword(String),
    }

    #[derive(Debug)]
    pub struct Instance {
        pub template: u32,
        pub node: InstanceKind,
    }

    #[derive(Debug)]
    pub enum InstanceKind {
        Repeat(Vec<InstanceKind>),
        Option(Option<Box<InstanceKind>>),
        Sequence(Vec<InstanceKind>),
        Choice(u32, Box<InstanceKind>),
        Nt(u32),
        Name(String),
        // TODO could be optimised
        Keyword(String),
    }

    impl InstanceKind {
        pub fn is_empty(&self) -> bool {
            match *self {
                InstanceKind::Repeat(ref is) => is.is_empty(),
                InstanceKind::Option(None) => true,
                _ => false,
            }
        }
    }
}

mod layout {
    use ast::*;

    use std::cell::Cell;

    pub struct Engine<'ast> {
        layouts: Vec<Layout>,
        ast: &'ast Ast,
        block_indent: Cell<i32>,
    }

    impl<'ast> Engine<'ast> {
        pub fn new(ast: &'ast Ast, layouts: Vec<Layout>) -> Engine<'ast> {
            Engine {
                ast: ast,
                layouts: layouts,
                block_indent: Cell::new(0),
            }
        }

        // TODO should swallow spaces after whitespace
        pub fn render(&self, instance: &Instance) -> String {
            let layout = &self.layouts[u!(instance.template)];
            self.render_instance_kind(&instance.node, layout)
        }

        fn render_instance_kind(&self, instance: &InstanceKind, layout: &Layout) -> String {
            // println!("render: {:?}, {:?}", instance, layout);
            let mut result = String::new();

            if instance.is_empty() {
                self.render_tnodes(&mut result, &layout.empty);
                return result;
            }

            self.render_tnodes(&mut result, &layout.pre);
            let node = match (instance, &layout.node) {
                (&InstanceKind::Repeat(ref is), &LNode::Repeat(ref l, ref join)) => {
                    for (c, i) in is.iter().enumerate() {
                        if c > 0 {
                            self.render_tnodes(&mut result, join);
                        }
                        result.push_str(&self.render_instance_kind(i, l));
                    }
                }
                (&InstanceKind::Option(Some(ref i)), &LNode::Option(ref l)) => {
                    result.push_str(&self.render_instance_kind(i, l));
                }
                (&InstanceKind::Sequence(ref is), &LNode::Sequence(ref ls)) => {
                    for (i, l) in is.iter().zip(ls.iter()) {
                        result.push_str(&self.render_instance_kind(i, l));
                    }
                }
                (&InstanceKind::Choice(n, ref i), &LNode::Choice(ref ls)) => {
                    result.push_str(&self.render_instance_kind(i, &ls[u!(n)]));
                }
                (&InstanceKind::Nt(i), &LNode::Nt) => result.push_str(&self.render(&self.ast.instances[u!(i)])),
                (&InstanceKind::Name(ref s), &LNode::Name) => result.push_str(s),
                (&InstanceKind::Keyword(ref s), &LNode::Keyword) => result.push_str(s),
                _ => unreachable!(),
            };
            self.render_tnodes(&mut result, &layout.post);

            result
        }

        fn render_tnodes(&self, result: &mut String, nodes: &[TNode]) {
            for t in nodes {
                match *t {
                    TNode::Break(BreakKind::Line) => self.render_new_line(result),
                    TNode::Break(BreakKind::OrLine) => {},
                    TNode::Break(BreakKind::Space) | TNode::Break(BreakKind::SpaceOrLine) => result.push(' '),
                    TNode::Literal(c) => result.push(c),
                    TNode::PushIndent => self.block_indent.set(self.block_indent.get() + 4),
                    TNode::PopIndent => self.block_indent.set(self.block_indent.get() - 4),
                }
            }
        }

        fn render_new_line(&self, result: &mut String) {
            result.push('\n');
            // TODO optimise
            for _ in 0..self.block_indent.get() {
                result.push(' ');
            }
        }
    }

    pub fn l(l: LNode) -> Layout {
        Layout {
            node: l,
            pre: vec![],
            post: vec![],
            empty: vec![],
        }
    }

    pub fn l_e(l: LNode, empty: Vec<TNode>) -> Layout {
        Layout {
            node: l,
            pre: vec![],
            post: vec![],
            empty: empty,
        }
    }

    pub fn l_pre(l: LNode, pre: Vec<TNode>) -> Layout {
        Layout {
            node: l,
            pre: pre,
            post: vec![],
            empty: vec![],
        }
    }

    pub fn l_post(l: LNode, post: Vec<TNode>) -> Layout {
        Layout {
            node: l,
            pre: vec![],
            post: post,
            empty: vec![],
        }
    }

    pub fn l_pp(l: LNode, pre: Vec<TNode>, post: Vec<TNode>) -> Layout {
        Layout {
            node: l,
            pre: pre,
            post: post,
            empty: vec![],
        }
    }

    pub fn l_ppe(l: LNode, pre: Vec<TNode>, post: Vec<TNode>, empty: Vec<TNode>) -> Layout {
        Layout {
            node: l,
            pre: pre,
            post: post,
            empty: empty,
        }
    }

    #[derive(Debug)]
    pub struct Layout {
        pre: Vec<TNode>,
        post: Vec<TNode>,
        empty: Vec<TNode>,
        node: LNode,
    }

    #[derive(Debug)]
    pub enum LNode {
        // Combinators
        // TODO need to be able to specify different behaviour for first and last
        // TODO merge Repeat and Group?
        // Vec<TNode> is join
        Repeat(Box<Layout>, Vec<TNode>),
        Option(Box<Layout>),
        Sequence(Vec<Layout>),
        Choice(Vec<Layout>),

        // TODO tie together the break-or-lines - use Group + tie to field newlines
        //   also, whether we break or not affects whether we should have a terminating comma for the fields
        Group(Box<Layout>),

        // Non-terminal
        Nt,
        // Terminals
        Name,
        Keyword,
    }

    #[derive(Debug)]
    pub enum TNode {
        Break(BreakKind),
        Literal(char),
        PushIndent,
        PopIndent,
    }

    // TODO priority
    #[derive(Debug)]
    pub enum BreakKind {
        Line,
        OrLine,
        SpaceOrLine,
        Space,
    }
}
