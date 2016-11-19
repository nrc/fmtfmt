#![Lowering]

#[instance(CRATE, module)]
pub struct Crate {
    pub module: Mod,
}

#[match(node, item)]
pub struct Item {
    pub ident: Ident,
    pub node: ItemKind,
    pub vis: Visibility,
}

pub enum ItemKind {
    #[instance(ITEM, item.vis, choice("fn"), name(item.ident), 5)]
    Fn(P<FnDecl>, Unsafety, Spanned<Constness>, Abi, Generics, P<Block>),
    #[instance(MOD, item.vis, "mod", name(item.ident), option(instance(BLOCK, 0)))]
    Mod(Mod),
    #[instance(ITEM, item.vis, choice("struct"), name(item.ident), 0)]
    Struct(VariantData, Generics),
    ..
}

// pub struct FnDecl {
//     pub inputs: Vec<Arg>,
//     pub output: FunctionRetTy,
// }

#[repeat(module.items)]
pub struct Mod {
    pub items: Vec<P<Item>>,
}

pub enum VariantData {
    #[instance(BLOCK, repeat(0))]
    Struct(Vec<StructField>, NodeId),
    ..
}

#[instance(BLOCK_ITEM, choice(instance(FIELD, sequence(vis, name(ident.unwrap()), ty))))]
pub struct StructField {
    pub ident: Option<Ident>,
    pub vis: Visibility,
    pub ty: P<Ty>,
}

#[option]
pub enum Visibility {
    #[Some("pub")]
    Public,
    #[None]
    Inherited,
    ..
}

#[instance(BLOCK, repeat(stmts))]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

// TODO stmts
